{-# LANGUAGE CPP #-}

#ifndef __OSC_SERVER_DEBUG__
#define __OSC_SERVER_DEBUG__
#endif

import qualified Control.Monad              as M (liftM, Monad, when)
import qualified Control.Concurrent         as C (forkIO, forkFinally
						 , myThreadId, ThreadId, threadDelay
						 , MVar, newEmptyMVar, takeMVar, putMVar)
import qualified Network.Socket             as S hiding (send, sendTo, recv, recvFrom)
import qualified Data.ByteString            as B (ByteString, empty, null)
-- | TMP
import qualified Data.ByteString.Char8      as BC (pack, unpack)
-- ^ TMP
import qualified Network.Socket.ByteString  as S
import qualified Data.Word                  as W (Word16)
import qualified System.IO                  as SIO (isEOF, putStrLn)
-- import qualified System.Posix.Signals       as SIG (sigTERM, sigQUIT, sigSTOP, installHandler)

#ifdef __OSC_SERVER_DEBUG__
threadNotifier :: String -> IO ()
threadNotifier m =
    printThreadId >>= \id -> SIO.putStrLn $ id ++ ": " ++ m
    where
	printThreadId :: IO String
	printThreadId = M.liftM show C.myThreadId
#endif

-- |
-- On EOF, perform an IO computation.
onStdinEOF :: IO a -> IO ()
onStdinEOF action = do
    SIO.putStrLn "Enter ^D (EOF) to quit."
    SIO.isEOF >>= \e -> M.when e (do action; return ())
    return ()

-- |
-- Bind a UDP socket on the default interface
bindUDPSocket :: String -> IO (S.Socket, S.SockAddr)
bindUDPSocket port = S.withSocketsDo $ do
    ai <- S.getAddrInfo	(Just (S.defaultHints {S.addrFlags = [S.AI_PASSIVE]}))
			 Nothing
			(Just port)
    let addr  = head ai 
        saddr = S.addrAddress addr
    sock <- S.socket (S.addrFamily addr)
			S.Datagram
			S.defaultProtocol
    S.bind sock saddr
#ifdef __OSC_SERVER_DEBUG__
    threadNotifier $ "UDP Socket bound @ " ++ show (S.addrAddress addr) 
#endif
    return (sock, saddr)

-- |
-- Send a null packet to a socket.
emptySend :: (S.Socket, S.SockAddr) -> IO Int
emptySend (s, a) = do
#ifdef __OSC_SERVER_DEBUG__
    threadNotifier "Closing..."
#endif
    S.sendTo s (BC.pack "OSC SOCKS") a -- B.empty

-- |
-- Threadable handler which executes blocking /recvFrom/
-- on /sock/, passing bytes to /action/.
closeOnEmptyRecv :: (B.ByteString -> IO ()) -> S.Socket -> IO ()
closeOnEmptyRecv action sock = do
#ifdef __OSC_SERVER_DEBUG__
    threadNotifier "Receiving..."
#endif
    (msg, _) <- S.recvFrom sock 2048
#ifdef __OSC_SERVER_DEBUG__
    threadNotifier "A wild PACKET appeared!"
#endif
    M.when (B.null msg) $ do
#ifdef __OSC_SERVER_DEBUG__
	threadNotifier "Honoring a close request." 
#endif
	return ()
    C.forkIO $ action msg
    closeOnEmptyRecv action sock

-- |
-- General function executing ByteString actions
-- received on a UDP socket in a new thread.
udpSocketHandler :: String -> (B.ByteString -> IO ()) -> IO (S.Socket, S.SockAddr)
udpSocketHandler port action = do
    (sock, saddr) <- bindUDPSocket port
    C.forkFinally
	(closeOnEmptyRecv action sock)
	(\_ -> S.sClose sock)
    return (sock, saddr)

-- |
-- Specific invocation of udpSocketHandler for OSC packet actions.
oscListener :: (B.ByteString -> IO ()) -> IO ()
oscListener action = do
    (sock, saddr) <- udpSocketHandler "9797" action -- . oscDecode
    C.threadDelay (round 2e6)
    emptySend (sock, saddr)
    C.threadDelay (round 5e6)
    return ()

-- |
-- Perform a computation on raw OSC bytes.
bytes2osc :: B.ByteString -> IO ()
bytes2osc b = do
    threadNotifier $ show b
    return ()

main :: IO ()
main = oscListener bytes2osc
