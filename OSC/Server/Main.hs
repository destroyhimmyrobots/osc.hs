{-# LANGUAGE CPP #-}

#ifndef __OSC_SERVER_DEBUG__
#define __OSC_SERVER_DEBUG__
#endif

import qualified UDPSocket		    as U
import qualified Control.Monad              as M (liftM, when)
import qualified Control.Concurrent         as C (myThreadId, ThreadId, threadDelay)
import qualified Data.ByteString	    as B
import qualified System.IO                  as SIO (isEOF, putStrLn)

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
-- Perform a computation on raw OSC bytes.
bytes2osc :: B.ByteString -> IO ()
bytes2osc b = do
    threadNotifier $ show b
    return ()

main :: IO ()
main = do
    hosc <- U.udpSocketWithHandler "9797" bytes2osc -- . oscDecode
    -- U.sendEmpty hosc this seems to block the following from printing:
    U.sendString "OSC Socks Rock!" hosc
    U.sendEmpty hosc
    C.threadDelay (round 2e6)

    sosc <- U.udpSocket "12002"
    U.sendString "/serialosc/list 127.0.0.1 9797" sosc
    C.threadDelay (round 2e6)

    return ()
