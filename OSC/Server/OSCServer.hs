import qualified Control.Monad              as M (liftM, Monad, when)
import qualified Control.Concurrent         as C (forkFinally, killThread, MVar, newEmptyMVar, putMVar, takeMVar, myThreadId, ThreadId)
import qualified Network.Socket             as S hiding (send, sendTo, recv, recvFrom)
import qualified Data.ByteString            as B (ByteString, empty, null)
import qualified Network.Socket.ByteString  as S
import qualified Data.Word                  as W (Word16)
import qualified System.IO                  as SIO (getChar, isEOF, putStr, putStrLn)
import qualified System.Posix.Signals       as SIG (sigTERM, sigQUIT, sigSTOP, installHandler)

threadNotifier :: String -> IO ()
threadNotifier m =
    printThreadId >>= \id -> SIO.putStrLn $ id ++ ": " ++ m
    where
	printThreadId :: IO String
	printThreadId = M.liftM show C.myThreadId

onStdinEOF :: M.Monad m => (a -> m b) -> a -> m ()
onStdinEOF handler args = do
    SIO.putStrLn "Enter ^D (EOF) to quit."
    SIO.isEOF >>= \e -> M.when e (handler args)
    -- isEOF is blocking; do not use unless. isEOF always returns 1.

bindUDPSocket :: String -> IO S.Socket
bindUDPSocket port = S.withSocketsDo $ do
    ai <- S.getAddrInfo	(Just (S.defaultHints {S.addrFlags = [S.AI_PASSIVE]}))
			Nothing
			(Just port)
    let addr = head ai 
    sock <- S.socket	(S.addrFamily addr)
			S.Datagram
			S.defaultProtocol
    S.bind sock (S.addrAddress addr)
    threadNotifier $ "UDP Socket bound @ " ++ show (S.addrAddress addr) 
    return sock

oscMsgHandler :: S.Socket -> IO ()
oscMsgHandler sock = do
    msg <- S.recvFrom sock 4096
    threadNotifier "A wild PACKET appeared!"
    M.when (B.null $ fst msg) $ return ()
    oscMsgHandler sock

closeOSCListener :: S.Socket -> IO Int
closeOSCListener = flip S.send B.empty

oscListener :: M.Monad m => String -> (S.Socket -> m a) -> IO ()
oscListener port handler = do
    sock <- bindUDPSocket port
    mvar <- C.newEmptyMVar 
    let tid = C.forkFinally (handler sock)
			    (\_ -> C.putMVar mvar C.myThreadId)
    onStdinEOF closeOSCListener sock
    r <- C.takeMVar mvar
    S.sClose sock
    return ()
    
main :: IO ()
main = oscListener "9797" oscMsgHandler
