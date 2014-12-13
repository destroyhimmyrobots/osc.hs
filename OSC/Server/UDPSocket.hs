module UDPSocket    ( udpSocket
		    , bindUDPSocket
		    , udpSocketWithHandler
		    , sendBytes
		    , sendEmpty
		    , sendString) where
import qualified Control.Monad              as M (when)
import qualified Control.Concurrent         as C (forkIO, forkFinally)
import qualified Network.Socket             as S hiding (send, sendTo, recv, recvFrom)
import qualified Data.ByteString            as B (ByteString, empty, null)
-- | TMP
import qualified Data.ByteString.Char8      as BC (pack, unpack)
-- ^ TMP
import qualified Network.Socket.ByteString  as S

udpSocket :: String -> IO (S.Socket, S.SockAddr)
udpSocket port = S.withSocketsDo $ do
    ai <- S.getAddrInfo	(Just (S.defaultHints {S.addrFlags = [S.AI_PASSIVE]}))
			 Nothing
			(Just port)
    let addr  = head ai 
        saddr = S.addrAddress addr
    sock <- S.socket (S.addrFamily addr)
			S.Datagram
			S.defaultProtocol
    return (sock, saddr)

-- |
-- Bind a UDP socket on the default interface
bindUDPSocket :: String -> IO (S.Socket, S.SockAddr)
bindUDPSocket port = S.withSocketsDo $ do
    (sock, saddr) <- udpSocket port
    S.bind sock saddr
    return (sock, saddr)

-- |
-- Send a strict ByteString to a socket.
sendBytes :: B.ByteString -> (S.Socket, S.SockAddr) -> IO Int
sendBytes b (s, a) = S.sendTo s b a

-- |
-- Send a string to a socket.
sendString :: Show a => a -> (S.Socket, S.SockAddr) -> IO Int
sendString g (s, a) = S.sendTo s (BC.pack $ show g) a

-- |
-- Send a null packet to a socket.
sendEmpty :: (S.Socket, S.SockAddr) -> IO Int
sendEmpty (s, a) = S.sendTo s B.empty a

-- |
-- Threadable handler which executes blocking /recvFrom/
-- on /sock/, passing bytes to /action/.
closeOnEmptyRecv :: (B.ByteString -> IO ()) -> S.Socket -> IO ()
closeOnEmptyRecv action sock = do
    (msg, _) <- S.recvFrom sock 2048
    M.when (B.null msg) $ return ()
    C.forkIO $ action msg
    closeOnEmptyRecv action sock

-- |
-- General function executing ByteString actions
-- received on a UDP socket in a new thread.
udpSocketWithHandler :: String -> (B.ByteString -> IO ()) -> IO (S.Socket, S.SockAddr)
udpSocketWithHandler port action = do
    (sock, saddr) <- bindUDPSocket port
    C.forkFinally
	(closeOnEmptyRecv action sock)
	(\_ -> S.sClose sock)
    return (sock, saddr)
