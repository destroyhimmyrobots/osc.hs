module OSC.UDPSocket( udpSocket
		    , udpSocketWithHandler
		    , sendBytes
		    , sendEmpty
		    , UdpOscHandle ) where
import qualified Control.Monad              as M (when)
import qualified Control.Concurrent         as C (forkIO, forkFinally)
import qualified Network.Socket             as S hiding (send, sendTo, recv, recvFrom)
import qualified Data.ByteString            as B (ByteString, empty, null)
import qualified Data.ByteString.Lazy	    as L (ByteString, toStrict)
import qualified Data.ByteString.Char8      as BC (pack)
import qualified Network.Socket.ByteString  as S

data UdpOscHandle = UdpOscHandle S.Socket S.SockAddr
-- |
-- Open, but do not bind, a UDP Socket on the
-- default interface on /port/.
udpSocket :: String -> IO UdpOscHandle
udpSocket port = S.withSocketsDo $ do
    ai <- S.getAddrInfo	(Just (S.defaultHints {S.addrFlags = [S.AI_PASSIVE]}))
			 Nothing
			(Just port)
    let addr  = head ai 
        saddr = S.addrAddress addr
    sock <- S.socket (S.addrFamily addr)
			S.Datagram
			S.defaultProtocol
    return $ UdpOscHandle sock saddr
-- |
-- Bind a UDP socket on the default interface.
bindUDPSocket :: String -> IO UdpOscHandle
bindUDPSocket port = S.withSocketsDo $ do
    uoh@(UdpOscHandle sock saddr) <- udpSocket port
    S.bind sock saddr
    return uoh
-- |
-- Threaded handler which executes blocking /recvFrom/
-- on /sock/, passing bytes to /action/.
closeOnEmptyRecv :: (B.ByteString -> IO ()) -> S.Socket -> IO ()
closeOnEmptyRecv action sock = do
    (msg, _) <- S.recvFrom sock 2048
    M.when (B.null msg) $ return ()
    C.forkIO $ action msg
    closeOnEmptyRecv action sock
-- |
-- |
-- General function executing ByteString actions
-- received on a UDP socket in a new thread.
udpSocketWithHandler :: String
		     -> (B.ByteString -> IO ())
		     -> IO UdpOscHandle
udpSocketWithHandler port action = do
    ush@(UdpOscHandle sock saddr) <- bindUDPSocket port
    C.forkFinally
	(closeOnEmptyRecv action sock)
	(\_ -> S.sClose sock)
    return ush
-- |
-- Send a Lazy ByteString to a socket.
sendBytes :: L.ByteString -> UdpOscHandle -> IO Int
sendBytes b (UdpOscHandle s a) = S.sendTo s (L.toStrict b) a

-- |
-- Send a null packet to a socket.
sendEmpty :: UdpOscHandle -> IO Int
sendEmpty (UdpOscHandle s a) = S.sendTo s B.empty a
