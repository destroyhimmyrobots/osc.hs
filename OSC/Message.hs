module OSC.Message
    ( oscMessage
    , sendOSC ) where
import OSC.Atom as A
import OSC.UDPSocket as U
import Network.Socket as S (Socket, SockAddr)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as L (append, ByteString, cons, concat, toStrict)

data OSCMessage = OSCMessage
    { msgAddress    :: A.OSCAtom
    , msgTypeTag    :: A.OSCAtom
    , msgData	    :: [A.OSCAtom] } deriving Show

oscAddress :: String -> A.OSCAtom
oscAddress a
    | '/' == head a = A.oscString a
    | otherwise = error "OSC Addresses start with a '/'."

oscMessage :: String -> [A.OSCAtom] -> OSCMessage
oscMessage a d =
    let tt = A.oscString $ ',' : map A.typeTag d 
    in OSCMessage (oscAddress a) tt d

toBytes :: OSCMessage -> B.ByteString
toBytes (OSCMessage a t d) =
    let bat = L.concat (map oscBytes [a, t])
	bd  = L.concat (map oscBytes d)
    in L.toStrict (L.append bat bd)

sendOSC  :: OSCMessage -> (S.Socket, S.SockAddr) -> IO Int
sendOSC p = U.sendBytes (toBytes p)
