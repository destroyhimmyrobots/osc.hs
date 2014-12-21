module OSC.Message
    ( oscMessage
    , readOSC
    , sendOSC ) where
import qualified OSC.Atom		as A
import qualified OSC.UDPSocket	    	as U
import qualified Data.Maybe	    	as M
import qualified Data.ByteString    	as B (ByteString)
import qualified Data.ByteString.Lazy	as L

data Message = Message
    { msgAddress    :: A.OSCString
    , msgTypeTag    :: A.OSCString
    , msgData	    :: L.ByteString } deriving Show

c2w8 :: Char -> W.Word8
c2w8 = fromIntegral . fromEnum

parseType :: L.ByteString -> L.ByteString -> [A.Atom] -- where atomizeable type =>
parseType t m
    | L.null t && L.null m = []
    | c2w8 ',' == h = parseType y m
    | c2w8 'f' == h = (A.decode m :: Float) : parseType y (L.drop 4 m)
    | c2w8 'i' == h = (A.decode m :: Int) : parseType y (L.drop 4 m)
    | c2w8 's' == h || c2w8 's' == h =
	let b = A.decode m :: A.OSCString
	    l = A.length b
	in  b : parseType y (L.drop l m)
    | c2w8 'b' == h || c2w8 's' == h =
	let b = A.decode m :: A.Blob
	    l = A.length b
	in  b : parseType y (L.drop l m)
    | otherwise = []
    where (h, y) = (L.head t, L.tail t)

readOSC :: B.ByteString -> Message
readOSC m = 
    let l = L.fromStrict m
	(a, r) = parseMsgString l
	(t, d) = parseMsgString r
    in  Message  (A.oscAtom 's' a) -- check if vaild.
		 (A.oscAtom 's' t)
		 (parseType  t  d)

oscMessage :: String -> [A.Atom] -> Message
oscMessage a d = Message (A.oscString a) (A.oscString $ ',' : map A.typeTag d) d
--     | isAddress a = Just $ 
--     | otherwise = Nothing

sendOSC :: Message -> U.UdpOscHandle -> IO Int
sendOSC p = U.sendBytes (toBytes p)
    where
	toBytes :: Message -> L.ByteString
	toBytes (Message a t d) =
	    let at = L.concat (map A.oscBytes [a, t])
		bd = L.concat (map A.oscBytes d)
	    in L.append at bd
