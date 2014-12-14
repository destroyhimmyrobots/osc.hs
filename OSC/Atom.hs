module OSC.Atom
	( OSCAtom
	, typeTag
	, oscBytes
	, oscInt
	, oscTimeTag
	, oscFloat
	, oscString
	, oscBlob
	, combine) where
-- For String
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
-- For Int, TimeTag
import qualified Data.Word as W ()
import qualified Data.Binary.Put as P (runPut, putWord32be, putWord64be)
-- For Float
import qualified Data.Binary.IEEE754 as F (putFloat32be)

data OSCAtom = OSCAtom
    { typeTag	:: Char
    , oscBytes	:: L.ByteString } deriving Show

toBytes	    :: (b -> L.ByteString)
	    -> (a -> b)
	    -> a
	    -> L.ByteString
toBytes serialize cast = serialize . cast

oscInt	    :: Integral a => a -> OSCAtom
oscTimeTag  :: Integral a => a -> OSCAtom
oscFloat    :: Float -> OSCAtom
oscString   :: String -> OSCAtom
oscBlob	    :: OSCAtom -> OSCAtom

oscInt w =
    let s = P.runPut . P.putWord32be
    in OSCAtom 'i' $ toBytes s fromIntegral w

oscTimeTag w =
    let s = P.runPut . P.putWord64be
    in OSCAtom 't' $ toBytes s fromIntegral w

oscFloat w =
    let s = P.runPut . F.putFloat32be
    in OSCAtom 'f' $ toBytes s id w

oscString s
    | null s = error "OSC Strings may not be null."
    | otherwise = OSCAtom 's' (encodeString s)

oscBlob oa@(OSCAtom _ a) =
    let l = bytesLen a
    in OSCAtom 'b' (append (oscInt l) oa)

encodeString :: String -> L.ByteString
encodeString s = 
	let 	b = C.pack s
		l = bytesLen b
		p = C.pack $ ((4 - l) `mod` 4) `replicate` '\NUL'
	in	C.append b p

append :: OSCAtom -> OSCAtom -> L.ByteString
append (OSCAtom _ b0) (OSCAtom _ b1) = C.append b0 b1

combine :: [OSCAtom] -> OSCAtom
combine [] = OSCAtom '\0' C.empty
combine (a:as) = OSCAtom 's' (append a (combine as))

bytesLen :: L.ByteString -> Int
bytesLen  b = fromIntegral (C.length b) :: Int
