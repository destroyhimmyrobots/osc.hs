module Atom
	( Atomic
	, isTypeTag
	, isTypeTagStr
	, isAddrStr
	, typeOf
	, encode
	, OSCString
	, Blob
	, Time
	, decode ) where
import qualified Data.ByteString.Lazy	as L
import qualified Data.ByteString.Lazy.Char8 as C (append, length, pack, unpack)
import qualified Data.Binary.Put	as P (runPut, putWord32be, putWord64be)
import qualified Data.Binary.Get	as G (runGet, getWord32be, getWord64be)
import qualified Data.Binary.IEEE754	as F (getFloat32be, putFloat32be)
import qualified Data.Word		as W (Word8)

newtype OSCString   = OSCString String
    deriving (Show, Read, Ord, Eq)
newtype Blob	    = Blob String
    deriving (Show, Read, Ord, Eq)
newtype Time	    = Time Int deriving
    (Bounded, Show, Read, Ord, Eq)

c2w8 :: Char -> W.Word8
c2w8 = fromIntegral . fromEnum

isAddrStr :: String -> Bool
isAddrStr a = '/' == head a

isTypeTagStr :: String -> Bool
isTypeTagStr t = ',' == head t

isTypeTag :: Char -> Bool
isTypeTag = flip elem "ifsbhtdScrmTFNI[]"

isLength :: L.ByteString -> Bool
isLength w = 0 == mod (L.length w) 4

bytesLen :: L.ByteString -> Int
bytesLen  b = fromIntegral (C.length b) :: Int

class Atomic a where
    typeOf :: a -> Char
    encode :: a -> L.ByteString
    decode :: L.ByteString -> a
    length :: a -> Int

instance Atomic Int where
    typeOf _ = 'i'
    length _ = 4
    decode = (.) fromIntegral (G.runGet G.getWord32be)
    encode = (P.runPut . P.putWord32be) . fromIntegral

instance Atomic Time where
    typeOf _ = 't'
    length _ = 8
    decode w = Time $ fromIntegral (G.runGet G.getWord64be w)
    encode (Time w) = (.) (P.runPut . P.putWord64be) fromIntegral w

instance Atomic Float where
    typeOf _ = 'f'
    length _ = 4
    decode = G.runGet F.getFloat32be
    encode = P.runPut . F.putFloat32be

instance Atomic OSCString where
    typeOf _ = 's'
    length = (.) bytesLen encode
    encode (OSCString s)
	| null s = L.empty
	| otherwise =
	    let b = C.pack s
		p = C.pack $ replicate ((4 - bytesLen b) `mod` 4) '\NUL'
	    in  C.append b  p
    decode m = OSCString $ C.unpack (sparse L.empty m 1)
	where sparse :: L.ByteString -> L.ByteString -> Int -> L.ByteString
	      sparse c x i
		| not isNul             = sparse s t (succ i)
		| isNul && 0 /= mod i 4 = sparse s t (succ i)
		| otherwise             = L.reverse s
		where	h = L.head x; s = L.cons h c; t = L.tail x
			isNul = h == c2w8 '\0'

instance Atomic Blob where
    typeOf _ = 'b'
    length = bytesLen . encode
    encode (Blob w) =
	let b = encode $ OSCString w
	    l = encode (bytesLen b)
	in L.append l b
    decode w =
	let l = decode (L.take 4 w) :: Int
	    (OSCString s) = decode (L.drop (fromIntegral l) w) :: OSCString
	in Blob s
