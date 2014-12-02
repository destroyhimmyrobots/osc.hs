-- | 
-- OSC Atomic Types encoded as Lazy ByteStrings. 
-- Cf. <http://opensoundcontrol.org/spec-1_0>
module OSCAtom (OSCInt, OSCFloat, oscBlob, oscString) where
import qualified Data.ByteString.Lazy.Char8 as D.B.L.C8	(pack)
import qualified Data.ByteString.Lazy	as D.B.L
import qualified Data.Binary		as D.B		(Binary, get, put)
import qualified Data.Binary.IEEE754	as D.B.F	(getFloat32be, putFloat32be)
import qualified Data.Binary.Get	as D.B.G	(getWord32be, getRemainingLazyByteString)
import qualified Data.Binary.Put	as D.B.P	(putWord32be)
--import qualified Data.Text		as D.X		(Text, pack, unpack)
--import qualified Data.Text.Encoding	as D.X.E	(encodeUtf8)
import qualified Data.Word		as D.W		(Word8, Word32)

-- /---------------------------------------------------------------------------
-- |
-- Serialize Data.Word.Word32 as 32-bit Big-Endian OSC Integer.
--
-- >>> Data.Binary.encode (OSCInt 31)
-- "\NUL\NUL\NUL\US"
--
-- >>> (Data.Binary.decode . Data.Binary.encode) (OSCInt 31) :: OSCInt
-- OSCInt 31
newtype OSCInt = OSCInt D.W.Word32 deriving (Bounded, Eq, Ord, Read, Show)

instance D.B.Binary OSCInt where
	put (OSCInt w)	= D.B.put w
	get = do 	t <- D.B.G.getWord32be
			return (OSCInt t)

-- /---------------------------------------------------------------------------
-- |
-- Serialize Float as IEEE754 32-bit Big-Endian OSC Float.
--
-- >>> Data.Binary.encode (OSCFloat 179424691)
-- "M+\FS\219"
--
-- >>> (Data.Binary.decode . Data.Binary.encode) (OSCFloat 179424691.31) :: OSCFloat
-- OSCFloat 1.7942469e8
newtype OSCFloat = OSCFloat Float deriving (Eq, Ord, Read, Show)
instance D.B.Binary OSCFloat where
	put (OSCFloat f) = D.B.F.putFloat32be f
	get = do 	t <- D.B.F.getFloat32be
			return (OSCFloat t)

-- /---------------------------------------------------------------------------
data OSCString = OSCString D.B.L.ByteString deriving (Eq, Ord, Show)
-- |
-- Serialize String as [Data.ByteString.Lazy] OSC String.
--
-- >>> Data.Binary.encode(oscBlob "Open Sound Control")
-- "\NUL\NUL\NUL\DC2\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC2Open Sound Control"
--
-- >>> (Data.Binary.decode . Data.Binary.encode) (Data.Binary.encode(oscString "Open Sound Control")) :: OSCString
-- OSCString "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC2Open Sound Control"
oscString 	:: String -> OSCString
oscString s	| null s    = 	OSCString D.B.L.empty
		| otherwise = 	let b = filter ('\NUL' /=) s
				in OSCString (D.B.L.C8.pack b)
				--p = replicate (mod (4 - (D.B.L.length b) 4) '\NUL'

instance D.B.Binary OSCString where
	put (OSCString c) = D.B.put c
	get = 	do 	t <- D.B.get
			return (oscString t)

instance Read OSCString where
	readsPrec _ input	| null input = []
				| head input == '\NUL' = []
				| otherwise  = [(oscString input, "")]

-- /---------------------------------------------------------------------------
data OSCBlob = OSCBlob D.W.Word32 [D.W.Word8] deriving (Eq, Ord, Read, Show)
-- |
-- Serialize Blob as [Data.Word.Word8] OSC Blob.
-- __/Bug:/__ Decoding a serialized OSC Blob has length __0__.
--
-- >>> oscBlob "Open Sound Control"
-- OSCBlob 18 [79,112,101,110,32,83,111,117,110,100,32,67,111,110,116,114,111,108]
--
-- >>> Data.Binary.encode (oscBlob "Open Sound Control")
-- "\NUL\NUL\NUL\DC2\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC2Open Sound Control"
oscBlob		:: String -> OSCBlob
oscBlob b	| null b    = 	OSCBlob 0 (D.B.L.unpack D.B.L.empty)
		| otherwise = 	let w = D.B.L.unpack (D.B.L.C8.pack b)
				in OSCBlob (fromIntegral (length w) :: D.W.Word32) w

instance D.B.Binary OSCBlob where 
	put (OSCBlob l b) = do 	D.B.P.putWord32be l
				D.B.put b
	get = do 	l <- D.B.G.getWord32be
			b <- D.B.G.getRemainingLazyByteString
			return (OSCBlob l (D.B.L.unpack b))
