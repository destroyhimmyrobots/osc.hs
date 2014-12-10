module OSCAtom
	( OSCAtom
	, oscAtomContents
	, oscAtomType
	, oscAtomBytes
	, oscString
	, oscBlob
	, oscTimeTag
	, oscInt
	, oscFloat) where

-- For Serialization
import qualified Data.Binary as B (Binary, put, get)
-- For String
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C

-- For Int, TimeTag
import qualified Data.Word as W (Word32, Word64)
import qualified Data.Binary.Put as P (runPut, putWord32be, putWord64be)

-- For Float
import qualified Data.Binary.IEEE754 as F (putFloat32be)

-- | Generic OSC Atomic Algebraic Data Type.
--
data OSCAtom a
	= OSCAtom	{ oscAtomContents	:: a
			, oscAtomType		:: Char
			, oscAtomBytes		:: L.ByteString }
instance Show (OSCAtom a) where
	show (OSCAtom _ t b) = t : "-OSCAtom " ++ show b ++ "\n"

instance B.Binary a => B.Binary (OSCAtom a) where
	put (OSCAtom _ _ b) = B.put b
	get = B.get
	
-- | Retrieve the length of an OSCAtom's ByteString member.
--
oscAtomByteLen :: OSCAtom a -> Int
oscAtomByteLen (OSCAtom _ _ b) = fromIntegral (C.length b) :: Int

-- | Contruct an ASCII OSC String from a Haskell String.
--
oscString :: String -> OSCAtom String
oscString s =
	let 	b = C.pack s
		l = fromIntegral (C.length b) :: Int
		p = C.pack $ ((4 - l) `mod` 4) `replicate` '\NUL'
		c = C.append b p
	in OSCAtom s 's' c

-- | Contruct an OSC Blob from a Haskell String.
--
oscBlob :: String -> OSCAtom (Int, String)
oscBlob b =
	let 	s = oscString b
		l = oscAtomByteLen s
	in OSCAtom (l, b) 'b' (oscAtomBytes s)

-- | Contruct an unsigned 32-bit OSC Integer from a Haskell Integral type.
--
oscInt :: Integral a => a -> OSCAtom W.Word32
oscInt i =
	let w = fromIntegral i :: W.Word32
	in OSCAtom w 'i' (P.runPut $ P.putWord32be w)

-- | Contruct an unsigned 64-bit OSC Time Tag from a Haskell Integral type.
--
oscTimeTag :: Integral a => a -> OSCAtom W.Word64
oscTimeTag t =
	let w = fromIntegral t :: W.Word64
	in OSCAtom w 't' (P.runPut $ P.putWord64be w)

-- | Contruct an IEEE-754 32-bit floating point number from a Haskell Float.
--
oscFloat :: Float -> OSCAtom Float
oscFloat f = OSCAtom f 'f' (P.runPut $ F.putFloat32be f)
