module OSCPacket (oscMessage, oscMessage2) where
import qualified OSCAtom as A
import qualified Data.ByteString.Lazy as L (ByteString, concat)
-- TEMP
import qualified Data.Word as W (Word32)

newtype OSCAddress = OSCAddress (A.OSCAtom String)
instance Show OSCAddress where show (OSCAddress r) = "OSCAddress " ++ show r
oscAddress :: String -> OSCAddress
oscAddress a
	| '/' == head a = OSCAddress $ A.oscString a
	| otherwise = error "OSC Addresses must begin with '/'."

newtype OSCTypeTag = OSCTypeTag (A.OSCAtom String)
instance Show OSCTypeTag where show (OSCTypeTag t) = "OSCTypeTag " ++ show t
oscTypeTag :: String -> OSCTypeTag
oscTypeTag [] = error "Empty OSC Type Tag."
oscTypeTag y@(a:as)
	| not $ all (`elem` "ifsbm") as = error "Invalid OSC Type Tag."
	| ',' /= a = error "OSC Type Tags must begin with ','."
	| otherwise = OSCTypeTag $ A.oscString y

data OSCMessage = OSCMessage
	{ oscMsgAddress	:: OSCAddress
	, oscMsgTypeTag	:: OSCTypeTag
	, oscMsgArgs	:: L.ByteString } deriving Show

oscMessage :: String -> [A.OSCAtom a] -> OSCMessage
oscMessage addr atoms =
	let 	d = oscAddress addr
		t = oscTypeTag $ ',' : map A.oscAtomType atoms
		r = L.concat $ map A.oscAtomBytes atoms
	in OSCMessage d t r

oscMessage2 :: String -> A.OSCAtom String -> A.OSCAtom W.Word32 -> L.ByteString
oscMessage2 addr atom0 atom1 =
	let 	d = A.oscString addr
		t0 = A.oscAtomType atom0
		t1 = A.oscAtomType atom1
		t = A.oscString $ ',' : [t0,  t1]
		r = L.concat [A.oscAtomBytes d, A.oscAtomBytes t, A.oscAtomBytes atom0, A.oscAtomBytes atom1]
	in r
