{-# LANGUAGE CPP #-}

#ifndef __OSC_SERVER_DEBUG__
#define __OSC_SERVER_DEBUG__
#endif

import qualified OSC.Message	    as M
import qualified OSC.UDPSocket	    as U
import qualified OSC.Atom	    as A
import qualified Control.Monad              as M (liftM, when)
import qualified Control.Concurrent         as C (myThreadId, ThreadId, threadDelay)
import qualified Data.ByteString	    as B
import qualified System.IO                  as SIO (isEOF, putStrLn)

import Data.List as L (elemIndex)

#ifdef __OSC_SERVER_DEBUG__
threadNotifier :: String -> IO ()
threadNotifier m =
    printThreadId >>= \id -> SIO.putStrLn $ id ++ ": " ++ m
    where
	printThreadId :: IO String
	printThreadId = M.liftM show C.myThreadId
#endif

-- |
-- On EOF, perform an IO computation.
onStdinEOF :: IO a -> IO ()
onStdinEOF action = do
    SIO.putStrLn "Enter ^D (EOF) to quit."
    SIO.isEOF >>= \e -> M.when e (do	action
					return ())
    return ()

-- |
-- Perform a computation on raw OSC bytes.
bytes2osc :: B.ByteString -> IO ()
bytes2osc b = do
#ifdef __OSC_SERVER_DEBUG__
    threadNotifier $ show b
    threadNotifier $ show (M.readOSC b)
#endif
    return ()

main :: IO ()
main = do
    hosc <- U.udpSocketWithHandler "9797" bytes2osc -- . oscDecode
    sosc <- U.udpSocket "12002"
    -- U.sendEmpty hosc seems to block the following from printing:

    let snd = M.oscMessage "/serialosc/list" [A.oscString "127.0.0.1", A.oscInt 9797]
    print snd
    M.sendOSC snd sosc

    C.threadDelay (round 5e6)
    return ()
