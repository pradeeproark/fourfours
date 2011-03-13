{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FourFours.Chat where
import System.IO (Handle, hClose)
import Network.WebSockets (shakeHands, getFrame, putFrame)
import Network
       (listenOn, PortID(PortNumber), accept, withSocketsDo)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Exception
import Prelude hiding (catch)
import qualified Data.Map as Map
import FourFours.GameState
 
chatMain :: FourFoursState -> IO ()
chatMain ffs
  = withSocketsDo $
      do initialGs <- newMVar ffs
         socket <- listenOn (PortNumber 12345)
         putStrLn "Listening on port 12345."
         channel <- newChan
         forkIO (dispatcher channel [] initialGs)
         forever $
           do (h, _, _) <- accept socket
              forkIO (listener channel h)
 
listener :: Chan Message -> Handle -> IO ()
listener channel h
  = do request <- shakeHands h
       case request of
           Left err -> print err
           Right _ -> do putFrame h (fromString "")
                         user' <- getFrame h
                         unless (B.null user') $
                           do let user = Player user'
                              writeChan channel (Join user h)
                              listenLoop user channel h
 
listenLoop :: Player -> Chan Message -> Handle -> IO ()
listenLoop user channel h
  = do msg <- getFrame h
       if B.null msg then writeChan channel (Leave user h) else
         do writeChan channel (Message user msg)
            listenLoop user channel h
 
dispatcher ::
           Chan Message -> [Handle] -> MVar FourFoursState -> IO ()
dispatcher channel handles mog
  = do msg <- readChan channel
       print msg
       gs <- takeMVar mog
       print gs
       let (umsgs, ngs) = procMsg msg gs
       putMVar mog ngs
       print ngs
       case msg of
           Join user handle -> do let handles' = (handle : handles)
                                  broadcast2 handles' systemUser umsgs
                                  dispatcher channel handles' mog
           Leave user handle -> do let handles' = filter (/= handle) handles
                                   broadcast2 handles' systemUser umsgs
                                   dispatcher channel handles' mog
           Message user message -> do broadcast2 handles user umsgs
                                      dispatcher channel handles mog
 
broadcast2 :: [Handle] -> Player -> ByteString -> IO ()
broadcast2 handles user msg
  = forM_ handles $
      \ h -> putFrame h msg `catch` (\ (e :: SomeException) -> return ())