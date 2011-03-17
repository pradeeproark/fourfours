{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FourFours.Chat where
import System.IO (Handle)
import Network.WebSockets (shakeHands, getFrame, putFrame)
import Network
       (listenOn, PortID(PortNumber), accept, withSocketsDo)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Control.Concurrent
import Control.Monad
import Control.Exception (catch, SomeException)
import Prelude hiding (catch)
import FourFours.GameState

chatMain :: FourFoursState -> IO ()
chatMain ffs
  = withSocketsDo $
      do initialGs <- newMVar ffs
         socket <- listenOn (PortNumber 12345)
         putStrLn "Listening on port 12345."
         channel <- newChan
         _ <- forkIO (dispatcher channel [] initialGs)
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
  = do fmsg <- getFrame h
       if B.null fmsg then writeChan channel (Leave user h) else
         do writeChan channel (Message user fmsg)
            listenLoop user channel h

dispatcher ::
           Chan Message -> [Handle] -> MVar FourFoursState -> IO ()
dispatcher channel handles mog
  = do cmsg <- readChan channel
       print cmsg
       gs <- takeMVar mog
       print gs
       let (umsgs, ngs) = procMsg cmsg gs
       putMVar mog ngs
       print ngs
       case cmsg of
           Join _ handle -> do let handles' = (handle : handles)
                               broadcast2 handles' systemUser umsgs
                               dispatcher channel handles' mog
           Leave _ handle -> do let handles' = filter (/= handle) handles
                                broadcast2 handles' systemUser umsgs
                                dispatcher channel handles' mog
           Message user _ -> do broadcast2 handles user umsgs
                                dispatcher channel handles mog

broadcast2 :: [Handle] -> Player -> ByteString -> IO ()
broadcast2 handles _ bmsg
  = forM_ handles $
      \ h ->
        putFrame h bmsg `catch` (\ (_ :: SomeException) -> return ())
