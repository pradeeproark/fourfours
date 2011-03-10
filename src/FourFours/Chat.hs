{-# Language OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language ScopedTypeVariables #-}
module Chat where

import System.IO (Handle, hClose)
import Network.WebSockets (shakeHands, getFrame, putFrame)
import Network (listenOn, PortID(PortNumber), accept, withSocketsDo)
import System.IO (Handle, hClose)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString) -- this is from utf8-string
import Control.Monad (forever)
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Exception
import Prelude hiding (catch)
import qualified Data.Map as Map
import GameState
newtype User = User { fromUser :: ByteString }
  deriving (Eq, Ord, Show)

data Message = Join User Handle
             | Leave User Handle
             | Message User ByteString
  deriving Show

--type GameState = Int
--
--a::GameState
--a = 5

t :: Command
t = FetchSolved True
--
cmd :: Command
cmd = FetchSolved True
--
procMsg :: User -> ByteString -> FourFoursState -> ([(User,ByteString)],FourFoursState)
procMsg user msg val = ([(user, msg)], process val cmd)
--
--gstate = FourFoursState [] [] (Map.fromList [])

--procMsg :: User -> ByteString -> GameState -> ([(User,ByteString)],GameState)
--procMsg user msg val = ([(user, fromString "Proced")],val)

--initialState = 5

systemUser :: User
systemUser = User { fromUser = fromString "<system>" }

chatMain :: IO ()
chatMain = withSocketsDo $ do
 initialGs <- newMVar initialState	
 socket <- listenOn (PortNumber 12345)
 putStrLn "Listening on port 12345."
 channel <- newChan
 forkIO (dispatcher channel [] initialGs)
 forever $ do
   (h, _, _) <- accept socket
   forkIO (listener channel h)

listener :: Chan Message -> Handle -> IO ()
listener channel h = do
 request <- shakeHands h
 case request of
   Left err -> print err
   Right  _ -> do
    putFrame h (fromString "")
    user' <- getFrame h
    unless (B.null user') $ do
      let user = User user'
      writeChan channel (Join user h)
      listenLoop user channel h

listenLoop :: User -> Chan Message -> Handle -> IO ()
listenLoop user channel h = do
    msg <- getFrame h
    if (B.null msg)
      then do
        writeChan channel (Leave user h)
      else do
        writeChan channel (Message user msg)
        listenLoop user channel h

dispatcher :: Chan Message -> [Handle] -> MVar FourFoursState -> IO ()
dispatcher channel handles mog = do
  msg <- readChan channel
  putStrLn (show msg)
  case msg of
    Join user handle  -> do
      let handles' = (handle:handles)
      broadcast handles' systemUser (fromUser user `B.append` fromString " joined :)")
      dispatcher channel handles' mog
    Leave user handle -> do
      let handles' = filter (/= handle) handles
      broadcast handles' systemUser (fromUser user `B.append` fromString " left :(")
      dispatcher channel handles' mog
    Message user message  -> do
      gs <- takeMVar mog
      print gs
      let (umsgs,ngs) = procMsg user message gs
      putMVar mog ngs
      forM_ umsgs $ \(usr,umsg) -> do broadcast handles usr umsg
      --broadcast handles user message
      dispatcher channel handles mog

broadcast :: [Handle] -> User -> ByteString -> IO ()
broadcast handles user msg = forM_ handles $ \h -> do
  (putFrame h $ B.concat [fromUser user, fromString ": ", msg]) `catch` (\(e :: SomeException) -> return ())
