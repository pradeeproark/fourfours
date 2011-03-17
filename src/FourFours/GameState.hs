{-# LANGUAGE DeriveDataTypeable #-}
module FourFours.GameState where
import FourFours.Expr
import qualified Data.Map as Map
import Data.List (nub, group)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Error (messageString,errorMessages)
import System.IO (Handle)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString, toString)
import Data.Typeable
import Data.Data
import Text.JSON.Generic

data Message = Join Player Handle
             | Leave Player Handle
             | Message Player ByteString
             deriving Show

data Problem = Problem{number :: Int, solution :: String}
             deriving (Eq, Show, Typeable, Data, Ord)

data Player = Player{playerName :: ByteString}
            deriving (Eq, Show, Ord, Typeable, Data)

data FourFoursState = FourFoursState{challengeFrom :: Int,
                                     challengeTo :: Int, allplayers :: [Player],
                                     challenges :: Map.Map Problem Player}
                    deriving (Show, Typeable, Data)

data Command = FetchSolved Bool
             | SubmitSolution Player Problem String
             | Score Player
             | Scores
             | Enter Player
             | Exit Player
             | SendMessage Player ByteString
             deriving Show

data Response = Response{from :: ByteString, msg :: String,
                         scores :: [(ByteString, Int)],
                         submissions :: [(Int, ByteString, String)]}
              deriving (Show, Typeable, Data)

submitParser :: Parser Problem
submitParser
  = do _ <- string "submit"
       skipMany1 space
       num <- many1 digit
       skipMany1 space
       ans <- expr
       eof
       return (Problem (read num :: Int) (show ans))

getScoreTbl :: Map.Map Problem Player -> [(ByteString, Int)]
getScoreTbl mpp
  = let pNames = map playerName (Map.elems mpp) in
      zip (nub pNames) (map length (group pNames))

getSubmissionsTbl ::
                  Map.Map Problem Player -> [(Int, ByteString, String)]
getSubmissionsTbl mpp
  = map
      (\ (problem, player) ->
         (number problem, playerName player, solution problem))
      (Map.assocs mpp)

toResponse :: Player -> String -> FourFoursState -> Response
toResponse player rmsg gs
  = Response (playerName player) rmsg (getScoreTbl (challenges gs))
      (getSubmissionsTbl (challenges gs))

responseToJSON :: Response -> String
responseToJSON resp = encode $ toJSON resp

systemUser :: Player
systemUser = Player (fromString "<system>")

initialState :: FourFoursState
initialState = FourFoursState 0 50 [] (Map.fromList [])

updateChallenges ::
                 FourFoursState -> Map.Map Problem Player -> FourFoursState
updateChallenges gs
  = FourFoursState (challengeFrom gs) (challengeTo gs)
      (allplayers gs)

isValidProblem :: FourFoursState -> Problem -> Bool
isValidProblem gs prob
  | number prob >= challengeFrom gs && number prob <= challengeTo gs
    = True
  | otherwise = False

submitAnswer ::
             FourFoursState -> Player -> Problem -> String -> FourFoursState
submitAnswer gs player prob _
  | isValidProblem gs prob && notElem prob (Map.keys (challenges gs))
    = updateChallenges gs (Map.insert prob player (challenges gs))
  | otherwise = gs

updatePlayer :: FourFoursState -> Player -> FourFoursState
updatePlayer gs player
  | player `notElem` allplayers gs =
    FourFoursState (challengeFrom gs) (challengeTo gs)
      (player : allplayers gs)
      (challenges gs)
  | otherwise = gs

process ::
        FourFoursState -> Command -> (ByteString, FourFoursState)
process gs (Enter player)
  = let ngs = updatePlayer gs player in
      (fromString "Entered player", ngs)
process gs (SubmitSolution player prob answer)
  = let ngs = (submitAnswer gs player prob answer) in
      (fromString (responseToJSON (toResponse player "submitted" ngs)),
       ngs)
process gs (SendMessage player pmsg)
  = (fromString
       (responseToJSON (toResponse player (toString pmsg) gs)),
     gs)
process gs _ = (fromString "", gs)

parseCommand :: String -> Player -> Command
parseCommand input player
  = case parse submitParser "" input of
        Left err -> SendMessage player (fromString (foldl (++) "" (map messageString (errorMessages err))))
        Right prob |
                     countFours input == 4 && (number prob == read (solution prob)) ->
                     SubmitSolution player (Problem (number prob) input) input
                   | otherwise -> SendMessage player (fromString "Invalid answer")

messageToCommand :: Message -> Command
messageToCommand (Join usr _) = Enter usr
messageToCommand (Leave usr _) = Exit usr
messageToCommand (Message usr umsg)
  = parseCommand (toString umsg) usr

procMsg ::
        Message -> FourFoursState -> (ByteString, FourFoursState)
procMsg pmsg val = process val (messageToCommand pmsg)
