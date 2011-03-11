{-# Language DeriveDataTypeable #-}
module FourFours.GameState  where

import FourFours.Expr
import Data.Maybe
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.String
import System.IO (Handle)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString, toString)
import Data.Typeable
import Data.Data
import Text.JSON.Generic

type PlayerName = String
type Solved = Bool
type Score = Int


data Message = Join Player Handle
             | Leave Player Handle
             | Message Player ByteString
  deriving Show

data Problem = Problem { number:: Int
						,solution :: String
					   } deriving (Eq, Show, Typeable, Data, Ord)

data Player = Player
					{ playerName::ByteString					
					} deriving (Eq, Show, Ord, Typeable, Data)


data FourFoursState = FourFoursState
 				 {
 				       challengeFrom ::Int
 				       ,challengeTo ::Int
 					   ,allplayers:: [Player]
 					  ,challenges::Map.Map Problem Player
 				 }deriving (Show, Typeable, Data)


data Command = FetchSolved Bool
 			   | SubmitSolution Player Problem String -- (Parser Int)
 			   | Score Player
 			   | Scores
               | Enter Player
               | Exit Player
               | SendMessage Player ByteString deriving (Show)
 			

data Response = Response
                {
                  from::ByteString,
                  msg::String,
                  scores::[(ByteString,Int)],
                  submissions::[(Int,ByteString,String)]
                } deriving (Show, Typeable, Data)

submitParser :: Parser Problem
submitParser = do
               {
                 ;string "submit"
                 ;skipMany1 space
                 ;number <- many1 digit
                 ;skipMany1 space
                 ;ans <- expr
                 ;eof
                 ;return (Problem (read number::Int) (show ans))
               }

getScoreTbl :: Map.Map Problem Player -> [(ByteString,Int)]
getScoreTbl mpp = map  (\(problem, player) -> (playerName player, 0)) (Map.assocs mpp)


getSubmissionsTbl :: Map.Map Problem Player -> [(Int,ByteString,String)]
getSubmissionsTbl mpp = map  (\(problem, player) -> (number problem,playerName player,solution problem)) (Map.assocs mpp)

toResponse :: Player -> String -> FourFoursState -> Response
toResponse player msg gs = Response (playerName player) msg (getScoreTbl (challenges gs)) (getSubmissionsTbl (challenges gs))

responseToJSON :: Response -> String
responseToJSON resp = encode $ toJSON resp

systemUser = Player (fromString "<system>")

initialState = FourFoursState 0 50 [] (Map.fromList [])

		

updateChallenges :: FourFoursState -> Map.Map Problem Player-> FourFoursState
updateChallenges gs = FourFoursState (challengeFrom gs) (challengeTo gs) (allplayers gs)

						
isValidAnswer :: Problem -> String -> Bool
isValidAnswer prob solution = True


isValidProblem :: FourFoursState -> Problem -> Bool
isValidProblem gs prob
            | number prob >= challengeFrom gs && number prob <= challengeTo gs = True
            | otherwise = False

submitAnswer :: FourFoursState -> Player -> Problem -> String -> FourFoursState
submitAnswer gs player prob solution
                    | isValidProblem gs prob && notElem prob (Map.keys (challenges gs)) = updateChallenges gs (Map.insert prob player (challenges gs))
                    | otherwise = gs  						


updatePlayer :: FourFoursState -> Player -> FourFoursState
updatePlayer gs player
            | player `notElem` allplayers gs = FourFoursState (challengeFrom gs) (challengeTo gs) (player : allplayers gs) (challenges gs)
            | otherwise = gs
						
process :: FourFoursState  -> Command -> (ByteString, FourFoursState)
process gs (Enter player) = let ngs = updatePlayer gs player in (fromString "Entered player", ngs)
process gs (SubmitSolution player prob answer) = let ngs = (submitAnswer gs player prob answer) in (fromString(responseToJSON(toResponse player "submitted" ngs)),ngs)
process gs (SendMessage player msg) = (fromString(responseToJSON(toResponse player (toString msg) gs)), gs)
process gs _ = (fromString "", gs)						



parseCommand :: String -> Player -> Command
parseCommand input player = case parse submitParser "" input of
                                Left err -> SendMessage player (fromString input)
                                Right prob
                                    | countFours input == 4 && (number prob == read (solution prob)) -> SubmitSolution player (Problem (number prob) input) input
                                    | otherwise -> SendMessage player (fromString "Invalid answer")



messageToCommand :: Message -> Command
messageToCommand (Join usr handle) = Enter usr
messageToCommand (Leave usr handle) = Exit usr
messageToCommand (Message usr msg) = parseCommand (toString msg) usr

procMsg :: Message -> FourFoursState -> (ByteString, FourFoursState)
procMsg msg val = process val (messageToCommand msg)
