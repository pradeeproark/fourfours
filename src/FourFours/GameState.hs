{-# Language DeriveDataTypeable #-}
module FourFours.GameState  where
import FourFours.CommandParser
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
					   } deriving (Eq, Show, Typeable, Data)

data Player = Player
					{ playerName::ByteString					
					} deriving (Eq, Show, Ord, Typeable, Data)


data FourFoursState = FourFoursState
 				 {
 				       challengeFrom ::Int
 				       ,challengeTo ::Int
 					   ,allplayers:: [Player]
 					  ,challenges::Map.Map Player Problem
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

--resp = Response (fromString "Gee") "Hello" [((fromString "Gee"),0)] [(1,(fromString "Gah"),"44/44")]

getScoreTbl :: Map.Map Player Problem -> [(ByteString,Int)]
getScoreTbl mpp = map  (\(player,problem) -> (playerName player, 0)) (Map.assocs mpp)


getSubmissionsTbl :: Map.Map Player Problem -> [(Int,ByteString,String)]
getSubmissionsTbl mpp = map  (\(player,problem) -> ((number problem),(playerName player), (solution problem))) (Map.assocs mpp)

toResponse :: Player -> String -> FourFoursState -> Response
toResponse player msg gs = Response (playerName player) msg (getScoreTbl (challenges gs)) (getSubmissionsTbl (challenges gs))

responseToJSON :: Response -> String
responseToJSON resp = encode $ toJSON resp

systemUser = Player (fromString "<system>")

--prob1 = Problem 1 ""
--prob2 = Problem 2 ""
--prob3 = Problem 3 ""
--
--player1 = Player (fromString "Gee")
--player2 = Player (fromString "Gah")
--player3 = Player (fromString "Goo")

--echallenges = Map.fromList ([(player1,prob1), (player2,prob1)])

initialState = FourFoursState 0 50 [] (Map.fromList [])

--picky = (player3,prob2)
--
--nProb1 = Problem 1 "44/44"

--nchallenges = Map.insert player1 prob3 echallenges

--isProblemSolved :: Problem -> Map.Map Player Problem -> Bool
--isProblemSolved problem mpp =  Map.size(Map.filter (\prob -> prob == problem && solved prob) mpp) > 0
--
--isPlayerFree :: Player -> Map.Map Player Problem -> Bool
--isPlayerFree player mpp = not (Map.member player mpp)
--
--pick :: Player -> Problem -> Map.Map Player Problem -> Map.Map Player Problem
--pick player problem mpp
--			| isPlayerFree player mpp && not (isProblemSolved problem mpp) = Map.insert player problem mpp
--			| otherwise = mpp
--
--pickOnGS :: FourFoursState -> Player -> Problem -> FourFoursState
--pickOnGS gs player problem = FourFoursState  (allplayers gs) (problems gs) (pick player problem (challenges gs))
--			

updateChallenges :: FourFoursState -> Map.Map Player Problem -> FourFoursState
updateChallenges gs mpp = FourFoursState (challengeFrom gs) (challengeTo gs) (allplayers gs) mpp

solvedProblems :: Map.Map Player Problem -> [Problem]
solvedProblems mpp = Map.elems mpp
						
isValidAnswer :: Problem -> String -> Bool
isValidAnswer prob solution = True


isValidProblem :: FourFoursState -> Problem -> Bool
isValidProblem gs prob
            | (number prob >= challengeFrom gs && number prob <= challengeTo gs) = True
            | otherwise = False

submitAnswer :: FourFoursState -> Player -> Problem -> String -> FourFoursState
submitAnswer gs player prob solution
                    | isValidProblem gs prob && not (prob `elem` (Map.elems (challenges gs))) && isValidAnswer prob solution = updateChallenges gs (Map.insert player prob (challenges gs))
                    | otherwise = gs  						


updatePlayer :: FourFoursState -> Player -> FourFoursState
updatePlayer gs player
            | not (player `elem` allplayers gs) = FourFoursState (challengeFrom gs) (challengeTo gs) (player:(allplayers gs)) (challenges gs)
            | otherwise = gs
						
process :: FourFoursState  -> Command -> (ByteString, FourFoursState)
process gs (Enter player) = let ngs = updatePlayer gs player in (fromString "Entered player", ngs)
process gs (SubmitSolution player prob answer) = let ngs = (submitAnswer gs player prob answer) in (fromString(responseToJSON(toResponse player "submitted" ngs)),ngs)
--process gs (Pick player problem) = (Map.fromList ([(player,fromString "picked")]), pickOnGS gs player problem)
--process gs (FetchSolved solved) = (Map.fromList ([]), pickOnGS gs player problem)
--process gs (Pick player problem) = (Map.fromList ([]), pickOnGS gs player problem)
process gs (SendMessage player msg) = (fromString(responseToJSON(toResponse player (toString msg) gs)), gs)
process gs _ = (fromString "", gs)						

--cmd = Pick player1 prob1



--mProb = Map.fromList ([(player1,prob1) , (player2,prob2)])

parseCommand :: String -> Player -> Command
parseCommand input player = case(parse submitParser "" input) of
                                Left err -> SendMessage player (fromString input)
                                Right prob -> SubmitSolution player prob (solution prob)
--parseCommand "show solved" player = FetchSolved True
--parseCommand "show unsolved" player = FetchSolved False
--parseCommand "score" player = Score player
--parseCommand "scores" player = Scores
--parseCommand msg player = SendMessage player (fromString msg)


messageToCommand :: Message -> Command
messageToCommand (Join usr handle) = Enter usr
messageToCommand (Leave usr handle) = Exit usr
messageToCommand (Message usr msg) = parseCommand (toString msg) usr

procMsg :: Message -> FourFoursState -> (ByteString, FourFoursState)
procMsg msg val = process val (messageToCommand msg)
