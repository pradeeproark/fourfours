
module GameState  where

import Data.Maybe
import qualified Data.Map as Map

type PlayerName = String
type Solved = Bool
type Score = Int


data Problem = Problem { number:: Int
						,solved :: Bool
					   } deriving (Eq, Show)

data Player = Player
					{ playerName::String
					  ,score::Int
					} deriving (Eq, Show, Ord)

-- data Challenge = Challenge
-- 					{ problem:: Problem
-- 					  ,players:: [Player]
-- 					} deriving (Show)
--
--
data FourFoursState = FourFoursState
 				 {
 					   allplayers:: [Player]
 					  ,problems::[Problem]
 					  ,challenges::Map.Map Player Problem
 				 }deriving (Show)


data Command = Pick Player Problem
 			   | FetchSolved Bool deriving (Show)

prob1 = Problem 1 False
prob2 = Problem 2 False
prob3 = Problem 3 True

player1 = Player "Gee" 0
player2 = Player "Gah" 0
player3 = Player "Goo" 0

echallenges = Map.fromList ([(player1,prob1), (player2,prob1)])

initialState = FourFoursState [player1, player2, player3] [prob1,prob2,prob3] (Map.fromList [])
picky = (player3,prob2)

nProb1 = Problem 1 True

nchallenges = Map.insert player1 prob3 echallenges

isProblemSolved :: Problem -> Map.Map Player Problem -> Bool
isProblemSolved problem mpp =  Map.size(Map.filter (\prob -> prob == problem && solved prob) mpp) > 0

isPlayerFree :: Player -> Map.Map Player Problem -> Bool
isPlayerFree player mpp = not (Map.member player mpp)

pick :: Player -> Problem -> Map.Map Player Problem -> Map.Map Player Problem
pick player problem mpp
			| isPlayerFree player mpp && not (isProblemSolved problem mpp) = Map.insert player problem mpp
			| otherwise = mpp

pickOnGS :: FourFoursState -> Player -> Problem -> FourFoursState
pickOnGS gs player problem = FourFoursState  (allplayers gs) (problems gs) (pick player problem (challenges gs))
			
solvedProblems :: Map.Map Player Problem -> [Problem]
solvedProblems mpp = Map.elems (Map.filter (\prob -> solved prob) mpp)
						
process :: FourFoursState -> Command -> FourFoursState
process gs (Pick player problem) = pickOnGS gs player problem
process gs _ = gs						
-- challenge1 = Challenge prob1 []
-- challenge2 = Challenge prob2 []
-- challenge3 = Challenge prob3 [player2, player3]
--
-- gstate = GameState [player1,player2,player3] [prob1, prob2, prob3] [challenge1, challenge2, challenge3]
-- show solved

mProb = Map.fromList ([(player1,prob1) , (player2,prob2)])

-- getProblemsWhich :: (Maybe Player -> Bool) -> GameState  -> [Problem]
-- getProblemsWhich cond gs =  map problem (filter (\c -> cond(solvedBy((problem c))) ) (challenges gs))
--
-- getChallengeWithProblem :: Problem -> [Challenge] -> Maybe Challenge
-- getChallengeWithProblem prb challenges = find (\c -> problem c == prb) challenges
--
-- canPick :: Player -> [Challenge] -> Bool
-- canPick player challenges = not (any (\c -> (elem player (players c))) challenges)
--
-- pickExistingChallenge :: Player -> Problem -> Challenge -> Challenge
-- pickExistingChallenge player problem challenge = Challenge problem (player:(players challenge))
--
-- pickNewChallenge :: Player -> Problem -> Challenge
-- pickNewChallenge player problem = Challenge problem [player]

--create a new challenge or become part of a challenge


--gstate = GameState [player1,player2,player3] [prob1, prob2, prob3] [challenge1, challenge2, challenge3]

-- isSolved :: Problem -> Challenge -> Bool
-- isSolved prb chlg =
--
--
-- isProblemSolved :: Problem -> [Challenge] -> Bool
-- isProblemSolved prb challenges = any (\c -> isSolved c prb) challenges
