-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (

main

) where

import FourFours.Chat
import FourFours.Expr
import FourFours.GameState
import FourFours.CommandParser

input = "(4+4)-(4/4)"

pickInput = "pick 2"

--main =  print $ parseCommand pickInput

main = chatMain initialState
