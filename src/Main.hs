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

import Chat
import Expr
import GameState

input = "(4+4)-(4/4)"

main =  do
        print $ countFours input
--        if count == 4
--            then parseTest expr input
--            else print $ "4 4s not there. Found"; print count

