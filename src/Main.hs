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

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr




--
--data BinaryOper = Plus | Minus | Mult | Div deriving (Show)
--
----data Term = Term Int deriving (Show)
--
--data Expr = Term Int | Expr Expr BinaryOper Expr  deriving (Show)
--
--
--t1 = Term 4
--t2 = Term 4
--
--oper1 =  Plus
--oper2 =  Minus
--
--expr1 = Expr t1 oper1 t2
--expr2 = Expr (Expr t1 oper2 t2) oper1 (t1)
--expr3 = Expr (Expr t1 oper2 t2) oper1 (Expr t1 oper2 t2)


--
--
--singleFour :: Parser Expr
--singleFour = do
--        {
--         char '4'
--         ;eof
--         ;return (Term (4))
--        }
--
--doubleFour :: Parser Expr
--doubleFour = do
--        {
--         char '4'
--         ;char '4'
--         ;eof
--         ;return (Term (44))
--        }
--singleFour' :: Parser String
--singleFour' = do
--        {
--         char '4'
--         ;eof
--         ;return ("4")
--        }
--
--doubleFour' :: Parser String
--doubleFour' = do
--        {
--         char '4'
--         ;char '4'
--         ;eof
--         ;return ("44")
--        }
--
--onlyFour = try doubleFour' <|> try singleFour'
--term :: Parser Expr
--term =  try doubleFour <|> try singleFour

--oper :: Parser BinaryOper
--oper = do
--         {
--            o <- try (oneOf "+,-,/,*")
--            ;   case o of
--                  '+'->   return Plus
--                  '-'->   return Minus
--                  '/'->   return Div
--                  '*'->   return Mult
--
--         }

--TODO: do factorial implementation
fac x = x

expr    :: Parser Int
expr    = buildExpressionParser table factor
        <?> "expression"

table   = [[op "*" (*) AssocLeft, op "/" div AssocLeft]
          ,[op "+" (+) AssocLeft, op "-" (-) AssocLeft]
          ,[postfix "!" (fac) ]
          ]
        where
          op s f assoc
                 = Infix (do{ string s; return f}) assoc

postfix  name fun  = Postfix (do{ string name; return fun })

factor  = do{ char '('
            ; x <- expr
            ; char ')'
            ; return x
            }
        <|> number
        <?> "simple expression"




number  :: Parser Int
number  = do{ ds <- try (count 2 (char '4'))  <|> try (count 1 (char '4'))
            ; optional eof
            ; return (read ds)
            }
        <?> "number"


countFours :: String -> Int
countFours  = length . filter (\x -> x == '4')


--
--expr :: Parser Expr
--expr = do
--        {
--            --try (return term)
--            t1 <- term
--            ;o <- oper
--            ;t2 <-term
--            ;eof
--            ;return (Expr t1 o t2)

--        }
--expr' :: Parser Expr
--expr' = do
--         {
--            expr <|> term
--         }
--run :: Parser Expr -> String -> String
--run p input
--        = case (parse p "" input) of
--            Left err ->  concat $ map messageString $ errorMessages err
--            Right x  ->  (show x)
-- Parse
-- Evaluate Function

-- (4+4) - (4-4)
-- 4-
-- 4+4&100

input = "(4+44)-(4/4)"
main =  do
        let count = countFours input
        if count == 4
            then parseTest expr input
            else print $ "4 4s not there. Found"; print count

