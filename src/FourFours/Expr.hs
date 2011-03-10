-----------------------------------------------------------------------------
--
-- Module      :  Expr
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

module FourFours.Expr  where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr

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
        <|> fournumber
        <?> "simple expression"




fournumber  :: Parser Int
fournumber  = do{ ds <- try (count 2 (char '4'))  <|> try (count 1 (char '4'))
            ; optional eof
            ; return (read ds)
            }
        <?> "number"


countFours :: String -> Int
countFours  = length . filter (\x -> x == '4')




