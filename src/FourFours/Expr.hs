module FourFours.Expr where
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

expr :: Parser Int
expr = buildExpressionParser table factor <?> "expression"


table
  = [[postfix "!" factorial],
     [op "*" (*) AssocLeft, op "/" div AssocLeft],
     [op "+" (+) AssocLeft, op "-" (-) AssocLeft]]
  where op s f
          = Infix
              (do _ <- string s
                  return f)
        postfix name fun
          = Postfix
              (do _ <- string name
                  return fun)

factor :: Parser Int
factor
  = (do _ <- char '('
        x <- expr
        _ <- char ')'
        return x)
      <|> fournumber
      <?> "simple expression"

fournumber :: Parser Int
fournumber
  = (do ds <- try (count 2 (char '4')) <|> try (count 1 (char '4'))
        optional eof
        return (read ds))
      <?> "number"

countFours :: String -> Int
countFours = length . filter (== '4')
