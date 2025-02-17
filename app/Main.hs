module Main where
import Data.Char

data Operator = Add | Sub | Mul | Div
data Expr = Num Int
          | BinOp Operator Expr Expr


-- type Parser a = String -> Either String (a, String)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f x = Parser $ \input -> case runParser x input of
    Just (v, rest) -> Just (f v, rest)
    Nothing -> Nothing


(<|>) :: Parser a -> Parser a -> Parser a
a <|> b = Parser $ \input -> case runParser a input of
  Nothing -> runParser b input
  success -> success
  
  

parseDigit :: Parser Char
parseDigit = Parser go
  where
    go "" = Nothing
    go (c:cs)
      | isDigit c = Just (c, cs)
      | otherwise = Nothing


many :: Parser a -> Parser [a]
many p = Parser $ \input -> case runParser p input of
    Nothing -> Just ([], input)
    Just (x, rest) -> case runParser (many p) rest of
        Nothing -> Just ([x], rest)
        Just (xs, remaining) -> Just (x:xs, remaining)

some :: Parser a -> Parser [a]
some p = Parser $ \input -> case runParser p input of
    Nothing -> Nothing
    Just (x, rest) -> case runParser (many p) rest of
        Nothing -> Just ([x], rest)
        Just (xs, remaining) -> Just (x:xs, remaining)

parseInt :: Parser Int
parseInt = read <$> some parseDigit



parseOperator :: Parser Operator
parseOperator = Parser go
  where
    go "" = Nothing
    go (char : rest) = case char of
        ' ' -> go rest
        '+' -> Just (Add, rest)
        '-' -> Just (Sub, rest)
        '*' -> Just (Mul, rest)
        '/' -> Just (Div, rest)
        _   -> Nothing







main :: IO ()
main = putStrLn "Hello, Haskell!"
