{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Char

data Operator = Add | Sub | Mul | Div

instance Show Operator where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

data Expr
  = Num Int
  | BinOp Operator Expr Expr

instance Show Expr where
    show (Num n) = show n
    -- show (BinOp op left right) = "(" ++ show left ++ " " ++ show op ++ " " ++ show right ++ ")"
    show (BinOp op left right) = show op ++ " " ++ show left ++ " " ++ show right
    -- show (BinOp op left right) = "(" ++ show op ++ " " ++ show left ++ " " ++ show right ++ ")"
    -- show (BinOp op left right) = show left ++ " " ++ show right ++ " " ++ show op

showTree :: Expr -> String
showTree = dat "" True
    where
        dat prefix isLeft (Num n) = 
            prefix ++ (if isLeft then "└── " else "┌── ") ++ show n ++ "\n"
        dat prefix isLeft (BinOp op left right) =
            dat (prefix ++ (if isLeft then "│   " else "    ")) False right ++ 
            prefix ++ (if isLeft then "└── " else "┌── ") ++ show op ++ "\n" ++
            dat (prefix ++ (if isLeft then "    " else "│   ")) True left

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f x = Parser $ \input -> case runParser x input of
    Just (v, rest) -> Just (f v, rest)
    Nothing -> Nothing

instance Applicative Parser where
  -- pure :: a -> Parser a
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pure x = Parser $ \input -> Just (x, input)

  a <*> b = Parser $ \str -> do
    (f, str') <- runParser a str
    (v, str'') <- runParser b str'
    return (f v, str'')

instance Monad Parser where
  -- return :: a -> Parser a
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  x >>= f = Parser $ \str -> do
    (v, str') <- runParser x str
    runParser (f v) str'

class (Applicative f) => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]

instance Alternative Parser where
  empty = Parser $ const Nothing

  -- (<|>) :: Parser a -> Parser a -> Parser a
  a <|> b = Parser $ \input -> case runParser a input of
    Nothing -> runParser b input
    success -> success

  -- many :: Parser a -> Parser [a]
  many p = Parser $ \input -> case runParser p input of
    Nothing -> Just ([], input)
    Just (x, rest) -> case runParser (many p) rest of
      Nothing -> Just ([x], rest)
      Just (xs, remaining) -> Just (x : xs, remaining)

  -- some :: Parser a -> Parser [a]
  some p = Parser $ \input -> case runParser p input of
    Nothing -> Nothing
    Just (x, rest) -> case runParser (many p) rest of
      Nothing -> Just ([x], rest)
      Just (xs, remaining) -> Just (x : xs, remaining)

-- parseDigit :: Parser Char
-- parseDigit = Parser go
--   where
--     go "" = Nothing
--     go (c : cs)
--       | isDigit c = Just (c, cs)
--       | otherwise = Nothing

parseDigit :: Parser Char
parseDigit = Parser $ \case
  (c:cs) | isDigit c -> Just (c, cs)
  _ -> Nothing

parseInt :: Parser Int
parseInt = spaces *> (read <$> some parseDigit)

parseLetter :: Parser Char
parseLetter = Parser $ \case
  (c:cs) | isAlpha c -> Just (c, cs)
  _ -> Nothing

parseWord :: Parser String
parseWord = spaces *> some parseLetter



-- space :: Parser Char
-- space = Parser $ \case
--   (c:cs) | c == ' ' -> Just (c, cs)
--   _ -> Nothing

space :: Parser Char
space = Parser $ \case
  (' ':cs) -> Just (' ', cs)
  _ -> Nothing


spaces :: Parser String
spaces = many space


lexeme :: Parser a -> Parser a
lexeme p = spaces *> p

parseOperator :: Parser Operator
parseOperator = lexeme $ Parser $ \case
  ('+' : rest) -> Just (Add, rest)
  ('-' : rest) -> Just (Sub, rest)
  ('*' : rest) -> Just (Mul, rest)
  ('/' : rest) -> Just (Div, rest)
  _ -> Nothing

parseChar :: Char -> Parser Char
parseChar expected = Parser $ \case
  (c:cs) | c == expected -> Just (expected, cs)
  _ -> Nothing

-- chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
-- chainl1 p op = do
--   x <- p -- parse first value
--   rest x -- parse remaining operations
--   where
--     rest x =
--       ( do
--           -- try to parse an operator and value
--           f <- op -- parse operator
--           y <- p -- parse another value
--           rest (f x y) -- recursively parse more with new accumulated value
--       )
--         <|> return x -- or just return accumulated value if no more operators


-- expr :: Parser Expr
-- expr = chainl1 term $ do
--   op <- parseOperator
--   case op of
--     Add -> return (BinOp Add)
--     Sub -> return (BinOp Sub)
--     _ -> empty

-- term :: Parser Expr
-- term = chainl1 factor $ do
--   op <- parseOperator
--   case op of
--     Mul -> return (BinOp Mul)
--     Div -> return (BinOp Div)
--     _ -> empty


-- -- Do notation version of expr
-- expr :: Parser Expr
-- expr = do
--     first <- term                     -- Parse the first term
--     rest <- many (do                  -- Parse any additional (operator + term) pairs
--         op <- parseOperator          -- Get the operator
--         case op of
--             Add -> do                -- If it's addition
--                 next <- term         -- Parse the next term
--                 return $ \x -> BinOp Add x next  -- Create function to combine terms
--             Sub -> do                -- If it's subtraction
--                 next <- term
--                 return $ \x -> BinOp Sub x next
--             _ -> empty)              -- Fail for multiply/divide
--     return $ foldl (\x f -> f x) first rest   -- Combine everything left-to-right

-- -- Do notation version of term
-- term :: Parser Expr
-- term = do
--     first <- factor                   -- Parse the first factor
--     rest <- many (do                  -- Parse any additional (operator + factor) pairs
--         op <- parseOperator
--         case op of
--             Mul -> do                -- If it's multiplication
--                 next <- factor
--                 return $ \x -> BinOp Mul x next
--             Div -> do                -- If it's division
--                 next <- factor
--                 return $ \x -> BinOp Div x next
--             _ -> empty)              -- Fail for add/subtract
--     return $ foldl (\x f -> f x) first rest


-- chainExpr :: Foldable t1 => t2 -> t1 (t2 -> t2) -> t2
chainExpr :: t -> [t -> t] -> t
chainExpr = foldl (\x f -> f x)


expr :: Parser Expr
expr = chainExpr <$> term <*> many (
    (\op next x -> BinOp op x next) <$> 
    (parseOperator >>= \case
        Add -> pure Add
        Sub -> pure Sub
        _ -> empty) <*> term)

term :: Parser Expr
term = chainExpr <$> factor <*> many (
    (\op next x -> BinOp op x next) <$>
    (parseOperator >>= \case
        Mul -> pure Mul
        Div -> pure Div
        _ -> empty) <*> factor)

factor :: Parser Expr
factor = (Num <$> parseInt) <|> (parseChar '(' *> expr <* parseChar ')')



eval :: Expr -> Int
eval (Num n) = n
eval (BinOp op a b) = eval a `x` eval b
  where
    x = case op of
      Add -> (+)
      Sub -> (-)
      Mul -> (*)
      Div -> div

extract :: Maybe a -> a
extract (Just x) = x
extract Nothing  = error "No value!"

-- getExpr = 
-- eval :: Expr -> Int
-- eval 
-- factor = 

-- factor :: Parser Int
-- factor = parseInt <|> parseParens

main :: IO ()
main = putStrLn "Hello, Haskell!"
