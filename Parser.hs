module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative
import Data.Char (isUpper, isDigit)

import Lambda
import Binding

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

-- Parser instances
instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, rest) <- p input
    return (f x, rest)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  (Parser pf) <*> (Parser pa) = Parser $ \input -> do
    (f, rest1) <- pf input
    (a, rest2) <- pa rest1
    return (f a, rest2)

instance Monad Parser where
  return = pure
  (Parser p) >>= f = Parser $ \input -> do
    (x, rest) <- p input
    parse (f x) rest

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    p1 input <|> p2 input

-- Primitive parsers
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $ \input ->
  case input of
    (c:cs) | pred c -> Just (c, cs)
    _ -> Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

letter :: Parser Char
letter = satisfy (`elem` ['a'..'z'])

variable :: Parser String
variable = some letter

spaces :: Parser ()
spaces = many (satisfy (== ' ')) *> pure ()

-- Lambda expression parser
lambdaExpr :: Parser Lambda
lambdaExpr = spaces *> (parseAbs <|> parseApp <|> parseVar) <* spaces

-- new
parseVar :: Parser Lambda
parseVar = do
  name <- some (satisfy (\c ->
              ('a' <= c && c <= 'z')  -- lowercase variable
           || ('A' <= c && c <= 'Z')  -- uppercase macro
           || ('0' <= c && c <= '9')))  -- digits
  -- decide whether it is a Macro or a Var
  if all (\c -> isUpper c || isDigit c) name
    then return (Macro name)
    else return (Var name)

parseAbs :: Parser Lambda
parseAbs = do
  char '\\' <|> char 'λ'
  spaces
  var <- variable
  spaces
  char '.'
  spaces
  body <- lambdaExpr
  return $ Abs var body

parseApp :: Parser Lambda
parseApp = do
  char '('
  spaces
  e1 <- lambdaExpr
  spaces
  e2 <- lambdaExpr
  spaces
  char ')'
  return $ App e1 e2

-- 2.1. parser entry point
parseLambda :: String -> Lambda
parseLambda input =
  case parse lambdaExpr input of
    Just (expr, "") -> expr
    Just (_, leftover) -> error $ "Parser did not consume entire input: " ++ leftover
    Nothing -> error "Failed to parse lambda expression"

-- 3.3. placeholder
parseLine :: String -> Either String Line
parseLine input =
  case parse bindingLine input of
    Just (result, "") -> Right result
    _ -> Left "Failed to parse line"

bindingLine :: Parser Line
bindingLine = tryBinding <|> tryEval

tryBinding :: Parser Line
tryBinding = do
  name <- some (satisfy (`elem` ['A'..'Z'] ++ ['0'..'9']))
  spaces
  char '='
  spaces
  expr <- lambdaExpr
  return $ Binding name expr

tryEval :: Parser Line
tryEval = Eval <$> lambdaExpr
