module Binding where

import Lambda
import qualified Data.Map as M

type Context = [(String, Lambda)]

data Line = Eval Lambda 
          | Binding String Lambda
          deriving (Eq)

instance Show Line where
    show (Eval l)       = show l
    show (Binding s l)  = s ++ " = " ++ show l

-- 3.1. Înlocuiește macro-urile cu valorile din context și evaluează
simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx step expr = do
  let ctxMap = M.fromList ctx
  expanded <- expand ctxMap expr
  return (simplify step expanded)

-- Funcție auxiliară care înlocuiește recursiv macro-urile
expand :: M.Map String Lambda -> Lambda -> Either String Lambda
expand ctx l = case l of
  -- plain variable (lowercase names)
  Var x ->
    Right (Var x)

  -- macro invocation: uppercase+digits
  Macro m ->
    case M.lookup m ctx of
      Just val -> expand ctx val
      Nothing  -> Left $ "Unknown macro: " ++ m

  -- abstraction: recurse into the body
  Abs x body ->
    Abs x <$> expand ctx body

  -- application: recurse on both sides
  App a b ->
    App <$> expand ctx a <*> expand ctx b

-- Helpers for the two strategies
normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
