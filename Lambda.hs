module Lambda where

import Data.List (nub, (\\))

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- Returns all variable names that appear in an expression
vars :: Lambda -> [String]
vars = nub . go
  where
    go (Var x) = [x]
    go (App e1 e2) = go e1 ++ go e2
    go (Abs x e) = x : go e
    go (Macro _) = []

-- Returns all free variable names in an expression
freeVars :: Lambda -> [String]
freeVars e = nub $ go e []
  where
    go (Var x) bound = if x `elem` bound then [] else [x]
    go (App e1 e2) bound = go e1 bound ++ go e2 bound
    go (Abs x e) bound = go e (x:bound)
    go (Macro _) _ = []

-- Returns the lexicographically smallest string not in the given list
newVar :: [String] -> String
newVar used = head $ dropWhile (`elem` used) candidates
  where
    candidates = concat [strings n | n <- [1..]]
    strings n = sequence (replicate n ['a'..'z'])

-- Checks if an expression is in normal form (contains no redexes)
isNormalForm :: Lambda -> Bool
-- folosirea functiei aux containsRedex
isNormalForm expr = not (containsRedex expr)
  where
    containsRedex (App (Abs _ _) _) = True
    containsRedex (App e1 e2) = containsRedex e1 || containsRedex e2
    containsRedex (Abs _ e) = containsRedex e
    containsRedex _ = False

-- Performs beta-reduction by substituting e2 for variable x in e1
reduce :: String -> Lambda -> Lambda -> Lambda
reduce x e1 e2 = 
  case e1 of
    Var y | y == x -> e2
          | otherwise -> Var y
    App left right -> App (reduce x left e2) (reduce x right e2)
    Abs y body
      | y == x -> Abs y body
      | y `elem` freeVars e2 -> 
          let freshVar = newVar (vars body ++ vars e2)
              renamedBody = reduce y body (Var freshVar)
              reducedBody = reduce x renamedBody e2
          in Abs freshVar reducedBody
      | otherwise -> Abs y (reduce x body e2)
    Macro m -> Macro m

-- Performs one step of normal order reduction (leftmost-outermost)
normalStep :: Lambda -> Lambda
normalStep expr = case expr of
  App (Abs x body) arg -> reduce x body arg
  App e1 e2 ->
    let e1' = normalStep e1
    in if e1' /= e1 then App e1' e2 else App e1 (normalStep e2)
  Abs x body -> Abs x (normalStep body)
  _ -> expr

-- Performs one step of applicative order reduction (leftmost-innermost)
applicativeStep :: Lambda -> Lambda
applicativeStep expr = case expr of
  App e1 e2 ->
    if containsRedex e1 then App (applicativeStep e1) e2
    else if containsRedex e2 then App e1 (applicativeStep e2)
    else case e1 of
      Abs x body -> reduce x body e2
      _ -> expr
  Abs x body -> Abs x (applicativeStep body)
  _ -> expr


-- Helper function for checking redexes
containsRedex :: Lambda -> Bool
containsRedex = not . isNormalForm

-- Applies a reduction strategy until normal form, returning all intermediate steps
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify step expr 
  -- daca se afla in forma normala, o returnam
  | isNormalForm expr = [expr]
  -- daca nu, mai aplicam un pas
  | otherwise = expr : simplify step (step expr)

-- Convenience functions for normal and applicative order reduction
normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
