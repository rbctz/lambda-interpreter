module Default where

import Lambda
import Binding

-- Variables (for convenience)
vx = Var "x"
vy = Var "y"
vz = Var "z"
vf = Var "f"
vg = Var "g"
vh = Var "h"
vm = Var "m"
vn = Var "n"

-- Basic combinators
m = Abs "x" $ App vx vx
i = Abs "x" $ vx
k = Abs "x" $ Abs "y" $ vx
ki = Abs "x" $ Abs "y" $ vy
c = Abs "x" $ Abs "y" $ Abs "z" $ App (App vx vz) vy
y = Abs "f" $ App fix fix
  where fix = Abs "x" $ App vf (App vx vx)

-- 4.1. Boolean encodings
bTrue  = Abs "t" $ Abs "f" $ Var "t"
bFalse = Abs "t" $ Abs "f" $ Var "f"
bAnd   = Abs "p" $ Abs "q" $
            App (App (Var "p") (Var "q")) bFalse
bOr    = Abs "p" $ Abs "q" $
            App (App (Var "p") bTrue) (Var "q")
bNot   = Abs "p" $
            App (App (Var "p") bFalse) bTrue
bXor   = Abs "p" $ Abs "q" $
            App
              (App (Var "p")
                   (App (App (Var "q") bFalse) bTrue))
              (App (App (Var "q") bTrue) bFalse)

-- 4.2. Pair encodings
pair   = Abs "x" $ Abs "y" $ Abs "f" $
            App (App (Var "f") (Var "x")) (Var "y")
first  = Abs "p" $
            App (Var "p") (Abs "x" $ Abs "y" $ Var "x")
second = Abs "p" $
            App (Var "p") (Abs "x" $ Abs "y" $ Var "y")

-- 4.3. Natural number encodings (Church numerals)
n0     = Abs "f" $ Abs "x" $ Var "x"
n1     = Abs "f" $ Abs "x" $ App (Var "f") (Var "x")
n2     = Abs "f" $ Abs "x" $
            App (Var "f") (App (Var "f") (Var "x"))
nSucc  = Abs "n" $ Abs "f" $ Abs "x" $
            App (Var "f") (App (App (Var "n") (Var "f")) (Var "x"))
nAdd   = Abs "m" $ Abs "n" $ Abs "f" $ Abs "x" $
            App
              (App (Var "m") (Var "f"))
              (App (App (Var "n") (Var "f")) (Var "x"))
nMult  = Abs "m" $ Abs "n" $ Abs "f" $
            App (Var "m") (App (Var "n") (Var "f"))
nPred  = Abs "n" $
           Abs "f" $
           Abs "x" $
             App
               (App
                 (App (Var "n")
                      (Abs "g" $ Abs "h" $
                         App (Var "h") (App (Var "g") (Var "f"))))
                 (Abs "u" $ Var "x"))
               (Abs "u" $ Var "u")
nSub   = Abs "m" $ Abs "n" $
            App (App (Var "n") nPred) (Var "m")

-- Default Context
defaultContext :: Context
defaultContext = 
    [ ("M", m)
    , ("I", i)
    , ("K", k)
    , ("KI", ki)
    , ("C", c)
    , ("Y", y)
    , ("TRUE", bTrue)
    , ("FALSE", bFalse)
    , ("AND", bAnd)
    , ("OR", bOr)
    , ("NOT", bNot)
    , ("XOR", bXor)
    , ("PAIR", pair)
    , ("FST", first)
    , ("SND", second)
    , ("N0", n0)
    , ("N1", n1)
    , ("N2", n2)
    , ("SUCC", nSucc)
    , ("PRED", nPred)
    ,("ADD", nAdd)
    , ("SUB", nSub)
    , ("MULT", nMult)
    ]
