{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Data.List
import Data.String

data Expr
  = Var String
  | Abs String Expr
  | App Expr Expr
  deriving (Eq)

instance Show Expr where
  show (Var a) = a
  show (Abs x e) = "λ" ++ x ++ "." ++ show e
  show (App (Var x) (Var y)) = show (Var x) ++ " " ++ show (Var y)
  show (App (Var x) e2) = show (Var x) ++ " (" ++ show e2 ++ ")"
  show (App e1 (Var y)) = "(" ++ show e1 ++ ") " ++ show (Var y)
  show (App e1 e2) = "(" ++ show e1 ++ ") (" ++ show e2 ++ ")"

λ :: String -> Expr -> Expr
λ = Abs

(·) :: Expr -> Expr -> Expr
(·) = App

infixr 9 ·

instance IsString Expr where
  fromString x = Var x

boundVars :: Expr -> [String]
boundVars (Var _) = []
boundVars (Abs x e) = x : boundVars e
boundVars (App e1 e2) = boundVars e1 ++ boundVars e2

freeVars :: Expr -> [String]
freeVars (Var x) = [x]
freeVars (Abs x e) = freeVars e \\ [x]
freeVars (App e1 e2) = freeVars e1 ++ freeVars e2

renameAll :: String -> String -> Expr -> Expr
renameAll from to (Var y) =
  if y == from
    then Var to
    else Var y
renameAll from to (Abs y ex) =
  if y == from
    then Abs to (renameAll from to ex)
    else Abs y ex
renameAll from to (App e1 e2) =
  App (renameAll from to e1) (renameAll from to e2)

alphaRename :: String -> String -> Expr -> Expr
alphaRename _from _to (Var x) = Var x
alphaRename from to (Abs x e) =
  if x == from
    then Abs to (renameAll from to e)
    else Abs x (alphaRename from to e)
alphaRename from to (App e1 e2) =
  App (alphaRename from to e1) (alphaRename from to e2)

betaReduce :: Expr -> Maybe Expr
betaReduce (Var _x) = Nothing
betaReduce (Abs x e) =
  case betaReduce e of
    Nothing -> Nothing
    Just e1 -> Just (Abs x e1)
betaReduce (App (Abs x e1) e2) =
  let e1_2 =
        foldl' (\e v -> alphaRename v (v ++ "′") e) (Abs x e1) (freeVars e2)
      x_2 =
        if x `elem` freeVars e2
          then x ++ "′"
          else x
   in case e1_2 of
        (Abs _x_3 e1_3) -> Just (replaceAll x_2 e2 e1_3)
        _ -> error "Impossible!"
  where
    replaceAll from to (Var y) =
      if y == from
        then to
        else Var y
    replaceAll from to (Abs y e) = Abs y (replaceAll from to e)
    replaceAll from to (App exp1 exp2) =
      App (replaceAll from to exp1) (replaceAll from to exp2)
betaReduce (App e1 e2) =
  case betaReduce e1 of
    Nothing ->
      case betaReduce e2 of
        Nothing -> Nothing
        Just e2' -> Just (App e1 e2')
    Just e1' -> Just (App e1' e2)

nf :: Expr -> Maybe Expr
nf e =
  case betaReduce e of
    Nothing -> Just e
    Just e2 -> betaReduce e2

showBetaReduction :: Expr -> IO ()
showBetaReduction e = do
  putStrLn $
    (show e) ++
    " → " ++
    (case betaReduce e of
       Nothing -> "Ø"
       Just e2 -> show e2)

showEvalToNf :: Int -> Expr -> IO ()
showEvalToNf fuel ex = do
  putStr $ (show ex)
  go fuel ex
  where
    go :: Int -> Expr -> IO ()
    go 0 _ = putStrLn " → ☠"
    go n e =
      case betaReduce e of
        Nothing -> putStrLn " → ■"
        Just e2 -> do
          putStr (" → " ++ show e2)
          go (n - 1) e2

main :: IO ()
main = do
  print $ alphaRename "x" "x_r" (λ "x" "x")
  showBetaReduction ((λ "x" ("x" · "x")) · (λ "y" ("y" · "y")))
  showBetaReduction "a"
  showBetaReduction $ (λ "x" "x") · "a"
  showBetaReduction $ (λ "x" "x") · (λ "x" "x")
  showBetaReduction $ (λ "x" ("x" · "x")) · (λ "x" ("x" · "x"))
  showBetaReduction $ (λ "x" (λ "y" ("x" · "y"))) · (λ "z" ((λ "c" "y") · "z"))
  print $ alphaRename "y" "y_r" $ (λ "y" ("x" · "y"))
  showBetaReduction $ (λ "x" (λ "y" ("x" · "y"))) · (λ "z" ((λ "c" "y") · "z"))
  showEvalToNf 10 $ (λ "x" (λ "y" ("x" · "y"))) · (λ "z" ((λ "c" "y") · "z"))
  showEvalToNf 5 ((λ "x" ("x" · "x")) · (λ "y" ("y" · "y")))
