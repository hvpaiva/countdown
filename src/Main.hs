module Main (main) where

main :: IO ()
main =  putStrLn . unlines . map show $ solve [1, 3, 7, 10, 25, 50] 765

data Op
  = Add
  | Sub
  | Mul
  | Div

instance Show Op where
  show Add = " + "
  show Sub = " - "
  show Mul = " * "
  show Div = " / "

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div _ 0 = False
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Maybe Int
apply op x y =
  if valid op x y
    then return $ exec op x y
    else Nothing

exec :: Op -> Int -> Int -> Int
exec Add x y = x + y
exec Sub x y = x - y
exec Mul x y = x * y
exec Div x y = x `div` y

data Expr
  = Val Int
  | Apply Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (Apply op l r) = bracket l ++ show op ++ bracket r
    where
      bracket (Val n) = show n
      bracket e = "(" ++ show e ++ ")"

eval :: Expr -> Maybe Int
eval (Val n)
  | n > 0 = return n
  | otherwise = Nothing
eval (Apply op l r) = do
  x <- eval l
  y <- eval r
  apply op x y

subsequence :: [a] -> [[a]]
subsequence [] = [[]]
subsequence (x : xs) = xss ++ map (x :) xss
  where
    xss = subsequence xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

permutation :: [a] -> [[a]]
permutation = foldr (concatMap . interleave) [[]]

choices :: [a] -> [[a]]
choices = concatMap permutation . subsequence

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [Apply o l r | o <- ops]

solve :: [Int] -> Int -> [Expr]
solve ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == Just n]

