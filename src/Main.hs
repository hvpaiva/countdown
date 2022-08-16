module Main (main) where

main :: IO ()
main = putStrLn . unlines . map show $ solve [1, 3, 7, 10, 25, 50] 765

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

solve :: [Int] -> Int -> [Expr]
solve ns n = [e | ns' <- choices ns, (e, m) <- results ns', m == n]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns, 
                     lx <- results ls, 
                     ry <- results rs, 
                     res <- combine' lx ry]
                     
combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(Apply o l r, exec o x y) | o <- ops, valid o x y]

