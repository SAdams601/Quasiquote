{-# LANGUAGE QuasiQuotes #-}
module EvalExpr where
import Expr

eval' :: Expr -> Integer
eval' [ex|$int:x|] = x
eval' [ex|$x + $y|] = eval' x + eval' y
eval' [ex|$x - $y|] = eval' x - eval' y
eval' [ex|$x * $y|] = eval' x * eval' y
eval' [ex|$x / $y|] = eval' x `div` eval' y
