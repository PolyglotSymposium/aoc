module Value
       ( Value(..)
       , toOrd
       ) where

import Prelude hiding (True, False)
import Data.List (intersperse)

data Value
  = I Integer
  | Vs [Value]
  | True
  | False
  | Fold (Value, Value -> Value -> Maybe Value)
  | Func (Value -> Maybe Value)
  | StepsOfFold (Value, Value -> Value -> Maybe Value)
  | CellState Char

data OrdValue
  = OrdI Integer
  | OrdVs [OrdValue]
  | OrdTrue
  | OrdFalse
  | OrdCellState Char
  deriving (Ord, Eq)

toOrd :: Value -> Maybe OrdValue
toOrd (I v)           = Just $ OrdI v
toOrd (Vs vs)         = OrdVs <$> (sequence $ map toOrd vs)
toOrd True            = Just $ OrdTrue
toOrd False           = Just $ OrdFalse
toOrd (CellState c)   = Just $ OrdCellState c
toOrd (Fold _)        = Nothing
toOrd (Func _)        = Nothing
toOrd (StepsOfFold _) = Nothing

instance Show Value where
  show (I v) = show v
  show (Vs vs) = "[" ++ concat (intersperse "," (map show vs)) ++ "]"
  show True = "true"
  show False = "false"
  show (Fold _) = "<function/fold>"
  show (StepsOfFold _) = "<function/fold_steps>"
  show (Func _) = "<function>"
  show (CellState c) = "{cell:" ++ [c] ++ "}"
