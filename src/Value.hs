module Value
       ( Value(..)
       ) where

import Prelude hiding (True, False)

data Value
  = I Integer
  | Vs [Value]
  | True
  | False
  | Fold (Value, Value -> Value -> Maybe Value)
  | StepsOfFold (Value, Value -> Value -> Maybe Value)

instance Show Value where
  show (I v) = show v
  show (Vs vs) = show $ map show vs
  show True = "true"
  show False = "false"
  show (Fold _) = "<function/fold>"
  show (StepsOfFold _) = "<function/fold_steps>"
