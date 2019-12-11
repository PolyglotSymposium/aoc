module Value
       ( Value(..)
       ) where

import Prelude hiding (True, False)

data Value
  = I Integer
  | Vs [Value]
  | True
  | False
  | Func (Value -> Value)

instance Show Value where
  show (I v) = show v
  show (Vs vs) = show $ map show vs
  show True = "true"
  show False = "false"
  show (Func _) = "<function>"
