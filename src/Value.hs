module Value
       ( Value(..)
       , toOrd
       , identType
       , identValue
       , Context
       , add
       , fromList
       , insert
       ) where

import qualified ConwayAst as Conway
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Text hiding (concat)
import           Prelude hiding (True, False)
import qualified Type

data Value
  = I Integer
  | Vs [Value]
  | True
  | False
  | Fold (Value, Value -> Value -> Maybe Value)
  | Func (Context -> Value -> Maybe Value)
  | StepsOfFold (Value, Value -> Value -> Maybe Value)
  | CellState Char
  | Pos (Integer, Integer)
  | Grid Conway.CellTransitions (M.Map (Integer, Integer) Char)

data OrdValue
  = OrdI Integer
  | OrdVs [OrdValue]
  | OrdTrue
  | OrdFalse
  | OrdCellState Char
  | OrdPos (Integer, Integer)
  | OrdGrid (M.Map (Integer, Integer) Char)
  deriving (Ord, Eq)

toOrd :: Value -> Maybe OrdValue
toOrd (I v)            = Just $ OrdI v
toOrd (Vs vs)          = OrdVs <$> (sequence $ toOrd <$> vs)
toOrd True             = Just $ OrdTrue
toOrd False            = Just $ OrdFalse
toOrd (CellState s)    = Just $ OrdCellState s
toOrd (Pos coords)     = Just $ OrdPos coords
toOrd (Grid _ state)   = Just $ OrdGrid state
toOrd (Fold _)         = Nothing
toOrd (Func _)         = Nothing
toOrd (StepsOfFold _)  = Nothing

instance Show Value where
  show (I v) = show v
  show (Vs vs) = "[" ++ concat (L.intersperse "," $ show <$> vs) ++ "]"
  show True = "true"
  show False = "false"
  show (Fold _) = "<function/fold>"
  show (StepsOfFold _) = "<function/fold_steps>"
  show (Func _) = "<function>"
  show (CellState c) = "{cell:" ++ [c] ++ "}"
  show (Pos (x, y)) = "{pos:x=" ++ show x ++ ",y=" ++ show y ++ "}"
  show (Grid _ _) = "{grid}"

type Context = M.Map Text (Type.Type, Value.Value)

identType :: Text -> Context -> Maybe Type.Type
identType ident ctx = fst <$> M.lookup ident ctx

identValue :: Text -> Context -> Maybe Value.Value
identValue ident ctx = snd <$> M.lookup ident ctx

add :: Context -> Context -> Context
add = M.union

fromList :: [(Text, (Type.Type, Value.Value))] -> Context
fromList = M.fromList

insert :: Text -> (Type.Type, Value.Value) -> Context -> Context
insert = M.insert
