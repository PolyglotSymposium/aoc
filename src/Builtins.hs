{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Builtins
       ( listContext
       , conwayContext
       , identType
       , identValue
       , Context
       , add
       ) where

import Data.Text
import qualified Data.Map.Strict as M
import qualified Type as Type
import qualified Value as Value
import qualified Data.Set as S

makeFold :: Integer -> (Integer -> Integer -> Integer) -> Value.Value
makeFold i f = Value.Fold (Value.I i, \v1 v2 ->
  case (v1, v2) of
    (Value.I v, Value.I acc) -> Just $ Value.I $ f v acc
    _                        -> Nothing)

repeats :: Value.Value
repeats = Value.Func $ \v -> Value.Vs <$> go S.empty v
  where
    go _ (Value.Vs []) = Just []
    go seen (Value.Vs (v:vs)) = do
      ord <- Value.toOrd v
      let acc = if S.member ord seen then [v] else []
      rest <- go (S.insert ord seen) (Value.Vs vs)
      pure $ acc ++ rest
    go _ _ = Nothing

first :: Value.Value
first = Value.Func $ \case
                      Value.Vs (v:_) -> Just v
                      _ -> Nothing

dupe :: Value.Value
dupe = Value.Func $ \case
                      Value.Vs vs -> Just (Value.Vs (vs ++ vs))
                      _ -> Nothing

(-->) :: Type.Type -> Type.Type -> Type.Type
i --> o = Type.Arrow i o

list :: Type.Type -> Type.Type
list = Type.List

a :: Type.Type
a = Type.Var 'a'

num :: Type.Type
num = Type.Number

bool :: Type.Type
bool = Type.Boolean

grid :: Type.Type
grid = Type.Grid

cellState :: Type.Type
cellState = Type.CellState

todo :: Value.Value
todo = Value.False

pos :: Type.Type
pos = Type.Position

baseIdentifiers :: [(Text, Type.Type, Value.Value)]
baseIdentifiers =
  [
    ("sum",                       list num --> num,      makeFold 0 (+))
  , ("product",                   list num --> num,      makeFold 1 (*))
  , ("repeats",                   list a --> list a,     repeats)
  , ("true",                      bool,                  Value.True)
  , ("false",                     bool,                  Value.False)
  , ("first",                     list a --> a,          first)
  , ("dupe",                      list a --> list a,     dupe)
  , ("reading_order",             list pos --> list num, todo)
  ]

core :: Context
core = M.fromList $ do
  (name, t, v) <- baseIdentifiers
  case (t, v) of
    (Type.Arrow iT oT, Value.Fold step) -> [(name, (t, v)), (append name "*", (iT --> list oT, Value.StepsOfFold step))]
    _    -> [(name, (t, v))]

type Context = M.Map Text (Type.Type, Value.Value)

identType :: Text -> Context -> Maybe Type.Type
identType ident ctx = fst <$> M.lookup ident ctx

identValue :: Text -> Context -> Maybe Value.Value
identValue ident ctx = snd <$> M.lookup ident ctx

listContext :: Context
listContext = core

conwayContext :: Context
conwayContext =
  M.insert "first_repeated_generation" (grid --> grid, todo) $
  M.insert "positions" (cellState --> (grid --> list pos), todo)
    core

add :: Context -> Context -> Context
add = M.union
