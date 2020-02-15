{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Builtins
       ( listContext
       , conwayContext
       ) where

import qualified Ast
import qualified ConwayAst as Conway
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import           Data.Text
import           ListEvaluator (evalValue)
import qualified Type as Type
import qualified Value as C
import qualified Value as Value

makeFold :: Integer -> (Integer -> Integer -> Integer) -> Value.Value
makeFold i f = Value.Fold (Value.I i, \v1 v2 ->
  case (v1, v2) of
    (Value.I v, Value.I acc) -> Just $ Value.I $ f v acc
    _                        -> Nothing)

repeats :: Value.Value
repeats = Value.Func $ \_ v -> Value.Vs <$> go S.empty v
  where
    go _ (Value.Vs []) = Just []
    go seen (Value.Vs (v:vs)) = do
      ord <- Value.toOrd v
      let acc = if S.member ord seen then [v] else []
      rest <- go (S.insert ord seen) (Value.Vs vs)
      pure $ acc ++ rest
    go _ _ = Nothing

first :: Value.Value
first = Value.Func $ \_ -> \case
                      Value.Vs (v:_) -> Just v
                      _ -> Nothing

dupe :: Value.Value
dupe = Value.Func $ \_ -> \case
                      Value.Vs vs -> Just (Value.Vs (vs ++ vs))
                      _ -> Nothing

evaluatesToTrue :: Value.Context -> Ast.Value -> Bool
evaluatesToTrue context ast =
  case evalValue context Nothing ast of
    Right Value.True -> True
    _ -> False

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) =
  [
    (x-1, y-1),
    (x+1, y+1),
    (x-1, y+1),
    (x+1, y-1),
    (x,   y+1),
    (x,   y-1),
    (x+1, y),
    (x-1, y)
  ]

neighbors :: Value.Value
neighbors = Value.Func $ \context state ->
  case (C.identValue "$grid" context, C.identValue "$pos" context, state) of
    (Just (Value.Grid _ _ state), Just (Value.Pos pos), Value.CellState c) ->
      Just $
        Value.I $
        toInteger $
        M.size $
        M.filter (== c) $
        M.restrictKeys state $
        S.fromList $
        adjacent pos
    _ -> Nothing

nextGeneration :: Value.Value
nextGeneration = Value.Func nextGeneration'

nextGeneration' _ grid@(Value.Grid context ts@(Conway.CellTransitions { .. }) state) =
  Just $ Value.Grid context ts $ M.mapWithKey (transition (C.insert "$grid" (Type.Grid, grid) context)) state
    where
      transition context pos c =
        fromMaybe (Conway.ident otherwiseCellIs) $
          matching cases pos c $
          M.insert "$pos" (Type.Position, Value.Pos pos) context

      matching [] _ _ _ = Nothing
      matching ((from, to, cond):cases) pos c ctx =
        if Conway.ident from == c && evaluatesToTrue ctx cond
        then Just $ Conway.ident to
        else matching cases pos c ctx

nextGeneration' _ _ = Nothing

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

core :: C.Context
core = C.fromList $ do
  (name, t, v) <- baseIdentifiers
  case (t, v) of
    (Type.Arrow iT oT, Value.Fold step) -> [(name, (t, v)), (append name "*", (iT --> list oT, Value.StepsOfFold step))]
    _    -> [(name, (t, v))]


listContext :: C.Context
listContext = core

conwayContext :: C.Context
conwayContext =
  C.add core $
    C.fromList [
      ("first_repeated_generation", (grid      --> grid,                todo)),
      ("next_generation",           (grid      --> grid,                nextGeneration)),
      ("positions",                 (cellState --> (grid --> list pos), todo)),
      ("neighbors",                 (cellState --> num,                 neighbors))
    ]
