{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Builtins
       ( listContext
       , conwayContext
       , programContext
       , turtleContext
       ) where

import qualified Ast
import qualified Conway.Ast as Conway
import qualified Turtle.Ast as Turtle
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import           Data.Text hiding (count, length, foldr, zip, maximum, concat)
import           Evaluator (evalValue, toBoolean)
import qualified Program.Ast as Program
import qualified Type
import qualified Value as C
import qualified Value

makeFold :: Integer -> (Integer -> Integer -> Integer) -> Value.Value
makeFold i f = Value.Fold (Value.I i, \v1 v2 ->
  case (v1, v2) of
    (Value.I v, Value.I acc) -> Just $ Value.I $ f v acc
    _                        -> Nothing)

count :: Value.Value
count = Value.Func $ \_ v ->
  case v of
    Value.Vs vs -> Just $ Value.I $ toInteger $ length vs
    _ -> Nothing

repeats :: Value.Value
repeats = Value.Func $ \_ v -> Value.Vs <$> go S.empty v
  where
    go _ (Value.Vs []) = Just []
    go seen (Value.Vs (v:vs)) = do
      ord  <- Value.toOrd v
      rest <- go (S.insert ord seen) $ Value.Vs vs
      pure $ [v | S.member ord seen] ++ rest
    go _ _ = Nothing

first :: Value.Value
first = Value.Func $ \_ -> \case
                      Value.Vs (v:_) -> Just v
                      _ -> Nothing

maximum' :: Value.Value
maximum' = Value.Func $ \_ -> \case
                      Value.Vs vs -> Value.I . maximum <$> extractIntegers vs
                      _ -> Nothing
  where
    extractIntegers [] = Just []
    extractIntegers (Value.I i:rest) = do
      others <- extractIntegers rest
      pure $ i:others
    extractIntegers _ = Nothing

even' :: Value.Value
even' = Value.Func $ \_ -> \case
                      Value.I v -> Just $ toBoolean $ even v
                      _ -> Nothing

indexOf0 :: Value.Value
indexOf0 = Value.Func $ \_ v -> Just $ Value.Func $ \_ vs ->
  case vs of
    Value.Vs items -> seekIndex 0 v items
    _ -> Nothing

  where
    seekIndex _ _ [] = Nothing
    seekIndex idx v (v':_) | Value.toOrd v =?= Value.toOrd v' = Just $ Value.I idx
    seekIndex idx v (_:rest) = seekIndex (idx+1) v rest

(=?=) :: Eq a => Maybe a -> Maybe a -> Bool
Nothing =?= _ = False
_ =?= Nothing = False
Just x =?= Just y = x == y

odd' :: Value.Value
odd' = Value.Func $ \_ -> \case
                      Value.I v -> Just $ toBoolean $ odd v
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

allAdjacent :: (Integer, Integer) -> [(Integer, Integer)]
allAdjacent (x, y) =
  [
    (x+1, y),
    (x,   y+1),
    (x,   y-1),
    (x-1, y)
  ]

allSurrounding :: (Integer, Integer) -> [(Integer, Integer)]
allSurrounding (x, y) =
  [
    (x+1, y),
    (x+1, y+1),
    (x+1, y-1),
    (x,   y+1),
    (x,   y-1),
    (x-1, y),
    (x-1, y+1),
    (x-1, y-1)
  ]

adjacent :: Value.Value
adjacent = Value.Func $ \context cell ->
  case (C.identValue "$grid" context, C.identValue "$pos" context, cell) of
    (Just (Value.Grid _ _ state), Just (Value.Pos coords), Value.CellState c) ->
      Just $
        Value.I $
        toInteger $
        M.size $
        M.filter (== c) $
        M.restrictKeys state $
        S.fromList $
        allAdjacent coords
    _ -> Nothing

neighbors :: Value.Value
neighbors = Value.Func $ \context cell ->
  case (C.identValue "$grid" context, C.identValue "$pos" context, cell) of
    (Just (Value.Grid _ _ state), Just (Value.Pos coords), Value.CellState c) ->
      Just $
        Value.I $
        toInteger $
        M.size $
        M.filter (== c) $
        M.restrictKeys state $
        S.fromList $
        allSurrounding coords
    _ -> Nothing

left :: Value.Value
left = Value.Func $ \context cell ->
  case (C.identValue "$grid" context, C.identValue "$pos" context, cell) of
    (Just (Value.Grid _ _ state), Just (Value.Pos (x, y)), Value.CellState c) ->
      case (M.lookup (x-1, y) state, C.identValue "$oob" context) of
        (Just c', _)                          -> Just $ toBoolean $ c' == c
        (Nothing, Just (Value.CellState oob)) -> Just $ toBoolean $ oob == c
        _                                     -> Nothing

    _ -> Nothing

right :: Value.Value
right = Value.Func $ \context cell ->
  case (C.identValue "$grid" context, C.identValue "$pos" context, cell) of
    (Just (Value.Grid _ _ state), Just (Value.Pos (x, y)), Value.CellState c) ->
      case (M.lookup (x+1, y) state, C.identValue "$oob" context) of
        (Just c', _)                          -> Just $ toBoolean $ c' == c
        (Nothing, Just (Value.CellState oob)) -> Just $ toBoolean $ oob == c
        _                                     -> Nothing

    _ -> Nothing

at :: Value.Value
at = Value.Func $ \ctx arg ->
  case (C.identValue "$pos" ctx, arg) of
    (Just (Value.Pos current), Value.Pos desired) ->
      Just $ toBoolean $ current == desired
    (Just current, Value.Func f) ->
      f ctx current
    _ -> Nothing

corner :: Value.Value
corner = Value.Func $ \ctx current ->
  case (C.identValue "$grid" ctx, current) of
    (Just (Value.Grid _ size _), Value.Pos (x, y)) ->
      let
        width = Value.width size
        height = Value.height size
      in
        Just $
          toBoolean $
            (x == 0 && (y == 0 || y == height-1)) || (x == width-1 && (y == 0 || y == height-1))
    _ -> Nothing

firstRepeatedGeneration :: Value.Value
firstRepeatedGeneration = Value.Func $ \context v -> go S.empty context v
  where
    go seen context grd@Value.Grid{} = do
      ord <- Value.toOrd grd
      if S.member ord seen
      then
        pure grd
      else do
        next <- nextGeneration' context grd
        go (S.insert ord seen) context next
    go _ _ _ = Nothing

afterTransitions :: Value.Value
afterTransitions = Value.Func $ \ctx n -> Just $ Value.Func $ \_ grd -> go n ctx grd
  where
    go (Value.I 0) _ grd           = Just grd
    go (Value.I n) ctx grd | n > 0 = do
                     next <- nextGeneration' ctx grd
                     go (Value.I (n - 1)) ctx next
    go _ _ _ = Nothing

to2dWithTransitions :: Value.Value
to2dWithTransitions = Value.Func $ \ctx n -> Just $ Value.Func $ \_ grd -> go n 0 ctx grd
  where
    go (Value.I n) y _ (Value.Grid ts size state) | y == n =
      Just $ setY n ts (size { Value.height = 1 }) state
    go (Value.I n) y ctx (Value.Grid ts size state) | y < n = do
      let myState = M.mapKeys (setY' y) state
      next <- nextGeneration' ctx $ setY 0 ts size state
      remainder <- go (Value.I n) (y + 1) ctx next
      case remainder of
        Value.Grid _ remSize remState -> Just $ Value.Grid ts (remSize { Value.width=Value.width remSize + 1 }) $ M.union myState remState
        _ -> Nothing

    go _ _ _ _ = Nothing

    setY y ts size = Value.Grid ts size . M.mapKeys (setY' y)

    setY' y (x, _) = (x, y)

nextGeneration :: Value.Value
nextGeneration = Value.Func nextGeneration'

nextGeneration' :: C.Context -> Value.Value -> Maybe Value.Value
nextGeneration' context grd@(Value.Grid ts@Conway.CellTransitions{ .. } size state) =
  Just $ Value.Grid ts size $ M.mapWithKey (transition (C.insert "$grid" (Type.Grid, grd) context)) state
    where
      transition ctx coords c =
        fromMaybe (Conway.ident otherwiseCellIs) $
          matching cases coords c $
          M.insert "$pos" (Type.Position, Value.Pos coords) ctx

      matching [] _ _ _ = Nothing
      matching ((from, to, cond):cs) coords c ctx =
        if Conway.ident from == c && evaluatesToTrue ctx cond
        then Just $ Conway.ident to
        else matching cs coords c ctx

nextGeneration' _ _ = Nothing

readingOrder :: Value.Value
readingOrder = Value.Func readingOrder'

readingOrder' :: C.Context -> Value.Value -> Maybe Value.Value
readingOrder' context ps =
  -- TODO generation_0 is not the best thing to use here since grids _can_ change
  case (C.identValue "$generation_0" context, ps) of
    (Just (Value.Grid _ size _), Value.Vs vs) ->
      Value.Vs <$> mapM (fmap (readingIndex (Value.width size)) . getPos) vs
    _ -> Nothing

  where
    readingIndex width (x, y) = Value.I (x + width * y)

countCells :: Value.Value
countCells = Value.Func $ \_ vs -> Just $ Value.Func $ \_ grd -> countCells' vs grd

traceRegisterValue :: Text -> Integer -> Value.Traces -> Value.Traces
traceRegisterValue r value Value.Traces{registerValues=Just (Value.RegHistory rh) } =
  Value.Traces $ Just $ Value.RegHistory $ M.alter (Just . (value :) . fromMaybe []) r rh
traceRegisterValue _ _ ts = ts

run :: Value.Value
run = Value.Func $ \context program ->
  case program of
    Value.Program (Program.IndexedInstructions p) (Value.Ip ip) (Value.Regs regs) traces ->
      run' p ip regs context traces
    _ -> Nothing

  where
    run' p ip regs context traces =
      case M.lookup ip p of
        Nothing -> Just $ Value.Program (Program.IndexedInstructions p) (Value.Ip ip) (Value.Regs regs) traces
        Just Program.Instruction{..} ->
          let
            currentContext = foldr (\(name, value) -> C.insert name (Type.Number, Value.I value)) context $ M.toList regs
            shouldExecute =
              case when of
                Just condition -> evaluatesToTrue currentContext condition
                Nothing -> True
          in
            if not shouldExecute
            then run' p (ip+1) regs context traces
            else
              case op of
                Program.JumpAway v ->
                  case evalValue currentContext Nothing v of
                    Right (Value.I offset) -> run' p (ip + offset) regs context traces
                    _ -> Nothing
                Program.Set dest v ->
                  case evalValue currentContext Nothing v of
                    Right (Value.I result) ->
                      run' p (ip + 1) (M.insert dest result regs) context $ traceRegisterValue dest result traces
                    _ -> Nothing

register :: Value.Value
register = Value.Func $ \_ r -> Just $ Value.Func $ \ _ program ->
  case (r, program) of
    (Value.Register name, Value.Program _ _ (Value.Regs regs) _) -> Just $ Value.I $ regs M.! name
    _ -> Nothing

incrementRegister :: Value.Value
incrementRegister = Value.Func $ \_ r -> Just $ Value.Func $ \ _ program ->
  case (r, program) of
    (Value.Register name, Value.Program p ip (Value.Regs regs) traces) ->
      let
        value = regs M.! name + 1
      in
      Just $ Value.Program p ip (Value.Regs $ M.insert name value regs) $ traceRegisterValue name value traces
    _ -> Nothing

registerValues :: Value.Value
registerValues = Value.Func $ \ _ program ->
  case program of
    (Value.Program _ _ (Value.Regs regs) _) ->
      Just $ Value.Vs $ Value.I . snd <$> M.toList regs
    _ -> Nothing

tracedRegisterValues :: Value.Value
tracedRegisterValues = Value.Func $ \ _ program ->
  case program of
    (Value.Program _ _ _ Value.Traces{registerValues=Just (Value.RegHistory rh) }) ->
      Just $ Value.Vs $ Value.I <$> (M.toList rh >>= snd)
    _ -> Nothing

getCellState :: Value.Value -> Maybe Char
getCellState (Value.CellState c) = Just c
getCellState _ = Nothing

countCells' :: Value.Value -> Value.Value -> Maybe Value.Value
countCells' (Value.Vs vs) grd =
  case (grd, sequence $ getCellState <$> vs) of
    (Value.Grid _ size state, Just buckets) ->
      let
        counts = go (Value.width size) (Value.height size) state $ M.fromList $ zip buckets (repeat 0)
      in
        Just $ Value.Vs $ fmap (\v -> Value.I (counts M.! v)) buckets
    _ -> Nothing

  where
    go width height state buckets =
      foldr (M.adjust (+ 1)) buckets [state M.! (x, y) | x <- [0..width-1], y <- [0..height-1]]

countCells' _ _ = Nothing

getPos :: Value.Value -> Maybe (Integer, Integer)
getPos (Value.Pos coord) = Just coord
getPos _ = Nothing

positions :: Value.Value
positions = Value.Func $ \_ cs -> Just $ Value.Func $ \_ grd ->
  case (cs, grd) of
    (Value.CellState c, Value.Grid _ _ state) ->
      Just $
        Value.Vs $
        fmap Value.Pos $
        M.keys $
        M.filter (== c) state
    _ -> Nothing

face :: Value.Value
face = Value.Func $ \_ dir -> Just $ Value.Func $ \_ trt ->
  case (dir, trt) of
    (Value.Direction d, Value.Turtle p _ actions) ->
      Just $ Value.Turtle p d actions
    _ ->
      Nothing

currentPosition :: Value.Value -> Maybe (Integer, Integer)
currentPosition (Value.Turtle p _ _ ) = Just p
currentPosition (Value.Pos p) = Just p
currentPosition _ = Nothing

manhattanDistance :: Value.Value
manhattanDistance = Value.Func $ \_ source -> Just $ Value.Func $ \_ v ->
  case (source, currentPosition v) of
    (Value.Pos (x0, y0), Just (x1, y1)) ->
      Just $ Value.I $ abs (x0 - x1) + abs (y0 - y1)
    _ ->
      Nothing

strollSplat :: Value.Value
strollSplat = Value.Func $ \_ trt ->
  case trt of
    (Value.Turtle p d actions) ->
      Just $ Value.Vs $ stroll' p d actions
    _ ->
      Nothing

  where
    stroll' p _ [] = [Value.Pos p]
    stroll' p _ (Turtle.Face d:rest) = stroll' p d rest
    stroll' p d (Turtle.Turn hs:rest) = stroll' p (turn d hs) rest
    stroll' p d (Turtle.TakeSteps 0:rest) = stroll' p d rest
    stroll' p d (Turtle.TakeSteps ss:rest) =
      Value.Pos p:stroll' (step d ss p) d (Turtle.TakeSteps (approachZero ss):rest)

    approachZero v | v > 0 = v - 1
    approachZero v = v + 1

    signOne v | v > 0 = 1
    signOne _ = -1

    step Turtle.Up n (x, y) = (x, y + signOne n)
    step Turtle.Down n (x, y) = (x, y - signOne n)
    step Turtle.Left n (x, y) = (x - signOne n, y)
    step Turtle.Right n (x, y) = (x + signOne n, y)

turn :: Turtle.Direction -> Turtle.Side -> Turtle.Direction
turn Turtle.Up Turtle.Lefthand = Turtle.Left
turn Turtle.Up Turtle.Righthand = Turtle.Right
turn Turtle.Left Turtle.Lefthand = Turtle.Down
turn Turtle.Left Turtle.Righthand = Turtle.Up
turn Turtle.Down Turtle.Lefthand = Turtle.Right
turn Turtle.Down Turtle.Righthand = Turtle.Left
turn Turtle.Right Turtle.Lefthand = Turtle.Up
turn Turtle.Right Turtle.Righthand = Turtle.Down

stroll :: Value.Value
stroll = Value.Func $ \_ trt ->
  case trt of
    (Value.Turtle p d actions) ->
      Just $ stroll' p d actions actions
    _ ->
      Nothing

  where
    stroll' p d [] = Value.Turtle p d
    stroll' p _ (Turtle.Face d:rest) = stroll' p d rest
    stroll' p d (Turtle.Turn hs:rest) = stroll' p (turn d hs) rest
    stroll' p d (Turtle.TakeSteps ss:rest) = stroll' (steps d ss p) d rest

    steps Turtle.Up n (x, y) = (x, y+n)
    steps Turtle.Down n (x, y) = (x, y-n)
    steps Turtle.Left n (x, y) = (x-n, y)
    steps Turtle.Right n (x, y) = (x+n, y)

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

prog :: Type.Type
prog = Type.Program

reg :: Type.Type
reg = Type.Register

cellState :: Type.Type
cellState = Type.CellState

pos :: Type.Type
pos = Type.Position

direction :: Type.Type
direction = Type.Direction

turtle :: Type.Type
turtle = Type.Turtle

baseIdentifiers :: [(Text, Type.Type, Value.Value)]
baseIdentifiers =
  [
    ("sum",                list num --> num,               makeFold 0 (+))
  , ("count",              list a   --> num,               count)
  , ("product",            list num --> num,               makeFold 1 (*))
  , ("repeats",            list a --> list a,              repeats)
  , ("true",               bool,                           Value.True)
  , ("false",              bool,                           Value.False)
  , ("first",              list a --> a,                   first)
  , ("dupe",               list a --> list a,              dupe)
  , ("even",               num --> bool,                   even')
  , ("odd",                num --> bool,                   odd')
  , ("maximum",            list num --> num,               maximum')
  , ("base_zero_index_of", a --> (list a --> num),         indexOf0)
  , ("manhattan_distance", pos --> (pos --> num),          manhattanDistance)
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
      ("first_repeated_generation", (grid      --> grid,                     firstRepeatedGeneration))
    , ("next_generation",           (grid      --> grid,                     nextGeneration))
    , ("to_2d_with_transitions",    (num       --> (grid --> grid),          to2dWithTransitions))
    , ("after_transitions",         (num       --> (grid --> grid),          afterTransitions))
    , ("positions",                 (cellState --> (grid --> list pos),      positions))
    , ("neighbors",                 (cellState --> num,                      neighbors))
    , ("corner",                    (pos,                                    corner))
    , ("at",                        (pos       --> bool,                     at))
    , ("adjacent",                  (cellState --> num,                      adjacent))
    , ("left",                      (cellState --> bool,                     left))
    , ("right",                     (cellState --> bool,                     right))
    , ("reading_order",             (list pos  --> list num,                 readingOrder))
    , ("count_cells",               (list cellState --> (grid --> list num), countCells))
    ]

programContext :: C.Context
programContext =
  C.add core $
    C.fromList [
      ("run",                    (prog --> prog,            run))
    , ("register",               (reg  --> (prog --> prog), register))
    , ("increment_register",     (reg  --> (prog --> prog), incrementRegister))
    , ("register_values",        (prog --> list num,        registerValues))
    , ("traced_register_values", (prog --> list num,        tracedRegisterValues))
    ]

turtleContext :: C.Context
turtleContext =
  C.add core $
    C.fromList [
      ("face",                    (direction --> (turtle --> turtle), face))
    , ("origin",                  (pos,                               Value.Pos (0, 0)))
    , ("up",                      (direction,                         Value.Direction Turtle.Up))
    , ("north",                   (direction,                         Value.Direction Turtle.Up))
    , ("down",                    (direction,                         Value.Direction Turtle.Down))
    , ("south",                   (direction,                         Value.Direction Turtle.Down))
    , ("left",                    (direction,                         Value.Direction Turtle.Left))
    , ("west",                    (direction,                         Value.Direction Turtle.Left))
    , ("right",                   (direction,                         Value.Direction Turtle.Right))
    , ("east",                    (direction,                         Value.Direction Turtle.Right))
    , ("stroll",                  (turtle --> turtle,                 stroll))
    , ("stroll*",                 (turtle --> list pos,               strollSplat))
    , ("distance_from",           (pos --> (turtle -->  num),         manhattanDistance))
    ]
