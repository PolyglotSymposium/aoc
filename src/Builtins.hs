{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Builtins
       ( listContext
       , conwayContext
       , programContext
       , turtleContext
       ) where

import qualified Ast
import qualified Conway.Ast as Conway
import           Control.Monad (replicateM)
import qualified Turtle.Ast as Turtle
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import           Data.Text hiding (count, length, foldr, zip, maximum, concat, filter, concatMap)
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

unique :: Value.Value
unique = Value.Func $ \_ v -> Value.Vs <$> go S.empty v
  where
    go _ (Value.Vs []) = Just []
    go seen (Value.Vs (v:vs)) = do
      ord  <- Value.toOrd v
      rest <- go (S.insert ord seen) $ Value.Vs vs
      pure $ [v | not $ S.member ord seen] ++ rest
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

combos :: Value.Value
combos = Value.Func $ \_ v -> Just $ Value.Func $ \_ vs ->
  case (v, vs) of
    (Value.I n, Value.Vs items) -> Just $ Value.Vs $ Value.Vs <$> replicateM (fromIntegral n) items
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

modX :: Value.Coord -> (Integer -> Integer) -> Value.Coord
modX (Value.D1 x) f = Value.D1 (f x)
modX (Value.D2 x y) f = Value.D2 (f x) y
modX (Value.D3 x y z) f = Value.D3 (f x) y z

allAdjacentCoords :: Conway.SolvableConwayDimensions -> Value.Coord -> [Value.Coord]
allAdjacentCoords Conway.OneD (Value.D1 x) = Value.D1 <$> [x-1, x+1]
allAdjacentCoords Conway.TwoD (Value.D2 x y) = uncurry Value.D2 <$> allAdjacent (x, y)
allAdjacentCoords Conway.ThreeD (Value.D3 x y z) = upDown ++ zAdjacent
  where
    upDown = Value.D3 x y <$> [z-1, z+1]
    zAdjacent = (\(x', y') -> Value.D3 x' y' z) <$> allAdjacent (x, y)
allAdjacentCoords  d coord = error ("adjacent: Got an out-of-dimension (" <> show d <> ") coordinate " <> show coord)

allSurroundingCoords :: Conway.SolvableConwayDimensions -> Value.Coord -> [Value.Coord]
allSurroundingCoords Conway.OneD (Value.D1 x) = Value.D1 <$> [x-1, x+1]
allSurroundingCoords Conway.TwoD (Value.D2 x y) = uncurry Value.D2 <$> allSurrounding (x, y)
allSurroundingCoords Conway.ThreeD p@(Value.D3 x y z) = filter (/= p) $ Value.D3 <$> [x-1..x+1] <*> [y-1..y+1] <*> [z-1..z+1]
allSurroundingCoords Conway.FourD p@(Value.D4 x y z w) = filter (/= p) $ Value.D4 <$> [x-1..x+1] <*> [y-1..y+1] <*> [z-1..z+1] <*> [w-1..w+1]
allSurroundingCoords d coord = error ("surrounding: Got an out-of-dimension (" <> show d <> ") coordinate " <> show coord)

pickMatching :: (Ord k, Eq a) => a -> M.Map k a -> [k] -> Maybe Value.Value
pickMatching c state =
  Just .
    Value.I .
    toInteger .
    M.size .
    M.filter (== c) .
    M.restrictKeys state .
    S.fromList

adjacent :: Value.Value
adjacent = Value.Func $ \context cell ->
  case (C.identValue "$grid" context, C.identValue "$pos" context, cell) of
    (Just (Value.Grid _ _ dim _ state), Just (Value.Coord coords), Value.CellState c) ->
      pickMatching c state $ allAdjacentCoords dim coords
    _ -> Nothing

neighbors :: Value.Value
neighbors = Value.Func $ \context cell ->
  case (C.identValue "$grid" context, C.identValue "$pos" context, cell) of
    (Just (Value.Grid _ _ dim _ state), Just (Value.Coord coords), Value.CellState c) ->
      pickMatching c state $ allSurroundingCoords dim coords
    _ -> Nothing

left :: Value.Value
left = Value.Func $ \context cell ->
  case (C.identValue "$grid" context, C.identValue "$pos" context, cell) of
    (Just (Value.Grid _ _ _ _ state), Just (Value.Coord coord), Value.CellState c) ->
      case (M.lookup (modX coord (\x -> x - 1)) state, C.identValue "$oob" context) of
        (Just c', _)                          -> Just $ toBoolean $ c' == c
        (Nothing, Just (Value.CellState oob)) -> Just $ toBoolean $ oob == c
        _                                     -> Nothing

    _ -> Nothing

right :: Value.Value
right = Value.Func $ \context cell ->
  case (C.identValue "$grid" context, C.identValue "$pos" context, cell) of
    (Just (Value.Grid _ _ _ _ state), Just (Value.Coord coord), Value.CellState c) ->
      case (M.lookup (modX coord (+ 1)) state, C.identValue "$oob" context) of
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
    (Just (Value.Grid _ (Value.Finite size) _ _ _), Value.Coord coord) ->
      let
        width = Value.width size
        height = Value.height size
        x = Value.getX coord
        y = Value.getY coord
      in
        Just $
          toBoolean $
            (x == 0 && (y == 0 || y == height-1)) || (x == width-1 && (y == 0 || y == height-1))
    _ -> Nothing

firstRepeatedGeneration :: Value.Value
firstRepeatedGeneration = Value.Func $ \context v -> go S.empty context v
  where
    go seen context grd = do
      ord <- Value.toOrd grd
      if S.member ord seen
      then
        pure grd
      else do
        next <- nextGeneration' context grd
        go (S.insert ord seen) context next

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
    go (Value.I n) y _ (Value.Grid ts (Value.Finite size) _ empty state) | y == n =
      Just $ setY n ts (size { Value.height = 1 }) empty state
    go (Value.I n) y ctx (Value.Grid ts (Value.Finite size) dim ec state) | y < n = do
      let myState = M.mapKeys (setY' y) state
      next <- nextGeneration' ctx $ setY 0 ts size ec state
      remainder <- go (Value.I n) (y + 1) ctx next
      case remainder of
        Value.Grid _ (Value.Finite remSize) _ _ remState -> Just $ Value.Grid ts (Value.Finite (remSize { Value.width=Value.width remSize + 1 })) Conway.TwoD ec $ M.union myState remState
        _ -> Nothing

    go _ _ _ _ = Nothing

    setY y ts size ec = Value.Grid ts (Value.Finite size) Conway.TwoD ec . M.mapKeys (setY' y)

    setY' y (Value.D1 x) = Value.D2 x y
    setY' y (Value.D2 x _) = Value.D2 x y
    setY' _ _ = error "Cannot convert dimensions above 2D to 2D"

nextGeneration :: Value.Value
nextGeneration = Value.Func nextGeneration'

nextGeneration' context grd@(Value.Grid ts@Conway.CellTransitions{..} bounds dim emptyCell state) =
  Just $ Value.Grid ts bounds dim emptyCell $ M.filter ((/= emptyCell) . Just) $ M.mapWithKey (transition (C.insert "$grid" (Type.Grid, grd) context)) searchSpace
    where
      searchSpace =
        case emptyCell of
          Just ec ->
            M.union state $ M.fromList $ concatMap (fmap (,ec) . around dim) $ M.keys state
          Nothing ->
            state

      transition ctx coords c =
        fromMaybe (otherwiseCell c otherwiseCellIs) $
          matching cases coords c $
          M.insert "$pos" (Type.Position, Value.Coord coords) ctx

      otherwiseCell _ (Conway.DefaultCell def) = Conway.ident def
      otherwiseCell c Conway.Unchanged = c

      matching [] _ _ _ = Nothing
      matching ((from, to, cond):cs) coords c ctx =
        if Conway.ident from == c && evaluatesToTrue ctx cond
        then Just $ Conway.ident to
        else matching cs coords c ctx

      around dim coord = filter (withinBounds bounds) $ justAround dim coord

        where
          withinBounds Value.Infinite _ = True
          withinBounds (Value.Finite Value.WidthHeight{width}) (Value.D1 x) = 0 <= x && x < width
          withinBounds (Value.Finite Value.WidthHeight{width, height}) (Value.D2 x y) =
            0 <= x && x < width && 0 <= y && y < height
          withinBounds bounds coord =
            error $ "Could not perform bounds check for position withing bounds " <> show coord <> ", " <> show bounds

          justAround Conway.OneD (Value.D1 x) = Value.D1 <$> [x-1..x+1]
          justAround Conway.TwoD (Value.D2 x y) = Value.D2 <$> [x-1..x+1] <*> [y-1..y+1]
          justAround Conway.ThreeD (Value.D3 x y z) = Value.D3 <$> [x-1..x+1] <*> [y-1..y+1] <*> [z-1..z+1]
          justAround Conway.FourD (Value.D4 x y z w) = Value.D4 <$> [x-1..x+1] <*> [y-1..y+1] <*> [z-1..z+1] <*> [w-1..w+1]
          justAround d c = error ("next generation: Got an out-of-dimension (" <> show d <> ") coordinate " <> show c)

nextGeneration' _ _ = Nothing

readingOrder :: Value.Value
readingOrder = Value.Func readingOrder'

readingOrder' :: C.Context -> Value.Value -> Maybe Value.Value
readingOrder' context ps =
  -- TODO generation_0 is not the best thing to use here since grids _can_ change
  case (C.identValue "$generation_0" context, ps) of
    (Just (Value.Grid _ (Value.Finite size) _ _ _), Value.Vs vs) ->
      Value.Vs <$> mapM (fmap (readingIndex (Value.width size)) . getPos) vs
    _ -> Nothing

  where
    readingIndex width (x, y) = Value.I (x + width * y)

countCells :: Value.Value
countCells = Value.Func $ \_ vs -> Just $ Value.Func $ \_ grd -> countCells' vs grd

traceRegisterValue :: Text -> Integer -> Value.Traces -> Value.Traces
traceRegisterValue r value Value.Traces{ registerValues=Just (Value.RegHistory rh), instructionPointers } =
  Value.Traces (Just $ Value.RegHistory $ M.alter (Just . (value :) . fromMaybe []) r rh) instructionPointers
traceRegisterValue _ _ ts = ts

data TerminalCondition
  = FallOffEnd
  | Cycle

runUntil :: TerminalCondition -> Value.Value
runUntil terminateWhen = Value.Func $ \context program ->
  case program of
    Value.Program (Program.IndexedInstructions p) (Value.Ip ip) (Value.Regs regs) traces ->
      run' p ip regs context traces
    _ -> Nothing

  where
    run' p ip regs context traces =
      case currentInstruction of
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
            then run' p (ip+1) regs context traces'
            else
              case op of
                Program.JumpAway v ->
                  case evalValue currentContext Nothing v of
                    Right (Value.I offset) -> run' p (ip + offset) regs context traces'
                    _ -> Nothing
                Program.Set dest v ->
                  case evalValue currentContext Nothing v of
                    Right (Value.I result) ->
                      run' p (ip + 1) (M.insert dest result regs) context $ traceRegisterValue dest result traces'
                    _ -> Nothing
                Program.DoNothing ->
                  run' p (ip + 1) regs context traces'
      where
        currentInstruction =
          case (M.lookup ip p, terminateWhen) of
            (Nothing, _)            -> Nothing
            (Just inst, FallOffEnd) -> Just inst
            (Just inst, Cycle) ->
              if S.member ip (C.instructionPointers traces)
              then Nothing
              else Just inst

        traces' =
          case terminateWhen of
            FallOffEnd -> traces
            Cycle -> traces{ C.instructionPointers=S.insert ip (C.instructionPointers traces) }

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
    (Value.Grid _ (Value.Finite size) _ _ state, Just buckets) ->
      let
        counts = go (M.elems state) $ M.fromList $ zip buckets (repeat 0)
      in
        Just $ Value.Vs $ fmap (\v -> Value.I (counts M.! v)) buckets
    _ -> Nothing

  where
    go values buckets =
      foldr (M.adjust (+ 1)) buckets values

countCells' _ _ = Nothing

getPos :: Value.Value -> Maybe (Integer, Integer)
getPos (Value.Coord (Value.D1 x)) = Just (x, 0)
getPos (Value.Coord (Value.D2 x y)) = Just (x, y)
getPos _ = Nothing

positions :: Value.Value
positions = Value.Func $ \_ cs -> Just $ Value.Func $ \_ grd ->
  case (cs, grd) of
    (Value.CellState c, Value.Grid _ _ _ emptyCell _) | Just c == emptyCell -> Nothing
    (Value.CellState c, Value.Grid _ _ _ _ state) -> filterCells Value.Coord c state
    _ -> Nothing

  where
    filterCells f c state =
      Just $
        Value.Vs $
        fmap f $
        M.keys $
        M.filter (== c) state

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
    stroll' p d (Turtle.TakeSteps 0 _:rest) = stroll' p d rest
    stroll' p d (Turtle.TakeSteps ss Nothing:rest) =
      Value.Pos p:stroll' (step d ss p) d (Turtle.TakeSteps (approachZero ss) Nothing:rest)
    stroll' p d (Turtle.TakeSteps ss (Just d'):rest) =
      Value.Pos p:stroll' (step d' ss p) d (Turtle.TakeSteps (approachZero ss) (Just d'):rest)

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
    stroll' p d (Turtle.TakeSteps ss Nothing:rest) = stroll' (steps d ss p) d rest
    stroll' p d (Turtle.TakeSteps ss (Just d'):rest) = stroll' (steps d' ss p) d rest

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
    ("sum",                list num --> num,                   makeFold 0 (+))
  , ("count",              list a   --> num,                   count)
  , ("unique",             list a --> list a,                  unique)
  , ("product",            list num --> num,                   makeFold 1 (*))
  , ("repeats",            list a --> list a,                  repeats)
  , ("true",               bool,                               Value.True)
  , ("false",              bool,                               Value.False)
  , ("first",              list a --> a,                       first)
  , ("dupe",               list a --> list a,                  dupe)
  , ("even",               num --> bool,                       even')
  , ("odd",                num --> bool,                       odd')
  , ("maximum",            list num --> num,                   maximum')
  , ("base_zero_index_of", a --> (list a --> num),             indexOf0)
  , ("manhattan_distance", pos --> (pos --> num),              manhattanDistance)
  , ("combinations",       num --> (list a --> list (list a)), combos)
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
      ("run",                    (prog --> prog,            runUntil FallOffEnd))
    , ("run_until_cycle",        (prog --> prog,            runUntil Cycle))
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
