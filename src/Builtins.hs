{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Builtins
       ( listContext
       , valueFromParsedLine
       , conwayContext
       , graphContext
       , programContext
       , turtleContext
       ) where

import qualified Ast
import qualified Conway.Ast as Conway
import           Control.Monad (replicateM, guard)
import qualified Turtle.Ast as Turtle
import qualified Data.Graph as G
import  Data.List (tails)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as S
import           Data.Text hiding (count, length, foldr, zip, maximum, concat, filter, concatMap, minimum, take, tails)
import qualified Data.Text as T
import           Evaluator (evalValue, toBoolean)
import qualified Program.Ast as Program
import qualified Type
import qualified Value as C
import qualified Value as V

makeFold :: Integer -> (Integer -> Integer -> Integer) -> V.Value
makeFold i f = V.Fold (V.I i, \v1 v2 ->
  case (v1, v2) of
    (V.I v, V.I acc) -> Just $ V.I $ f v acc
    _                        -> Nothing)

count :: V.Value
count = funcOfList $ Just . V.I . toInteger . length

repeats :: V.Value
repeats = funcOfList $ fmap V.Vs . go S.empty
  where
    go _ [] = Just []
    go seen (v:vs) = do
      ord  <- V.toOrd v
      rest <- go (S.insert ord seen) vs
      pure $ [v | S.member ord seen] ++ rest

unique :: V.Value
unique = funcOfList $ fmap V.Vs . go S.empty
  where
    go _ [] = Just []
    go seen (v:vs) = do
      ord  <- V.toOrd v
      rest <- go (S.insert ord seen) vs
      pure $ [v | not $ S.member ord seen] ++ rest

first :: V.Value
first = funcOfList listToMaybe

second :: V.Value
second = funcOfList $ \case
  (_:v:_) -> Just v
  _ -> Nothing

listFunctionOverText :: ([Text] -> V.Value) -> V.Value
listFunctionOverText f =
  funcOfList $ fmap f . extractText

  where
    extractText [] = Just []
    extractText (V.Txt i:rest) = do
      others <- extractText rest
      pure $ i:others
    extractText _ = Nothing

listFunctionOverIntegers :: ([Integer] -> V.Value) -> V.Value
listFunctionOverIntegers f =
  funcOfList $ fmap f . extractIntegers

  where
    extractIntegers [] = Just []
    extractIntegers (V.I i:rest) = do
      others <- extractIntegers rest
      pure $ i:others
    extractIntegers _ = Nothing

funcOfList :: ([V.Value] -> Maybe V.Value) -> V.Value
funcOfList f = V.Func $ \_ -> \case
                      V.Vs vs -> f vs
                      _ -> Nothing

funcOfNumber :: (Integer -> Maybe V.Value) -> V.Value
funcOfNumber f = V.Func $ \_ -> \case
                      V.I v -> f v
                      _ -> Nothing

funcOfChar :: (Char -> Maybe V.Value) -> V.Value
funcOfChar f = V.Func $ \_ -> \case
                      V.Ch c -> f c
                      _ -> Nothing

funcOfText :: (Text -> Maybe V.Value) -> V.Value
funcOfText f = V.Func $ \_ -> \case
                      V.Txt c -> f c
                      _ -> Nothing

funcOfGraph :: (G.EqualMapGraph Text -> Maybe V.Value) -> V.Value
funcOfGraph f = V.Func $ \_ -> \case
                      V.Graph c -> f c
                      _ -> Nothing

funcOfDijkstra :: ((G.Distances Text, G.Prevs Text) -> Maybe V.Value) -> V.Value
funcOfDijkstra f = V.Func $ \_ -> \case
                      V.DijkstraOutputs c -> f c
                      _ -> Nothing

reachableFrom :: V.Value
reachableFrom = funcOfText $ \edge -> Just $ funcOfGraph $ \g ->
  Just $ V.Vs $ fmap V.Txt $ S.toList $ (S.\\ S.singleton edge) $ S.map G.unVertex $ G.subGraph g $ G.Keyed edge

dijkstraFrom :: V.Value
dijkstraFrom = funcOfText $ \edge -> Just $ funcOfGraph $ \g ->
  Just $ V.DijkstraOutputs $ G.dijkstra g $ G.Keyed edge

rawDistances :: V.Value
rawDistances = funcOfDijkstra $ Just . V.Vs . fmap (V.I . fromIntegral) . M.elems . fst

distanceTo :: V.Value
distanceTo = funcOfText $ \edge -> Just $ funcOfDijkstra $ \(dists, _) ->
  Just $ V.I $ fromIntegral $ M.findWithDefault maxBound (G.Keyed edge) dists

topologicalOrder :: V.Value
topologicalOrder = funcOfGraph $ Just . V.Vs . fmap V.Txt . G.topologicalSort

concat' :: V.Value
concat' = listFunctionOverText $ V.Txt . T.concat

maximum' :: V.Value
maximum' = listFunctionOverIntegers $ V.I . maximum

minimum' :: V.Value
minimum' = listFunctionOverIntegers $ V.I . minimum

subtract' :: V.Value
subtract' =
  funcOfNumber $ \amt -> Just $ funcOfNumber $ Just . V.I . subtract amt

combos :: V.Value
combos = funcOfNumber $ \n -> Just $ funcOfList $ \items ->
  Just $ V.Vs $ V.Vs <$> replicateM (fromIntegral n) items

nwise :: V.Value
nwise = funcOfNumber $ \n -> Just $ funcOfList $ \items ->
  Just $ V.Vs $ do
    t <- tails items
    let nt = take (fromIntegral n) t
    guard $ length nt == fromIntegral n
    pure $ V.Vs nt

countChar :: V.Value
countChar = funcOfChar $ \c -> Just $ funcOfText $ \txt ->
  Just $ V.I $ fromIntegral $ T.count (T.singleton c) $ txt

charAt1 :: V.Value
charAt1 = funcOfNumber $ \i -> Just $ funcOfText $ \txt ->
  Just $ V.Ch $ T.index txt $ subtract 1 $ fromIntegral i

isInfixOf' :: V.Value
isInfixOf' = funcOfText $ \ifx -> Just $ funcOfText $ Just . toBoolean . T.isInfixOf ifx

indexOf0 :: V.Value
indexOf0 = V.Func $ \_ v -> Just $ funcOfList $ seekIndex 0 v

  where
    seekIndex _ _ [] = Nothing
    seekIndex idx v (v':_) | V.toOrd v =?= V.toOrd v' = Just $ V.I idx
    seekIndex idx v (_:rest) = seekIndex (idx+1) v rest

(=?=) :: Eq a => Maybe a -> Maybe a -> Bool
Nothing =?= _ = False
_ =?= Nothing = False
Just x =?= Just y = x == y

even' :: V.Value
even' = funcOfNumber $ Just . toBoolean . even

odd' :: V.Value
odd' = funcOfNumber $ Just . toBoolean . odd

dupe :: V.Value
dupe = funcOfList $ \vs -> Just (V.Vs (vs ++ vs))

valueFromParsedLine :: Text -> V.Value
valueFromParsedLine name =
  V.Func $ \_ -> \case
    V.ParsedLine values -> Just $ values M.! name
    _ -> Nothing

evaluatesToTrue :: V.Context -> Ast.Value -> Bool
evaluatesToTrue context ast =
  case evalValue context Nothing ast of
    Right V.True -> True
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

modX :: V.Coord -> (Integer -> Integer) -> V.Coord
modX (V.D1 x) f = V.D1 (f x)
modX (V.D2 x y) f = V.D2 (f x) y
modX (V.D3 x y z) f = V.D3 (f x) y z
modX (V.D4 x y z w) f = V.D4 (f x) y z w

allAdjacentCoords :: Conway.SolvableConwayDimensions -> V.Coord -> [V.Coord]
allAdjacentCoords Conway.OneD (V.D1 x) = V.D1 <$> [x-1, x+1]
allAdjacentCoords Conway.TwoD (V.D2 x y) = uncurry V.D2 <$> allAdjacent (x, y)
allAdjacentCoords Conway.ThreeD (V.D3 x y z) = upDown ++ zAdjacent
  where
    upDown = V.D3 x y <$> [z-1, z+1]
    zAdjacent = (\(x', y') -> V.D3 x' y' z) <$> allAdjacent (x, y)
allAdjacentCoords  d coord = error ("adjacent: Got an out-of-dimension (" <> show d <> ") coordinate " <> show coord)

allSurroundingCoords :: Conway.SolvableConwayDimensions -> V.Coord -> [V.Coord]
allSurroundingCoords Conway.OneD (V.D1 x) = V.D1 <$> [x-1, x+1]
allSurroundingCoords Conway.TwoD (V.D2 x y) = uncurry V.D2 <$> allSurrounding (x, y)
allSurroundingCoords Conway.ThreeD p@(V.D3 x y z) = filter (/= p) $ V.D3 <$> [x-1..x+1] <*> [y-1..y+1] <*> [z-1..z+1]
allSurroundingCoords Conway.FourD p@(V.D4 x y z w) = filter (/= p) $ V.D4 <$> [x-1..x+1] <*> [y-1..y+1] <*> [z-1..z+1] <*> [w-1..w+1]
allSurroundingCoords d coord = error ("surrounding: Got an out-of-dimension (" <> show d <> ") coordinate " <> show coord)

pickMatching :: (Ord k, Eq a) => a -> M.Map k a -> [k] -> Maybe V.Value
pickMatching c state =
  Just .
    V.I .
    toInteger .
    M.size .
    M.filter (== c) .
    M.restrictKeys state .
    S.fromList

adjacent :: V.Value
adjacent = V.Func $ \context cell ->
  case (C.identValue "$grid" context, C.identValue "$pos" context, cell) of
    (Just (V.Grid _ _ dim _ state), Just (V.Coord coords), V.CellState c) ->
      pickMatching c state $ allAdjacentCoords dim coords
    _ -> Nothing

neighbors :: V.Value
neighbors = V.Func $ \context cell ->
  case (C.identValue "$grid" context, C.identValue "$pos" context, cell) of
    (Just (V.Grid _ _ dim _ state), Just (V.Coord coords), V.CellState c) ->
      pickMatching c state $ allSurroundingCoords dim coords
    _ -> Nothing

left :: V.Value
left = V.Func $ \context cell ->
  case (C.identValue "$grid" context, C.identValue "$pos" context, cell) of
    (Just (V.Grid _ _ _ _ state), Just (V.Coord coord), V.CellState c) ->
      case (M.lookup (modX coord (\x -> x - 1)) state, C.identValue "$oob" context) of
        (Just c', _)                          -> Just $ toBoolean $ c' == c
        (Nothing, Just (V.CellState oob)) -> Just $ toBoolean $ oob == c
        _                                     -> Nothing

    _ -> Nothing

right :: V.Value
right = V.Func $ \context cell ->
  case (C.identValue "$grid" context, C.identValue "$pos" context, cell) of
    (Just (V.Grid _ _ _ _ state), Just (V.Coord coord), V.CellState c) ->
      case (M.lookup (modX coord (+ 1)) state, C.identValue "$oob" context) of
        (Just c', _)                          -> Just $ toBoolean $ c' == c
        (Nothing, Just (V.CellState oob)) -> Just $ toBoolean $ oob == c
        _                                     -> Nothing

    _ -> Nothing

at :: V.Value
at = V.Func $ \ctx arg ->
  case (C.identValue "$pos" ctx, arg) of
    (Just (V.Pos current), V.Pos desired) ->
      Just $ toBoolean $ current == desired
    (Just current, V.Func f) ->
      f ctx current
    _ -> Nothing

corner :: V.Value
corner = V.Func $ \ctx current ->
  case (C.identValue "$grid" ctx, current) of
    (Just (V.Grid _ (V.Finite size) _ _ _), V.Coord coord) ->
      let
        width = V.width size
        height = V.height size
        x = V.getX coord
        y = V.getY coord
      in
        Just $
          toBoolean $
            (x == 0 && (y == 0 || y == height-1)) || (x == width-1 && (y == 0 || y == height-1))
    _ -> Nothing

firstRepeatedGeneration :: V.Value
firstRepeatedGeneration = V.Func $ \context v -> go S.empty context v
  where
    go seen context grd = do
      ord <- V.toOrd grd
      if S.member ord seen
      then
        pure grd
      else do
        next <- nextGeneration' context grd
        go (S.insert ord seen) context next

afterTransitions :: V.Value
afterTransitions = V.Func $ \ctx n -> Just $ V.Func $ \_ grd -> go n ctx grd
  where
    go (V.I 0) _ grd           = Just grd
    go (V.I n) ctx grd | n > 0 = do
                     next <- nextGeneration' ctx grd
                     go (V.I (n - 1)) ctx next
    go _ _ _ = Nothing

to2dWithTransitions :: V.Value
to2dWithTransitions = V.Func $ \ctx n -> Just $ V.Func $ \_ grd -> go n 0 ctx grd
  where
    go (V.I n) y _ (V.Grid ts (V.Finite size) _ emptyCell state) | y == n =
      Just $ setY n ts (size { V.height = 1 }) emptyCell state
    go (V.I n) y ctx (V.Grid ts (V.Finite size) _ ec state) | y < n = do
      let myState = M.mapKeys (setY' y) state
      next <- nextGeneration' ctx $ setY 0 ts size ec state
      remainder <- go (V.I n) (y + 1) ctx next
      case remainder of
        V.Grid _ (V.Finite remSize) _ _ remState -> Just $ V.Grid ts (V.Finite (remSize { V.width=V.width remSize + 1 })) Conway.TwoD ec $ M.union myState remState
        _ -> Nothing

    go _ _ _ _ = Nothing

    setY y ts size ec = V.Grid ts (V.Finite size) Conway.TwoD ec . M.mapKeys (setY' y)

    setY' y (V.D1 x) = V.D2 x y
    setY' y (V.D2 x _) = V.D2 x y
    setY' _ _ = error "Cannot convert dimensions above 2D to 2D"

nextGeneration :: V.Value
nextGeneration = V.Func nextGeneration'

nextGeneration' :: V.Context -> V.Value -> Maybe V.Value
nextGeneration' context grd@(V.Grid ts@Conway.CellTransitions{..} bounds dim emptyCell state) =
  Just $ V.Grid ts bounds dim emptyCell $ M.filter ((/= emptyCell) . Just) $ M.mapWithKey (transition (C.insert "$grid" (Type.Grid, grd) context)) searchSpace
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
          M.insert "$pos" (Type.Position, V.Coord coords) ctx

      otherwiseCell _ (Conway.DefaultCell def) = Conway.ident def
      otherwiseCell c Conway.Unchanged = c

      matching [] _ _ _ = Nothing
      matching ((from, to, cond):cs) coords c ctx =
        if Conway.ident from == c && evaluatesToTrue ctx cond
        then Just $ Conway.ident to
        else matching cs coords c ctx

      around dimension = filter (withinBounds bounds) . justAround dimension

        where
          withinBounds V.Infinite _ = True
          withinBounds (V.Finite V.WidthHeight{width}) (V.D1 x) = 0 <= x && x < width
          withinBounds (V.Finite V.WidthHeight{width, height}) (V.D2 x y) =
            0 <= x && x < width && 0 <= y && y < height
          withinBounds boundaries coord =
            error $ "Could not perform bounds check for position withing bounds " <> show coord <> ", " <> show boundaries

          justAround Conway.OneD (V.D1 x) = V.D1 <$> [x-1..x+1]
          justAround Conway.TwoD (V.D2 x y) = V.D2 <$> [x-1..x+1] <*> [y-1..y+1]
          justAround Conway.ThreeD (V.D3 x y z) = V.D3 <$> [x-1..x+1] <*> [y-1..y+1] <*> [z-1..z+1]
          justAround Conway.FourD (V.D4 x y z w) = V.D4 <$> [x-1..x+1] <*> [y-1..y+1] <*> [z-1..z+1] <*> [w-1..w+1]
          justAround d c = error ("next generation: Got an out-of-dimension (" <> show d <> ") coordinate " <> show c)

nextGeneration' _ _ = Nothing

readingOrder :: V.Value
readingOrder = V.Func readingOrder'

readingOrder' :: C.Context -> V.Value -> Maybe V.Value
readingOrder' context ps =
  -- TODO generation_0 is not the best thing to use here since grids _can_ change
  case (C.identValue "$generation_0" context, ps) of
    (Just (V.Grid _ (V.Finite size) _ _ _), V.Vs vs) ->
      V.Vs <$> mapM (fmap (readingIndex (V.width size)) . getPos) vs
    _ -> Nothing

  where
    readingIndex width (x, y) = V.I (x + width * y)

countCells :: V.Value
countCells = V.Func $ \_ vs -> Just $ V.Func $ \_ grd -> countCells' vs grd

traceRegisterValue :: Text -> Integer -> V.Traces -> V.Traces
traceRegisterValue r value V.Traces{ registerValues=Just (V.RegHistory rh), instructionPointers } =
  V.Traces (Just $ V.RegHistory $ M.alter (Just . (value :) . fromMaybe []) r rh) instructionPointers
traceRegisterValue _ _ ts = ts

data TerminalCondition
  = FallOffEnd
  | Cycle

runUntil :: TerminalCondition -> V.Value
runUntil terminateWhen = V.Func $ \context program ->
  case program of
    V.Program (Program.IndexedInstructions p) (V.Ip ip) (V.Regs regs) traces ->
      run' p ip regs context traces
    _ -> Nothing

  where
    run' p ip regs context traces =
      case currentInstruction of
        Nothing -> Just $ V.Program (Program.IndexedInstructions p) (V.Ip ip) (V.Regs regs) traces
        Just Program.Instruction{..} ->
          let
            currentContext = foldr (\(name, value) -> C.insert name (Type.Number, V.I value)) context $ M.toList regs
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
                    Right (V.I offset) -> run' p (ip + offset) regs context traces'
                    _ -> Nothing
                Program.Set dest v ->
                  case evalValue currentContext Nothing v of
                    Right (V.I result) ->
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

register :: V.Value
register = V.Func $ \_ r -> Just $ V.Func $ \ _ program ->
  case (r, program) of
    (V.Register name, V.Program _ _ (V.Regs regs) _) -> Just $ V.I $ regs M.! name
    _ -> Nothing

incrementRegister :: V.Value
incrementRegister = V.Func $ \_ r -> Just $ V.Func $ \ _ program ->
  case (r, program) of
    (V.Register name, V.Program p ip (V.Regs regs) traces) ->
      let
        value = regs M.! name + 1
      in
      Just $ V.Program p ip (V.Regs $ M.insert name value regs) $ traceRegisterValue name value traces
    _ -> Nothing

registerValues :: V.Value
registerValues = V.Func $ \ _ program ->
  case program of
    (V.Program _ _ (V.Regs regs) _) ->
      Just $ V.Vs $ V.I . snd <$> M.toList regs
    _ -> Nothing

tracedRegisterValues :: V.Value
tracedRegisterValues = V.Func $ \ _ program ->
  case program of
    (V.Program _ _ _ V.Traces{registerValues=Just (V.RegHistory rh) }) ->
      Just $ V.Vs $ V.I <$> (M.toList rh >>= snd)
    _ -> Nothing

getCellState :: V.Value -> Maybe Char
getCellState (V.CellState c) = Just c
getCellState _ = Nothing

countCells' :: V.Value -> V.Value -> Maybe V.Value
countCells' (V.Vs vs) grd =
  case (grd, sequence $ getCellState <$> vs) of
    (V.Grid _ (V.Finite _) _ _ state, Just buckets) ->
      let
        counts = go (M.elems state) $ M.fromList $ zip buckets (repeat 0)
      in
        Just $ V.Vs $ fmap (\v -> V.I (counts M.! v)) buckets
    _ -> Nothing

  where
    go values buckets =
      foldr (M.adjust (+ 1)) buckets values

countCells' _ _ = Nothing

getPos :: V.Value -> Maybe (Integer, Integer)
getPos (V.Coord (V.D1 x)) = Just (x, 0)
getPos (V.Coord (V.D2 x y)) = Just (x, y)
getPos _ = Nothing

positions :: V.Value
positions = V.Func $ \_ cs -> Just $ V.Func $ \_ grd ->
  case (cs, grd) of
    (V.CellState c, V.Grid _ _ _ (Just emptyCell) _) | c == emptyCell -> Nothing
    (V.CellState c, V.Grid _ _ _ _ state) -> filterCells V.Coord c state
    _ -> Nothing

  where
    filterCells f c state =
      Just $
        V.Vs $
        fmap f $
        M.keys $
        M.filter (== c) state

face :: V.Value
face = V.Func $ \_ dir -> Just $ V.Func $ \_ trt ->
  case (dir, trt) of
    (V.Direction d, V.Turtle p _ actions ctx) ->
      Just $ V.Turtle p d actions ctx 
    _ ->
      Nothing

currentPosition :: V.Value -> Maybe (Integer, Integer)
currentPosition (V.Turtle p _ _ _) = Just p
currentPosition (V.Pos p) = Just p
currentPosition _ = Nothing

manhattanDistance :: V.Value
manhattanDistance = V.Func $ \_ source -> Just $ V.Func $ \_ v ->
  case (source, currentPosition v) of
    (V.Pos (x0, y0), Just (x1, y1)) ->
      Just $ V.I $ abs (x0 - x1) + abs (y0 - y1)
    _ ->
      Nothing

strollSplat :: V.Value
strollSplat = V.Func $ \ctx trt ->
  case trt of
    (V.Turtle p d actions ctx') ->
      Just $ V.Vs $ stroll' p d actions $ C.add ctx ctx'
    _ ->
      Nothing

  where
    stroll' p _ [] _ = [V.Pos p]
    stroll' p _ (Turtle.Face d:rest) ctx = stroll' p d rest ctx
    stroll' p d (Turtle.Turn hs:rest) ctx = stroll' p (turn d hs) rest ctx
    stroll' p d (Turtle.TakeSteps (Left 0) _:rest) ctx = stroll' p d rest ctx
    stroll' p d (Turtle.TakeSteps (Left ss) Nothing:rest) ctx =
      V.Pos p:stroll' (step d ss p) d (Turtle.TakeSteps (Left $ approachZero ss) Nothing:rest) ctx
    stroll' p d (Turtle.TakeSteps (Left ss) (Just d'):rest) ctx =
      V.Pos p:stroll' (step d' ss p) d (Turtle.TakeSteps (Left $ approachZero ss) (Just d'):rest) ctx
    stroll' p d (Turtle.TakeSteps (Right ast) d':rest) ctx =
      case evalValue ctx Nothing ast of
        Right (V.I n) ->
          stroll' p d (Turtle.TakeSteps (Left n) d':rest) ctx
        Left err ->
          error $ "TODO: " <> show err
        Right v ->
          error $ "TODO: " <> show v
    stroll' p d (Turtle.SetState name ast:rest) ctx =
      case evalValue ctx Nothing ast of
        Right v ->
          stroll' p d rest $ C.insert name (Type.Number, v) ctx
        Left err ->
          error $ "TODO: " <> show err

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

stroll :: V.Value
stroll = V.Func $ \ctx trt ->
  case trt of
    V.Turtle p d actions ctx' ->
      let (mkTurtle, ctx'') = stroll' p d actions $ C.add ctx ctx'
      in Just $ mkTurtle actions ctx''
    _ ->
      Nothing

  where
    stroll' p d [] ctx = (V.Turtle p d, ctx)
    stroll' p _ (Turtle.Face d:rest) ctx = stroll' p d rest ctx
    stroll' p d (Turtle.Turn hs:rest) ctx = stroll' p (turn d hs) rest ctx
    stroll' p d (Turtle.TakeSteps (Left ss) Nothing:rest) ctx = stroll' (steps d ss p) d rest ctx
    stroll' p d (Turtle.TakeSteps (Left ss) (Just d'):rest) ctx = stroll' (steps d' ss p) d rest ctx
    stroll' p d (Turtle.TakeSteps (Right ast) d':rest) ctx =
      case evalValue ctx Nothing ast of
        Right (V.I n) ->
          stroll' p d (Turtle.TakeSteps (Left n) d':rest) ctx
        Left err ->
          error $ "TODO: " <> show err <> ", " <> show ctx
        Right v ->
          error $ "TODO: " <> show v
    stroll' p d (Turtle.SetState name ast:rest) ctx =
      case evalValue ctx Nothing ast of
        Right v ->
          stroll' p d rest $ C.insert name (Type.Number, v) ctx
        Left err ->
          error $ "TODO: " <> show err

    steps Turtle.Up n (x, y) = (x, y+n)
    steps Turtle.Down n (x, y) = (x, y-n)
    steps Turtle.Left n (x, y) = (x-n, y)
    steps Turtle.Right n (x, y) = (x+n, y)

rawComponents :: V.Value
rawComponents = V.Func $ const $ \case
  V.Turtle (x, y) _ _ _ -> Just $ V.Vs [V.I x, V.I y]
  _ -> Nothing

(-->) :: Type.Type -> Type.Type -> Type.Type
i --> o = Type.Arrow i o

list :: Type.Type -> Type.Type
list = Type.List

a :: Type.Type
a = Type.Var 'a'

num :: Type.Type
num = Type.Number

text :: Type.Type
text = Type.Text

char :: Type.Type
char = Type.Char

bool :: Type.Type
bool = Type.Boolean

grid :: Type.Type
grid = Type.Grid

graph :: Type.Type
graph = Type.Graph

dijkstra :: Type.Type
dijkstra = Type.DijkstraOutputs

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

baseIdentifiers :: [(Text, Type.Type, V.Value)]
baseIdentifiers =
  [
    ("sum",                list num --> num,                   makeFold 0 (+))
  , ("count",              list a   --> num,                   count)
  , ("unique",             list a --> list a,                  unique)
  , ("product",            list num --> num,                   makeFold 1 (*))
  , ("repeats",            list a --> list a,                  repeats)
  , ("true",               bool,                               V.True)
  , ("false",              bool,                               V.False)
  , ("first",              list a --> a,                       first)
  , ("second",             list a --> a,                       second)
  , ("dupe",               list a --> list a,                  dupe)
  , ("even",               num --> bool,                       even')
  , ("odd",                num --> bool,                       odd')
  , ("maximum",            list num --> num,                   maximum')
  , ("minimum",            list num --> num,                   minimum')
  , ("subtract",           num --> (num --> num),              subtract')
  , ("base_zero_index_of", a --> (list a --> num),             indexOf0)
  , ("manhattan_distance", pos --> (pos --> num),              manhattanDistance)
  , ("combinations",       num --> (list a --> list (list a)), combos)
  , ("nwise",              num --> (list a --> list (list a)), nwise)
  , ("count_char",         char --> (text --> num),            countChar)
  , ("base_one_char_at",   num --> (text --> char),            charAt1)
  , ("is_infix_of",        text --> (text --> bool),           isInfixOf')
  , ("concat",             list text --> text,                 concat')
  ]

core :: C.Context
core = C.fromList $ do
  (name, t, v) <- baseIdentifiers
  case (t, v) of
    (Type.Arrow iT oT, V.Fold step) -> [(name, (t, v)), (append name "*", (iT --> list oT, V.StepsOfFold step))]
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

graphContext :: C.Context
graphContext =
  C.add core $
    C.fromList [
      ("reachable_from",    (text --> (graph --> list text), reachableFrom))
    , ("dijkstra_from",     (text --> dijkstra,              dijkstraFrom))
    , ("raw_distances",     (dijkstra --> list num,          rawDistances))
    , ("distance_to",       (text --> (dijkstra --> num),    distanceTo))
    , ("topological_order", (graph --> list text,            topologicalOrder))
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
    , ("origin",                  (pos,                               V.Pos (0, 0)))
    , ("up",                      (direction,                         V.Direction Turtle.Up))
    , ("north",                   (direction,                         V.Direction Turtle.Up))
    , ("down",                    (direction,                         V.Direction Turtle.Down))
    , ("south",                   (direction,                         V.Direction Turtle.Down))
    , ("left",                    (direction,                         V.Direction Turtle.Left))
    , ("west",                    (direction,                         V.Direction Turtle.Left))
    , ("right",                   (direction,                         V.Direction Turtle.Right))
    , ("east",                    (direction,                         V.Direction Turtle.Right))
    , ("stroll",                  (turtle --> turtle,                 stroll))
    , ("stroll*",                 (turtle --> list pos,               strollSplat))
    , ("raw_components",          (turtle --> list num,               rawComponents))
    , ("distance_from",           (pos --> (turtle -->  num),         manhattanDistance))
    ]
