module Value
       ( Value(..)
       , WidthHeight(..)
       , GridBounds(..)
       , InstructionPointer(..)
       , Registers(..)
       , registersFrom
       , toOrd
       , identType
       , identValue
       , Context
       , add
       , fromList
       , insert
       , empty
       , merge
       , Traces(..)
       , RegisterHistory(..)
       , Coord(..)
       , getX
       , getY
       ) where

import qualified Conway.Ast as Conway
import qualified Turtle.Ast as Turtle
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Text hiding (concat, empty, foldr)
import           Prelude hiding (True, False)
import qualified Program.Ast as Prog
import qualified Type
import qualified Data.Set as S

data WidthHeight
  = WidthHeight { width :: Integer, height :: Integer }
  deriving (Show, Eq, Ord)

data GridBounds
  = Infinite
  | Finite WidthHeight
  deriving (Show, Eq, Ord)

newtype InstructionPointer
  = Ip Integer
  deriving (Show, Eq)

newtype Registers
  = Regs (M.Map Text Integer)
  deriving (Show, Eq)

newtype RegisterHistory
  = RegHistory (M.Map Text [Integer])
  deriving (Show, Eq)

registersFrom :: [(Text, Integer)] -> Registers
registersFrom = Regs . M.fromList

data Traces = Traces {
    registerValues :: Maybe RegisterHistory
  , instructionPointers :: S.Set Integer
  }

data Coord
  = D1 Integer
  | D2 Integer Integer
  | D3 Integer Integer Integer
  | D4 Integer Integer Integer Integer
  deriving (Show, Eq, Ord)

getX :: Coord -> Integer
getX (D1 v) = v
getX (D2 v _) = v
getX (D3 v _ _) = v
getX (D4 v _ _ _) = v

getY :: Coord -> Integer
getY (D1 _) = 0
getY (D2 _ v) = v
getY (D3 _ v _) = v
getY (D4 _ v _ _) = v

minMaxBy :: (a -> Integer) -> [a] -> (Integer, Integer)
minMaxBy f = foldr (\v (min', max') -> (min min' $ f v, max max' $ f v)) (0, 0)

d2CellOrEmpty :: Char -> (Integer, Integer) -> Conway.SolvableConwayDimensions -> M.Map Coord Char -> Char
d2CellOrEmpty emptyCell (x, y) Conway.TwoD = M.findWithDefault emptyCell $ D2 x y
d2CellOrEmpty emptyCell (x, 0) Conway.OneD = M.findWithDefault emptyCell $ D1 x
d2CellOrEmpty emptyCell _ Conway.OneD = const emptyCell
d2CellOrEmpty emptyCell (x, y) Conway.ThreeD = M.findWithDefault emptyCell $ D3 x y 0
d2CellOrEmpty emptyCell (x, y) Conway.FourD = M.findWithDefault emptyCell $ D4 x y 0 0

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
  | Coord Coord
  | Grid Conway.CellTransitions GridBounds Conway.SolvableConwayDimensions (Maybe Char) (M.Map Coord Char)
  | Register Text
  | Program Prog.IndexedProgram InstructionPointer Registers Traces
  | Direction Turtle.Direction
  | Turtle (Integer, Integer) Turtle.Direction [Turtle.Action]
  | ParsedLine (M.Map Text Value)

data OrdValue
  = OrdI Integer
  | OrdVs [OrdValue]
  | OrdTrue
  | OrdFalse
  | OrdCellState Char
  | OrdCoord Coord
  | OrdPos (Integer, Integer)
  | OrdGrid GridBounds Conway.SolvableConwayDimensions (Maybe Char) (M.Map Coord Char)
  | OrdInfiniteGrid Conway.SolvableConwayDimensions Char (M.Map Coord Char)
  | OrdRegister Text
  | OrdDirection Turtle.Direction
  | OrdTurtle (Integer, Integer) Turtle.Direction
  | OrdParsedLine (M.Map Text OrdValue)
  deriving (Ord, Eq)

toOrd :: Value -> Maybe OrdValue
toOrd (I v)            = Just $ OrdI v
toOrd (Vs vs)          = OrdVs <$> traverse toOrd vs
toOrd True             = Just OrdTrue
toOrd False            = Just OrdFalse
toOrd (CellState s)    = Just $ OrdCellState s
toOrd (Pos coords)     = Just $ OrdPos coords
toOrd (Coord coord)    = Just $ OrdCoord coord
toOrd (Grid _ bs dim ec st) = Just $ OrdGrid bs dim ec st
toOrd (Register name)  = Just $ OrdRegister name
toOrd (Direction d)    = Just $ OrdDirection d
toOrd (Turtle pos d _) = Just $ OrdTurtle pos d
toOrd (ParsedLine vs)  = OrdParsedLine <$> traverse toOrd vs
toOrd Program{}        = Nothing
toOrd (Fold _)         = Nothing
toOrd (Func _)         = Nothing
toOrd (StepsOfFold _)  = Nothing

instance Show Value where
  show (I v) = show v
  show (Vs vs) = "[" ++ L.intercalate "," (show <$> vs) ++ "]"
  show True = "true"
  show False = "false"
  show (Fold _) = "<function/fold>"
  show (StepsOfFold _) = "<function/fold_steps>"
  show (Func _) = "<function>"
  show (CellState c) = "{cell:" ++ [c] ++ "}"
  show (Pos (x, y)) = "{pos:x=" ++ show x ++ ",y=" ++ show y ++ "}"
  show (Coord (D1 x)) = "{pos:x=" ++ show x ++ "}"
  show (Coord (D2 x y)) = "{pos:x=" ++ show x ++ ",y=" ++ show y ++ "}"
  show (Coord (D3 x y z)) = "{pos:x=" ++ show x ++ ",y=" ++ show y ++ ",z=" ++ show z ++ "}"
  show (Coord (D4 x y z w)) = "{pos:x=" ++ show x ++ ",y=" ++ show y ++ ",z=" ++ show z ++ ",w=" ++ show w ++ "}"
  show (Turtle (x, y) d _) =
    "{turtle:x=" ++ show x ++ ",y=" ++ show y ++ ",dir=" ++ show d ++ "}"
  show (Register r) = "{" ++ show r ++ ":reg}"
  show (Grid _ _ dim cell state) = do
     let emptyCell =
           case cell of
             Just ec -> ec
             _ -> ' '
     let (minY, maxY) = minMaxBy getY $ M.keys state
     let (minX, maxX) = minMaxBy getX $ M.keys state
     y <- [minY..maxY]
     fmap (\x -> d2CellOrEmpty emptyCell (x, y) dim state) [minX..maxX] ++ "\n"
  show Program{} = "<program...>"
  show (Direction d) = show d
  show (ParsedLine vs) =
    "{" <> L.intercalate ", " ((\(k, v) -> unpack k <> "=" <> show v) <$> M.toList vs) <> "}"

type Context = M.Map Text (Type.Type, Value.Value)

identType :: Text -> Context -> Maybe Type.Type
identType ident ctx = fst <$> M.lookup ident ctx

identValue :: Text -> Context -> Maybe Value.Value
identValue ident ctx = snd <$> M.lookup ident ctx

add :: Context -> Context -> Context
add = M.union

empty :: Context
empty = M.empty

merge :: [Context] -> Context
merge = foldr add empty

fromList :: [(Text, (Type.Type, Value.Value))] -> Context
fromList = M.fromList

insert :: Text -> (Type.Type, Value.Value) -> Context -> Context
insert = M.insert
