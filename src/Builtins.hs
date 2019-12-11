{-# LANGUAGE OverloadedStrings #-}

module Builtins
       ( identifiers
       , identType
       , identValue
       ) where

import Data.Text
import qualified Data.Map.Strict as M
import qualified Type as Type
import qualified Value as Value

identType :: Text -> Maybe Type.Type
identType ident = (\(t, _,_) -> t) <$> M.lookup ident identifiers 

identValue :: Text -> Maybe Value.Value
identValue ident = (\(_, _, v) -> v) <$> M.lookup ident identifiers 

data ListRelevance
  = Fold
  | StepsOfFold 
  | None

baseIdentifiers :: [(Text, (Type.Type, ListRelevance, Value.Value))]
baseIdentifiers = 
  [
    ("sum",          ((Type.List Type.Number)   `Type.Arrow` Type.Number,     Fold, Value.True))
  , ("first_repeat", ((Type.List (Type.Var 'a')) `Type.Arrow` (Type.Var 'a'), None, Value.True))
  , ("true",         (Type.Boolean,                                           None, Value.True))
  , ("false",        (Type.Boolean,                                           None, Value.False))
  ]

identifiers :: M.Map Text (Type.Type, ListRelevance, Value.Value)
identifiers = M.fromList $ do
  o@(name, (t, list, v)) <- baseIdentifiers
  case (t, list) of
    (Type.Arrow iT oT, Fold) -> [o, (append name "*", (Type.Arrow iT (Type.List oT), StepsOfFold, v))]
    _    -> [o]
