{-# LANGUAGE OverloadedStrings #-}

module Builtins
       ( identifiers
       , identType
       ) where

import Data.Text
import qualified Data.Map.Strict as M
import qualified Type as Type

identType :: Text -> Maybe Type.Type
identType ident = (\(t, _, _) -> t) <$> M.lookup ident identifiers 

data ListRelevance
  = Fold
  | StepsOfFold 
  | None

baseIdentifiers :: [(Text, (Type.Type, ListRelevance, ()))]
baseIdentifiers = 
  [
    ("sum",          ((Type.List Type.Number)   `Type.Arrow` Type.Number,     Fold, ()))
  , ("first_repeat", ((Type.List (Type.Var 'a')) `Type.Arrow` (Type.Var 'a'), None, ()))
  , ("true",         (Type.Boolean,                                           None, ()))
  , ("false",        (Type.Boolean,                                           None, ()))
  , ("and",          (Type.Boolean `Type.Arrow` Type.Boolean,                 None, ()))
  ]

identifiers :: M.Map Text (Type.Type, ListRelevance, ())
identifiers = M.fromList $ do
  o@(name, (t, list, v)) <- baseIdentifiers
  case (t, list) of
    (Type.Arrow iT oT, Fold) -> [o, (append name "*", (Type.Arrow iT (Type.List oT), StepsOfFold, v))]
    _    -> [o]
