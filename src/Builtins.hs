{-# LANGUAGE OverloadedStrings #-}

module Builtins
       ( identifiers
       , identType
       ) where

import Data.Text
import qualified Data.Map.Strict as M
import qualified Type as Type

identType :: Text -> Maybe Type.Type
identType ident = fst <$> M.lookup ident identifiers 

identifiers :: M.Map Text (Type.Type, ())
identifiers = 
  M.fromList
    [
      ("sum", ((Type.List Type.Number) `Type.Arrow` Type.Number, ()))
    ]
