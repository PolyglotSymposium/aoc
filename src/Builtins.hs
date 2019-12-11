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
import qualified Data.Set as S 

identType :: Text -> Maybe Type.Type
identType ident = fst <$> M.lookup ident identifiers 

identValue :: Text -> Maybe Value.Value
identValue ident = snd <$> M.lookup ident identifiers 

makeFold :: Integer -> (Integer -> Integer -> Integer) -> Value.Value
makeFold i f = Value.Fold (Value.I i, \v1 v2 ->
  case (v1, v2) of
    (Value.I v, Value.I acc) -> Just $ Value.I $ f v acc
    _                        -> Nothing)

firstRepeat :: Value.Value
firstRepeat = Value.Func $ \v -> Value.Vs <$> go S.empty v
  where
    go _ (Value.Vs []) = Just []
    go seen (Value.Vs (v:vs)) = do
      ord <- Value.toOrd v
      let acc = if S.member ord seen then [v] else []
      rest <- go (S.insert ord seen) (Value.Vs vs)
      pure $ acc ++ rest
    go _ _ = Nothing

baseIdentifiers :: [(Text, (Type.Type, Value.Value))]
baseIdentifiers = 
  [
    ("sum",          ((Type.List Type.Number)   `Type.Arrow` Type.Number,                 makeFold 0 (+)))
  , ("first_repeat", ((Type.List (Type.Var 'a')) `Type.Arrow` (Type.List (Type.Var 'a')), firstRepeat))
  , ("true",         (Type.Boolean,                                                       Value.True))
  , ("false",        (Type.Boolean,                                                       Value.False))
  ]

identifiers :: M.Map Text (Type.Type, Value.Value)
identifiers = M.fromList $ do
  o@(name, (t, v)) <- baseIdentifiers
  case (t, v) of
    (Type.Arrow iT oT, Value.Fold step) -> [o, (append name "*", (Type.Arrow iT (Type.List oT), Value.StepsOfFold step))]
    _    -> [o]
