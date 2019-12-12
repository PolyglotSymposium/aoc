{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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

baseIdentifiers :: [(Text, Type.Type, Value.Value)]
baseIdentifiers = 
  [
    ("sum",     list num --> num,  makeFold 0 (+))
  , ("product", list num --> num,  makeFold 1 (*))
  , ("repeats", list a --> list a, repeats)
  , ("true",    bool,              Value.True)
  , ("false",   bool,              Value.False)
  , ("first",   list a --> a,      first)
  , ("dupe",    list a --> list a, dupe)
  ]

identifiers :: M.Map Text (Type.Type, Value.Value)
identifiers = M.fromList $ do
  (name, t, v) <- baseIdentifiers
  case (t, v) of
    (Type.Arrow iT oT, Value.Fold step) -> [(name, (t, v)), (append name "*", (iT --> list oT, Value.StepsOfFold step))]
    _    -> [(name, (t, v))]
