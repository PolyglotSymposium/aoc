module Program.Backend
       ( Backend(..)
       ) where

import Data.Text

data Backend = Backend
  { genDeclare            :: Text -> Text
  , genInitialize         :: Text -> Integer -> Text
  , genBoilerplateBracket :: (Text, Text)
  , genPrintLnInt         :: Text -> Text
  , genWhile              :: Text -> Text -> Text
  , genIncrement          :: Text -> Text
  , genSwitch             :: Text -> Text -> Text
  , genCase               :: Text -> Text -> Text
  }
