module Parser
       ( parse
       , parseErrorPretty
       ) where

import qualified ListParser as List

import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Error (parseErrorPretty)

parse = runParser List.list
