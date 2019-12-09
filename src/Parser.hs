module Parser
       ( parse
       ) where

import qualified ListParser as List

import Text.Megaparsec hiding (parse)

parse = runParser List.list
