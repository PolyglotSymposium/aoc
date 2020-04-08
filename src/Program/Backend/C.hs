{-# LANGUAGE OverloadedStrings #-}

module Program.Backend.C
       (
       ) where

import qualified Data.Text as T
import Program.Backend

backend =
  Backend
  { genDeclare     = \name -> T.concat ["int ", T.pack $ show name, ";"]
  , genInitialize  = \name v -> T.concat [T.pack $ show name, " = ", T.pack $ show v, ";"]
  , genBoilerplateBracket =
      (
        "#include<stdio.h>\n\nint main() {",
        "return 0;\n}"
      )
  , genPrintLnInt = \name -> T.concat ["printf(\"%d\n\", ", T.pack $ show name, ");"]
  , genWhile = \cond inner -> T.concat ["while (", T.pack $ show cond, ") {\n", inner, "\n}"]
  , genIncrement = \name -> T.concat [name, " += 1;"]
  , genSwitch = \val inner -> T.concat ["switch (", T.pack $ show val, ") {\n", inner, "\n}"]
  , genCase = \val behavior -> T.concat ["case ", T.pack $ show val, ": ", behavior, "break;"]
  }
