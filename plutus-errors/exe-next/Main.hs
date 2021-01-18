{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Main where

import           Errors
import           Errors.TH.GenCodes
import           Data.Text.Prettyprint.Doc

-- | Executable to help developers by returning a currently-unused error code
main :: IO ()
main =  print $
        "An error code that is not currently in-use is:"
        <+> pretty (succ $ maximum $(genCodes allErrors))
