{-# LANGUAGE TemplateHaskell #-}
module Main where

import           ErrorCode
import           Errors
import           TH.GenCodes

-- | Executable to help developers by returning a currently-unused error code
main :: IO ()
main =  putStrLn $ "An error code that is not currently in-use is: "
                 ++ show (case maximum $(genCodes allErrors) of
                              E n -> n+ 1)
