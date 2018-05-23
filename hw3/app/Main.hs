module Main where

import           Language           (executeFromFile)
import           System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> executeFromFile file
        _      -> putStrLn "Wrong number of arguments"
