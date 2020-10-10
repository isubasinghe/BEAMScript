module Main where

import System.Environment
import BSParser
import BSLexer
import BSAST

main = do
    args <- getArgs
    let sourceFile = args !! 0
    source <- readFile sourceFile
    let x = show $ parseFile sourceFile source
    putStrLn x

