{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StrictData #-}

module Main where

import BSCodeGen.LLVM (compileModule, compilePretty)
import BSParser (parseFile)
import CLIParser (CLIOpts (inFile), emitLLVM, execParser, opts)
import qualified Data.Text.Lazy.IO as TLIO
import LLVM.Pretty (ppllvm)
import System.FilePath.Posix (addExtension, splitExtension)

data CompMode = Pretty | BitCode
  deriving (Show, Eq)

compir :: CompMode -> FilePath -> IO ()
compir c f = do
  input <- readFile f
  let mod = compileModule <$> parseFile f input
  case mod of
    (Right mod') -> fn mod' (addExtension fileName ext)
    (Left e) -> putStrLn e
  where
    fileName = fst $ splitExtension f
    (fn, ext) = case c of
      Pretty -> (compilePretty, ".ll")
      _others -> error "not implemented yet" 

main :: IO [()]
main = do
  opts <- execParser opts
  let emitLLVMFlag = emitLLVM opts
  if emitLLVMFlag
    then mapM (compir Pretty) (inFile opts)
    else mapM (compir BitCode) (inFile opts)
