module Main where

import BSCodeGen.LLVM (compile)
import BSParser (parseFile)
import CLIParser (CLIOpts (inFile), execParser, opts)
import qualified Data.Text.Lazy.IO as TLIO
import LLVM.Pretty (ppllvm)
import System.FilePath.Posix (addExtension, splitExtension)

compir :: FilePath -> IO ()
compir f = do
  input <- readFile f
  let out = ppllvm <$> compile <$> (parseFile f input)
  case out of
    (Right p) -> TLIO.writeFile (addExtension (fst $ splitExtension f) ".ll") p
    (Left e) -> putStrLn e

main = do
  opts <- execParser opts
  sequence $ map (compir) (inFile opts)
