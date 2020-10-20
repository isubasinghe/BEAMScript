module Main where

import BSAST
import BSCodeGen.LLVM (compile)
import BSParser
import CLIParser
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as TLIO
import LLVM.Pretty (ppllvm)
import System.FilePath.Posix

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
