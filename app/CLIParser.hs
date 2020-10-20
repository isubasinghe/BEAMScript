module CLIParser (cliopts, CLIOpts (..), execParser, opts) where

import Options.Applicative

data CLIOpts = CLIOpts
  { inFile :: [String],
    outFile :: String,
    emitLLVM :: Bool,
    optimisationLevel :: Int
  }
  deriving (Show, Eq)

parseStringList :: Monad m => String -> m [String]
parseStringList = return . words

multiString :: Mod OptionFields [String] -> Parser [String]
multiString desc = concat <$> some single
  where
    single = option (str >>= parseStringList) desc

cliopts :: Parser CLIOpts
cliopts =
  CLIOpts
    <$> multiString
      ( long "in"
          <> metavar "INPUT_FILES"
          <> help "Files to be compiled"
      )
      <*> strOption
        ( long "out"
            <> short 'o'
            <> metavar "OUTPUT"
            <> help "output file"
        )
      <*> switch
        ( long "emit-llvm"
            <> showDefault
            <> help "Emit LLVM IR"
        )
      <*> option
        auto
        ( long "optimise"
            <> short 'O'
            <> help "How much optimisation"
            <> showDefault
            <> value 1
            <> metavar "INT"
        )

opts :: ParserInfo CLIOpts
opts =
  info
    (cliopts <**> helper)
    ( fullDesc
        <> progDesc "Print a greeting for TARGET"
        <> header "hello - a test for optparse-applicative"
    )
