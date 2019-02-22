{-# LANGUAGE ApplicativeDo #-}
module AptParse where

import Options.Applicative

data Hello = Hello
  { name :: String
  , yey :: Bool
  , wee :: Int
  } deriving (Eq, Show)

{- |
    A Parser for Things
    is a function from Strings
    to Lists of Pairs
    of Things and Strings!

`Parser a = String -> [(a, String)]
-}


helloParser :: Parser Hello
helloParser = Hello
  <$> strOption (long "name" <> metavar "NAME" <> help "This is your name")
  <*> switch (long "quiet" <> short 'q' <> help "quiet mode")
  <*> option auto (long "weee" <> help "how you much hello" <> showDefault <> value 1 <> metavar "INT")

-- | Rewrite with applicative do notation
helloParser' :: Parser Hello
helloParser' = do
   name <- strOption (long "name" <> metavar "NAME" <> help "This is your name")

   quiet <- switch (long "quiet" <> short 'q' <> help "quiet mode")

   happy <- option auto (long "weee" <> help "how you much hello" <> showDefault <> value 1 <> metavar "INT")
   pure $ Hello name quiet happy

main = greet =<< execParser opts
  where
    opts = info (helloParser' <**> helper)
      ( fullDesc
     <> progDesc "print hello for some name"
     <> header "hello - this is a more cool cli" )

greet :: Hello -> IO ()
greet (Hello h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
greet _ = return ()
