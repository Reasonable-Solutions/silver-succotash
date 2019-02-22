{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE FlexibleInstances #-}

module GenParse where

import Options.Generic

import           Options.Generic

data Hello w
  = Hello { name     :: w ::: String <?> "target name"
           , yey    :: w ::: Maybe Bool <?> "quiet mode"
           , wee :: w ::: Maybe Int <?> "n, how happy"
           } deriving (Generic)

instance ParseRecord (Hello Wrapped)
deriving instance Show (Hello Unwrapped)

main :: IO ()
main = do
  args <- unwrapRecord "hello program"
  greet (args :: Hello Unwrapped)

greet :: Hello Unwrapped -> IO ()
greet (Hello name Nothing (Just n)) = putStrLn $ "Hello, " <> name
greet (Hello name Nothing Nothing) = putStrLn $ "Hello, " <> name
greet (_) = putStrLn "" --quiet mode


