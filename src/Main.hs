module Main where
import System.Environment (getArgs)
import qualified AptParse as A
import qualified GenParse as G

main1 = do
    args <- getArgs  -- IO [String]
    doStuff (parseArgs args)
  where
    parseArgs = id
    doStuff _ = putStrLn "HELLO Creuna!"

main = G.main