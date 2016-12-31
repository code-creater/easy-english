module Main where

import Game ( startGame )
import System.Environment ( getArgs )
import Loader ( loadNewSentences )

main :: IO ()
main = do
    arg <- getArgs
    if null arg
    then startGame
    else mapM_ loadNewSentences arg

