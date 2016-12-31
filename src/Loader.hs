module Loader where

import System.IO ( IOMode(..), openFile, hGetContents, hGetLine, hSetEncoding, utf8 )
import Sentence ( Sentence(), newSentence, saveResult, Topic(..) )
import Data.Char ( toLower, isAlpha )

loadNewSentences :: FilePath -> IO ()
loadNewSentences file = do
    inp <- openFile file ReadMode
    hSetEncoding inp utf8
    topic <- Topic . map toLower . filter isAlpha . head . words . tail . dropWhile (/= '-') <$> hGetLine inp
    dirtyArr <- lines <$> hGetContents inp
    let arr = getSentences dirtyArr
    putStrLn "All done"
    saveResult arr topic

getSentences :: [String] -> [Sentence]
getSentences (a:b:c:xs) = newSentence a b : getSentences xs
getSentences _        = []