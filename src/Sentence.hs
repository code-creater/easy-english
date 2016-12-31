module Sentence (
    Sentence(..),
    RussianSentence,
    EnglishTranslation,
    DateOfAttempt,
    Topic(..),
    loadResult,
    saveResult,
    newSentence,
    wrongLastAttempt,
    correctLastAttempt,
    mkCorrectSentence,
    mkWrongSentence
) where 

import Data.Time.Clock ( UTCTime(..) )
import Data.Char ( isSpace )
import Control.Monad ( forM_ )
import System.IO ( openFile, hGetContents, IOMode(..), hPrint, hFlush, hSetEncoding, utf8 )

data Sentence = Sentence {
                             rus            :: RussianSentence,
                             eng            :: EnglishTranslation,
                             date           :: Maybe DateOfAttempt,
                             lastAttempt    :: Attempt
                         }
    deriving (Read, Eq)

instance Show Sentence where
    show (Sentence a b c d) = "Sentence {rus = \"" ++ a ++ "\", eng = \"" ++ b ++ "\", date = " ++ show c ++ ", lastAttempt = " ++ show d ++"}"

type RussianSentence     = String
type EnglishTranslation  = String 
type DateOfAttempt       = UTCTime
data Attempt             = Correct | Wrong               deriving (Read, Show, Eq)

newtype Topic            = Topic { fromTopic :: String } deriving (Read, Show, Eq)

newSentence :: String -> String -> Sentence
newSentence rus eng = Sentence rus eng Nothing Wrong

saveResult :: [Sentence] -> Topic -> IO ()
saveResult arr (Topic str) = do
    file <- openFile ("./.dic/" ++ str) WriteMode
    hSetEncoding file utf8
    hPrint file arr
    forM_ [1..10] (\a -> seq a $ hFlush file)

loadResult :: Topic -> IO [Sentence]
loadResult (Topic x)= do
    file <- openFile ("./.dic/" ++ x) ReadMode
    hSetEncoding file utf8
    str <- hGetContents file
    if any (not . isSpace) str
    then return $ read str
    else return []

mkWrongSentence :: Sentence -> UTCTime -> Sentence
mkWrongSentence (Sentence a b _ _) time = Sentence a b (Just time) Wrong

mkCorrectSentence :: Sentence -> UTCTime -> Sentence
mkCorrectSentence (Sentence a b _ _) time = Sentence a b (Just time) Correct

wrongLastAttempt :: Sentence -> Bool
wrongLastAttempt s = lastAttempt s == Wrong

correctLastAttempt :: Sentence -> Bool
correctLastAttempt s = lastAttempt s == Correct