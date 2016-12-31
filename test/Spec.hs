module Main where

import Sentence
import Test.Hspec
import Data.Time.Clock
import Game

main :: IO ()
main = hspec $ do
    sentenceTest
    gameTest

sentenceTest :: SpecWith ()
sentenceTest = describe "sentenceTest" $ do
    sentenceReadTest
    sentenceShowTest

sentenceReadTest :: Spec
sentenceReadTest = it "sentenceReadTest" $ let sent = newSentence "Как вас зовут?" "What's your name?"
                                               str = "Sentence {rus = \"Как вас зовут?\", eng = \"What's your name?\", date = Nothing, lastAttempt = Wrong}"
                                           in read str `shouldBe` sent

sentenceShowTest :: Spec
sentenceShowTest = it "sentenceShowTest" $ let sent = newSentence "Как вас зовут?" "What's your name?"
                                               str = "Sentence {rus = \"Как вас зовут?\", eng = \"What's your name?\", date = Nothing, lastAttempt = Wrong}"
                                           in show sent `shouldBe` str

gameTest :: SpecWith ()
gameTest = describe "gameTest" $ do
    gameGetTopicTest

gameGetTopicTest :: Spec
gameGetTopicTest = it "gameGetTopicTest" $ let i = 1
                                               str = Topic "beginning"
                                           in getTopic i `shouldBe` str

