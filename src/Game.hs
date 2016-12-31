module Game ( 
    startGame,
    getTopic,
    topics
) where 

import Sentence ( Sentence(..), Topic(..), loadResult, saveResult, wrongLastAttempt, mkWrongSentence, mkCorrectSentence )
import System.Random.Shuffle ( shuffle' )
import System.Random ( getStdGen )
import System.Process ( system )
import Control.Exception ( catch, SomeException(..) )
import Control.Monad ( unless )
import Data.Time.Calendar ( diffDays )
import Data.Time.Clock ( getCurrentTime, utctDay )
import Data.Char ( toLower, isDigit, isAlpha )
import System.IO ( hFlush, stdout )

startGame :: IO ()
startGame = do
    name <- showGreeting
    mainLoop name
    return ()

showGreeting :: IO String
showGreeting = do
    putStrLn "Hello, dear student :) - Здраствуй, дорогой ученик"
    putStr "How can I call you? - Как мне тебя называть?\nYour name: "
    hFlush stdout
    getLine

mainLoop :: String -> IO ()
mainLoop name = do
    _ <- system "cls"
    putStrLn "On what topic Do you want to learn new sentences?\nНа какую тему вы хотите изучить новые предложения?"
    showTopics
    catch (do
               top <- getLine
               unless (top == ":q") (let topic = read top in 
                    do
                        dirtySentences <- loadResult $ getTopic topic
                        sentences <- shuffle dirtySentences
                        _ <- system "cls"
                        check (splitAt (length sentences - 10) sentences) $ getTopic topic
                )
          )
          (const $ mainLoop name :: SomeException -> IO ())

check :: ([Sentence], [Sentence]) -> Topic -> IO ()
check (arr, x:xs) topic = do
    sentence <- checkSentense x
    saveResult (arr ++ sentence:xs) topic
    check (sentence:arr, xs) topic
check (_, []) _ = return ()

checkSentense :: Sentence -> IO Sentence
checkSentense sentence = 
    case date sentence of
        Nothing -> func            
        Just mTime -> do
                time <- getCurrentTime
                if wrongLastAttempt sentence || diffDays (utctDay time) (utctDay mTime) > minOld
                then func
                else return sentence
    where func = do
              putStrLn "Переведите на английский:"
              putStrLn $ rus sentence
              str <- getLine
              let yours = words . map toLower . filter (\a -> isAlpha a || a == '\'') $ str
                  corr  = words . map toLower . filter (\a -> isAlpha a || a == '\'') $ eng sentence
              time <- getCurrentTime 
              if yours == corr
              then do
                  putStrLn "Well, it's right"
                  return $ mkCorrectSentence sentence time
              else do
                  putStrLn "Wrong"
                  putStrLn $ "Correct: " ++ eng sentence
                  return $ mkWrongSentence sentence time

          minOld = 7


shuffle :: [a] -> IO [a]
shuffle ar = do
    gen <- getStdGen 
    let arr = shuffle' ar (length ar) gen 
    return arr

showTopics :: IO ()
showTopics = let tops = map (\(a, b) -> show a ++ ". " ++ fromTopic b) $ zip [1..] topics in
                mapM_ putStrLn tops

getTopic :: Int -> Topic
getTopic i = let top = topics !! (i - 1)
                 res = Topic . map toLower . filter isAlpha . head . words . tail . dropWhile (/= '-') . fromTopic $ top
             in res

topics :: [Topic]
topics = [
              Topic "Начало - Beginning"
            , Topic "Таможня, граница - Customs, border"
            , Topic "Приветствие - Greeting"
            , Topic "Прощание - Parting"
            , Topic "Поздравления и пожелания - Congratulations and wishes"
            , Topic "Как начать разговор? - How to begin conversation?"
            , Topic "Знакомство - Acquaintance"
            , Topic "Взаимопонимание - Understanding"
            , Topic "Различные вопросы - Questions"
            , Topic "Выражение согласия - Agreement"
            , Topic "Несогласие, отказ - Disagreement"
            , Topic "Приглашение, предложение - Invitation, offer"
            , Topic "Благодарность - Gratitude"
            , Topic "Язык - Language"
            -- , Topic "Страна и национальность - Country and nationality"
            -- , Topic "Время - Time"
            -- , Topic "Погода - Weather"
            -- , Topic "Питание, рестораны, кафе - Nourishment, restaurant, cafe"
            -- , Topic "Здоровье, самочувствие - Health"
            -- , Topic "Семья, родственники - Family, relation"
            -- , Topic "Возраст, внешность - Age, appearance"
            -- , Topic "Профессия, работа - Profession and work"
            -- , Topic "Магазин, покупки - Store, shopping"
            -- , Topic "Деньги - Money"
            -- , Topic "Телефонный разговор - A telephone conversation"
            -- , Topic "Путешествие - Travelling"
            -- , Topic "Автомобиль - Car"
            -- , Topic "Гостиница - Hotel"
            -- , Topic "Прогулки по городу - Town"
         ]