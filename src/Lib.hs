{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib
    ( run
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad

import Text.Pretty.Simple (pPrint)

import Definitions
import Parser
import Grader

run :: String -> IO ()
run f = do
  hw <- parseHomework f <$> TIO.readFile f
  case hw of
    Left msg -> error msg
    Right hw -> feedback $ grade hw

feedback :: Feedback -> IO ()
feedback Feedback { fbName = name
                  , fbTotalPoints = total
                  , fbPoints = points
                  , fbExercises = excs
                  } = do
  putStrLn $ "~~~ Feedback ~~~"
  putStrLn $ "Homework Name: " ++ name
  putStrLn $ "Total Points: " ++ show total
  putStrLn $ "Earned Points: " ++ show points
  putStrLn $ "Percentage Score: " ++ show percentage ++ "%"
  putStrLn ""
  feedbackSummary excs
  putStrLn ""
  putStrLn $ "~~~ Detail ~~~"
  mapM_ feedbackExercise excs
    where percentage = fromIntegral (points * 100) / fromIntegral total

feedbackSummary :: [ExerciseFeedback] -> IO ()
feedbackSummary excs = do
  putStrLn "~~~ Summary ~~~"
  if null wrongs
    then putStrLn "All exercises are correct!"
    else putStrLn "The following exercises are incorrect:" >> mapM_ putStrLn wrongs
    where wrongs = map efbName $ filter (\e -> efbPoints e < efbTotalPoints e) excs

feedbackExercise :: ExerciseFeedback -> IO ()
feedbackExercise ExerciseFeedback { efbName = name
                                  , efbTotalPoints = total
                                  , efbPoints = points
                                  , efbItems = items
                                  } = do
  putStrLn $ "~ Exercise (" ++ name ++ ")"
  putStrLn $ "Possible Points: " ++ show total
  putStrLn $ "Earned Points: " ++ show points
  if ifbName (head items) == name
    then feedbackItem False $ head items
    else mapM_ (feedbackItem True) items
  putStrLn ""

feedbackItem :: Bool -> ItemFeedback -> IO ()
feedbackItem b ItemFeedback { ifbName = name
                            , ifbStatus = status
                            , ifbComment = comment
                            } = do
  when b $ putStrLn "" >> putStrLn ("~~ Sub-Exercise (" ++ name ++ ")")
  putStrLn $ "Status: " ++ showStatus status
  putStrLn $ "Comment: " ++ if null comment then "None" else comment
    where showStatus Nothing = "Ungraded"
          showStatus (Just True) = "Correct"
          showStatus (Just False) = "Incorrect"
