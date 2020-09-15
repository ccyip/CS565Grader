{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib
    ( run
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad
import Data.Maybe
import Data.List
import System.FilePath
import System.Directory
import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Csv
import Data.List.Split (splitOn)
import Data.Time

import Text.Pretty.Simple (pPrint)

import Definitions
import Parser (parseHomework)
import Grader (grade)

buildDir = "build"
outputDir = "output"
inputDir = "input"
gradebookFile = "gradebook.csv"
auxDir = "aux"
hwDir = "hw"
miscFiles = ["index.html"]
idIdx = 0
lastNameIdx = 1
firstNameIdx = 2
scoreIdx = 3

usage :: IO ()
usage = do
  putStrLn "Usage:"
  error ""

message :: String -> IO ()
message x = putStrLn $ ">> " ++ x

run :: [String] -> IO ()
run (cmd:top:args) = run_ cmd top args
run _ = usage

run_ :: String -> FilePath -> [String] -> IO ()
run_ cmd top args = do
  gb <- loadGradebook top
  students <- loadStudentDirs top
  go cmd args $ buildDirMap gb students
    where go "prep" (hwFile:_) dirMap = runPrepare dirMap top hwFile
          go _ _ _ = usage

-- hw <- parseHomework f <$> TIO.readFile f
-- case hw of
--   Left msg -> error msg
--   Right hw -> feedback $ grade hw

loadGradebook :: FilePath -> IO [Vector String]
loadGradebook top = do
  gb <- decode NoHeader <$> BL.readFile (top </> inputDir </> gradebookFile)
  case gb of
    Left msg -> error msg
    Right gb -> return $ V.toList gb

loadStudentDirs :: FilePath -> IO [(LocalTime, String, String)]
loadStudentDirs top = do
  xs <- listDirectory $ top </> inputDir </> hwDir
  return $ mapMaybe go xs
    where go x = case splitOn " - " x of
                      _:name:time:_ -> Just (parseTime time, name, x)
                      _ -> Nothing
          parseTime = parseTimeOrError True defaultTimeLocale "%b %-e, %Y %-l:%M %P"

buildDirMap :: [Vector String] -> [(LocalTime, String, String)] -> [(String, String)]
buildDirMap gb students = mapMaybe go gb
  where go x = let id = x ! idIdx
                   firstName = x ! firstNameIdx
                   lastName = x ! lastNameIdx
                   cands = filter (\(_, s, _) -> s == firstName ++ " " ++ lastName) students
               in ((,) id) <$> pick cands
        pick [] = Nothing
        pick xs = case maximum xs of
                    (_, _, dir) -> Just dir

runPrepare :: [(String, String)] -> FilePath -> FilePath -> IO ()
runPrepare dirMap top hwFile = do
  message "Preparing..."
  mapM_ (\(id, dir) -> runPrepare_ id dir top hwFile) dirMap
  message "Finished preparing"

runPrepare_ :: String -> String -> FilePath -> FilePath -> IO ()
runPrepare_ id dir top hwFile = do
  message $ "Preparing for " ++ id
  createDirectoryIfMissing True tgtDirPath
  xs <- listDirectory auxDirPath
  message $ "Copying auxiliary files"
  mapM_ copyAux xs
  message $ "Copying homework file from " ++ srcDirPath
  copyFile (srcDirPath </> hwFile) (tgtDirPath </> hwFile)
    where tgtDirPath = top </> buildDir </> dropWhile (== '#') id
          srcDirPath = top </> inputDir </> hwDir </> dir
          auxDirPath = top </> inputDir </> auxDir
          copyAux x
            | "local" `isSuffixOf` takeBaseName x = do
                exists <- doesFileExist (auxDirPath </> x)
                if exists
                  then message ("Skipping local file " ++ x)
                  else copyAux_ x
            | otherwise = copyAux_ x
          copyAux_ x = copyFile (auxDirPath </> x) (tgtDirPath </> x)

feedback :: Feedback -> IO ()
feedback Feedback { fbName = name
                  , fbTotalPoints = total
                  , fbPoints = points
                  , fbFinal = final
                  , fbPenalty
                  , fbComment
                  , fbExercises = excs
                  } = do
  putStrLn $ "~~~ Feedback ~~~"
  putStrLn $ "Homework Name: " ++ name
  putStrLn $ "Percentage Score: " ++ show percentage ++ "%"
  putStrLn $ "Total Possible Points: " ++ show total
  putStrLn $ "Earned Points: " ++ show final
  putStrLn $ "Raw Points: " ++ show points
  maybe (return ()) (\pen -> putStrLn ("Applied Penalty: -" ++ show pen ++ "%")) fbPenalty
  putStrLn $ "Comment: " ++ if null fbComment then "None" else fbComment
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
