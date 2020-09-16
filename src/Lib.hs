{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib
    ( run
    ) where

import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad
import Data.Maybe
import Data.List
import Data.Bifunctor
import System.FilePath
import System.Directory
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Csv
import Data.List.Split (splitOn)
import Data.Time
import System.Process.Typed
import System.Exit
import System.IO

import Text.Pretty.Simple (pPrint)

import Definitions
import Parser (parseHomework)
import Grader (grade)

idIdx = 0
lastNameIdx = 1
firstNameIdx = 2
scoreIdx = 3

inputDir top = top </> "input"
buildDir top = top </> "build"
outputDir top = top </> "output"

gradebookFile top = inputDir top </> "gradebook.csv"
auxDir top = inputDir top </> "aux"
hwDir top =  inputDir top </> "hw"
miscFiles top = map (hwDir top </>) ["index.html"]

buildDirFromId top id = buildDir top </> dropWhile (== '#') id
logFileName = "log"
feedbackFileName = "feedback"
scoreFileName = "score"

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
          go "grade" _ dirMap = runGrade dirMap top
          go _ _ _ = usage

loadGradebook :: FilePath -> IO [Vector String]
loadGradebook top = do
  gb <- decode NoHeader <$> BL.readFile (gradebookFile top)
  case gb of
    Left msg -> error msg
    Right gb -> return $ V.toList gb

loadStudentDirs :: FilePath -> IO [(LocalTime, String, String)]
loadStudentDirs top = do
  xs <- listDirectory $ hwDir top
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
  forM_ dirMap $ \(id, dir) -> runPrepare_ id dir top hwFile
  message "All done!!"

runPrepare_ :: String -> String -> FilePath -> FilePath -> IO ()
runPrepare_ id dir top hwFile = do
  message ""
  message $ "Preparing for " ++ id
  createDirectoryIfMissing True tgtDirPath
  xs <- listDirectory auxDirPath
  message $ "Copying auxiliary files"
  mapM_ copyAux xs
  message $ "Copying homework file from " ++ srcDirPath
  copyFile (srcDirPath </> hwFile) (tgtDirPath </> hwFile)
    where tgtDirPath = buildDirFromId top id
          srcDirPath = hwDir top </> dir
          auxDirPath = auxDir top
          copyAux x
            | "local" `isSuffixOf` takeBaseName x = do
                exists <- doesFileExist (auxDirPath </> x)
                if exists
                  then message ("Skipping local file " ++ x)
                  else copyAux_ x
            | otherwise = copyAux_ x
          copyAux_ x = copyFile (auxDirPath </> x) (tgtDirPath </> x)

runGrade :: [(String, String)] -> FilePath -> IO ()
runGrade dirMap top = do
  message "Grading..."
  feedbacks <- forM dirMap $ \(id, _) -> runGrade_ id (buildDirFromId top id)
  let failedList = map fst $ filter (isNothing . snd) feedbacks
  let ungradedList = filter (not . null . snd) $ map (second (maybe [] getUngraded)) feedbacks
  message ""
  unless (null failedList) $ message "Compilation failed:" >> forM_ failedList putStrLn
  unless (null ungradedList) $ message "Need manual grading:" >> forM_ ungradedList printUngraded
  when (null failedList && null ungradedList) $ message "ALL DONE!!"
      where getUngraded fb = concatMap getUngradedItems (fbExercises fb)
            getUngradedItems exc = map ifbName
              $ filter (isNothing . ifbStatus) (efbItems exc)
            printUngraded (id, xs) = putStrLn $ id ++ ": " ++ intercalate ", " xs

runGrade_ :: String -> FilePath -> IO (String, Maybe Feedback)
runGrade_ id dir = do
  message ""
  message $ "Processing " ++ id ++ " in " ++ dir
  message "Making"
  (ecode, out) <- withCurrentDirectory dir $ readProcessStdout "make"
  case ecode of
    ExitSuccess -> do
      log <- loadLog (BL.toStrict out)
      fb <- gradeFromLog id dir logFile log
      return (id, Just fb)
    ExitFailure _ -> message "Compilation failed" >> return (id, Nothing)
    where loadLog out = do
            when ("----" `BS.isInfixOf` out)
              $ message "Writing log" >> BS.writeFile logFile out
            message "Loading log"
            TIO.readFile logFile
          logFile = dir </> logFileName

gradeFromLog :: String -> FilePath -> FilePath -> Text -> IO Feedback
gradeFromLog id dir logFile log = do
  message "Parsing"
  let hw = parseHomework logFile log
  message "Grading"
  let fb = grade hw
  message "Generating feedback"
  withFile feedbackFile WriteMode $ \h -> feedback (hPutStrLn h) fb
  message "Writing score"
  writeFile scoreFile $ show (fbFinalScore fb)
  return fb
    where feedbackFile = dir </> feedbackFileName
          scoreFile = dir </> scoreFileName

feedback :: (String -> IO ()) -> Feedback -> IO ()
feedback pp Feedback { fbName = name
                     , fbTotalPoints = total
                     , fbPoints = points
                     , fbRawScore = raw
                     , fbFinalScore = final
                     , fbPenalty
                     , fbComment
                     , fbExercises = excs
                     } = do
  pp $ "~~~ Feedback ~~~"
  pp $ "Homework Name: " ++ name
  pp $ "Final Score: " ++ showScore final
  pp ""
  pp $ "Total Possible Points: " ++ show total
  pp $ "Earned Points: " ++ show points
  pp $ "Raw Score: " ++ showScore raw
  maybe (return ()) (\pen -> pp ("Applied Penalty: -" ++ show pen ++ "%")) fbPenalty
  pp $ "Comment: " ++ if null fbComment then "None" else fbComment
  pp ""
  feedbackSummary pp excs
  pp ""
  pp $ "~~~ Detail ~~~"
  forM_ excs $ feedbackExercise pp
    where showScore x = let y = x * 100
                        in show (fromIntegral (numerator y) / fromIntegral (denominator y)) ++ "%"

feedbackSummary :: (String -> IO ()) -> [ExerciseFeedback] -> IO ()
feedbackSummary pp excs = do
  pp "~~~ Summary ~~~"
  if null wrongs
    then pp "All exercises are correct!"
    else pp "The following exercises are incorrect:" >> mapM_ pp wrongs
    where wrongs = map efbName $ filter (\e -> efbPoints e < efbTotalPoints e) excs

feedbackExercise :: (String -> IO ()) -> ExerciseFeedback -> IO ()
feedbackExercise pp ExerciseFeedback { efbName = name
                                     , efbTotalPoints = total
                                     , efbPoints = points
                                     , efbItems = items
                                     } = do
  pp $ "*> Exercise (" ++ name ++ ")"
  pp $ "Possible Points: " ++ show total
  pp $ "Earned Points: " ++ show points
  if ifbName (head items) == name
    then feedbackItem pp False $ head items
    else forM_ items $ feedbackItem pp True
  pp ""

feedbackItem :: (String -> IO ()) -> Bool -> ItemFeedback -> IO ()
feedbackItem pp b ItemFeedback { ifbName = name
                               , ifbStatus = status
                               , ifbComment = comment
                               } = do
  when b $ pp "" >> pp ("**> Sub-Exercise (" ++ name ++ ")")
  pp $ "Status: " ++ showStatus status
  pp $ "Comment: " ++ if null comment then "None" else comment
    where showStatus Nothing = "Ungraded"
          showStatus (Just True) = "Correct"
          showStatus (Just False) = "Incorrect"
