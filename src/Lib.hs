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
import Data.Void
import Data.Maybe
import Data.List
import Data.Bifunctor
import System.FilePath
import System.Directory
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import qualified Data.Csv as Csv
import Data.List.Split (splitOn)
import Data.Time
import System.Process.Typed
import System.Exit
import System.Environment
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char

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

gradebookFileName = "gradebook.csv"
gradebookFile top = inputDir top </> gradebookFileName
auxDir top = inputDir top </> "aux"
hwDir top =  inputDir top </> "hw"

miscFileNames = ["index.html"]

feedbackDir top = outputDir top </> "feedback"

normalizeId = dropWhile (== '#')
buildDirFromId top id = buildDir top </> id
logFileName = "log"
feedbackFileName = "feedback"
scoreFileName = "score"

usage :: IO ()
usage = do
  prog <- getProgName
  putStrLn "Usage:"
  putStrLn $ prog ++ " prepare <workdir> <homework file>"
  putStrLn $ prog ++ " grade <workdir> [-f] [student IDs]"
  putStrLn $ prog ++ " publish <workdir>"
  exitFailure

message :: String -> IO ()
message x = putStrLn $ ">> " ++ x

run :: [String] -> IO ()
run ("parse":file:_) = runParse file
run ("one":dir:_) = do
  message "Grading..."
  xs <- (maybe [] getUngraded . snd) <$> runGrade_ False "unknown" dir
  unless (null xs) $ message "Need manual grading:" >> putStrLn (intercalate ", " xs)
run (cmd:top:args) = run_ cmd top args
run _ = usage

run_ :: String -> FilePath -> [String] -> IO ()
run_ cmd top args = do
  gb <- loadGradebook top
  students <- loadStudentDirs top
  go cmd args gb $ buildDirMap (tail gb) students
    where go "prepare" (hwFile:_) _ dirMap = runPrepare dirMap top hwFile
          go "grade" ("-f":xs) _ dirMap = runGrade True dirMap top xs
          go "grade" xs _ dirMap = runGrade False dirMap top xs
          go "publish" _ gb dirMap = runPublish dirMap top gb
          go _ _ _ _ = usage

loadGradebook :: FilePath -> IO [Vector String]
loadGradebook top = do
  gb <- Csv.decode Csv.NoHeader <$> BL.readFile (gradebookFile top)
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
          parseTime = parseTimeOrError True defaultTimeLocale "%b %-e, %Y %-l%M %P"

buildDirMap :: [Vector String] -> [(LocalTime, String, String)] -> [(String, String)]
buildDirMap gb students = mapMaybe go gb
  where go x = let id = normalizeId (x ! idIdx)
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
  b <- doesFileExist (tgtDirPath </> hwFile)
  if b
    then message "Skipping homework file"
    else message ("Copying homework file from " ++ srcDirPath)
         >> copyFile (srcDirPath </> hwFile) (tgtDirPath </> hwFile)
    where tgtDirPath = buildDirFromId top id
          srcDirPath = hwDir top </> dir
          auxDirPath = auxDir top
          copyAux x
            | "local" `isSuffixOf` takeBaseName x = do
                exists <- doesFileExist (tgtDirPath </> x)
                if exists
                  then message ("Skipping local file " ++ x)
                  else copyAux_ x
            | x == hwFile = error $ hwFile ++ " in aux directory"
            | otherwise = copyAux_ x
          copyAux_ x = copyFile (auxDirPath </> x) (tgtDirPath </> x)

getUngraded :: Feedback -> [String]
getUngraded fb = concatMap getUngradedItems (fbExercises fb)
  where getUngradedItems exc = map ifbName
              $ filter (isNothing . ifbStatus) (efbItems exc)

runGrade :: Bool -> [(String, String)] -> FilePath -> [String] -> IO ()
runGrade clean dirMap top xs = do
  message "Grading..."
  feedbacks <- forM (dirMap_ xs) $ \(id, _) -> runGrade_ clean id (buildDirFromId top id)
  let failedList = map fst $ filter (isNothing . snd) feedbacks
  let ungradedListById = map (second (maybe [] getUngraded)) feedbacks
  let itmNames = maybe [] (getItmNames . fromJust . snd) $ find (isJust . snd) feedbacks
  let ungradedListByItm = map (\itm -> (itm, getUngradedByItm itm ungradedListById)) itmNames
  let ungradedList = filter (not . null . snd) ungradedListByItm
  message ""
  unless (null failedList) $ message "Compilation failed:" >> forM_ failedList putStrLn
  unless (null ungradedList) $ message "Need manual grading:" >> forM_ ungradedList printUngraded
  when (null failedList && null ungradedList) $ message "ALL DONE!!"
      where printUngraded (k, xs) = putStrLn $ k ++ ": " ++ intercalate ", " xs
            getItmNames fb = concatMap (map ifbName . efbItems) $ fbExercises fb
            getUngradedByItm itm xs = map fst $ filter (elem itm . snd) xs
            dirMap_ [] = dirMap
            dirMap_ xs = filterDirMap dirMap xs
            filterDirMap dirMap xs
              | any (\x -> isNothing (lookup x dirMap)) xs = error "Some students do not exist"
              | otherwise = filter ((`elem` xs) . fst) dirMap

runGrade_ :: Bool -> String -> FilePath -> IO (String, Maybe Feedback)
runGrade_ clean id dir = do
  message ""
  message $ "Processing " ++ id ++ " in " ++ dir
  when clean $ message "Cleaning" >> withCurrentDirectory dir (runProcess_ (proc "make" ["clean"]))
  message "Making"
  (ecode, out) <- withCurrentDirectory dir $ readProcessStdout (proc "make" ["-s"])
  case ecode of
    ExitSuccess -> do
      log <- loadLog (BL.toStrict out)
      fb <- gradeFromLog dir logFile log
      return (id, Just fb)
    ExitFailure _ -> message "Compilation failed" >> return (id, Nothing)
    where loadLog out = do
            when ("----" `BS.isInfixOf` out)
              $ message "Writing log" >> BS.writeFile logFile out
            message "Loading log"
            TIO.readFile logFile
          logFile = dir </> logFileName

gradeFromLog :: FilePath -> FilePath -> Text -> IO Feedback
gradeFromLog dir logFile log = do
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

runParse :: FilePath -> IO ()
runParse file = do
  txt <- TIO.readFile file
  let hw = parseHomework file txt
  message "Homework"
  pPrint hw
  let fb = grade hw
  message "Feedback"
  pPrint fb
  feedback putStrLn fb

pScoreHeader :: Parsec Void String Int
pScoreHeader = do
  skipManyTill anySingle (char '<')
  string "Numeric MaxPoints:"
  score <- some digitChar
  skipManyTill anySingle (char '>')
  return (read score)

runPublish :: [(String, String)] -> FilePath -> [Vector String] -> IO ()
runPublish dirMap top gb = do
  message "Publishing..."
  createDirectoryIfMissing True feedbackDirPath
  forM_ dirMap copyFeedback
  message "Copying miscellaneous files"
  forM_ miscFileNames $ \x -> copyFile (hwDir top </> x) (feedbackDirPath </> x)
  message "Loading scores"
  scoreMap <- forM dirMap getScore
  message "Building gradebook"
  let gb' = (head gb) : map (updateStudent scoreMap) (tail gb)
  message "Writing gradebook"
  BL.writeFile (outputDir top </> gradebookFileName) $ Csv.encode gb'
  message "ALL DONE!!"
    where copyFeedback (id, dir) = do
            let srcDir = buildDirFromId top id
            let tgtDir = feedbackDirPath </> dir
            message $ "Copying feedback for " ++ id
            createDirectoryIfMissing False tgtDir
            copyFile (srcDir </> feedbackFileName) (tgtDir </> feedbackFileName ++ ".txt")
          getScore (id, dir) = do
            score <- read <$> readFile (buildDirFromId top id </> scoreFileName)
            return (id, score)
          updateStudent scoreMap student = case lookup (normalizeId (student ! idIdx)) scoreMap of
                                             Just s -> student // [(scoreIdx, showScore s)]
                                             Nothing -> student
          showScore :: Ratio Int -> String
          showScore score = show $ round (score * fromIntegral totalScore)
          totalScore = let scoreHeader = (head gb ! scoreIdx)
                       in case parse pScoreHeader "gradebook" scoreHeader of
                            Left msg -> error $ errorBundlePretty msg
                            Right s -> s
          feedbackDirPath = feedbackDir top

feedback :: (String -> IO ()) -> Feedback -> IO ()
feedback pp Feedback { fbName = name
                     , fbTotalPoints = total
                     , fbBonusPoints = bonus
                     , fbPoints = points
                     , fbRawScore = raw
                     , fbFinalScore = final
                     , fbPenalty
                     , fbComment
                     , fbBonus
                     , fbExercises = excs
                     } = do
  pp $ "~~~ Feedback ~~~"
  pp $ "Homework Name: " ++ name
  pp $ "Final Score: " ++ showScore final
  pp ""
  pp $ "Total Possible Points: " ++ show total
  when (bonus > 0) $ pp ("Bonus Points: " ++ show bonus)
  pp $ "Earned Points: " ++ show points
  pp $ "Raw Score: " ++ showScore raw
  maybe (return ()) (\pen -> pp ("Applied Penalty: -" ++ show pen ++ "%")) fbPenalty
  pp $ "Comment: " ++ if null fbComment then "None" else fbComment
  pp ""
  pp "~~~ Summary ~~~"
  when (not (null fbBonus)) $ pp ("Bonus exercises: " ++ intercalate ", " fbBonus) >> pp ""
  feedbackSummary pp excs
  pp ""
  pp $ "~~~ Detail ~~~"
  forM_ excs $ feedbackExercise pp
    where showScore x = let y = x * 100
                        in show (fromIntegral (numerator y) / fromIntegral (denominator y)) ++ "%"

feedbackSummary :: (String -> IO ()) -> [ExerciseFeedback] -> IO ()
feedbackSummary pp excs = do
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
