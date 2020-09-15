module Definitions where

import Data.Ratio

data Homework = Homework
  { hwName :: String
  , hwPenalty :: Maybe Int
  , hwComment :: String
  , hwAllowedModuleAxioms :: [String]
  , hwAllowedAxioms :: [String]
  , hwExercises :: [Exercise]
  } deriving (Show)

data Exercise = Exercise
  { excName :: String
  , excPoints :: Int
  , excItems :: [Item]
  } deriving (Show)

data Item = Item
  { itName :: String
  , itDependencies :: [String]
  , itManualGrade :: Maybe ManualGrade
  , itCheckers :: [Checker]
  } deriving (Show)

data ManualGrade = ManualGrade
  { mgScore :: Score
  , mgComment :: String
  } deriving (Show)

data Score = Ungraded | NScore Int | BScore Bool
  deriving (Show)

data Checker = Checker
  { ckType :: CheckerType
  , ckFailure :: Maybe String
  , ckAssumptions :: [String]
  } deriving (Show)

data CheckerType = CompleteChecker | SoundVerifier | SoundFalsifier
  deriving (Show)

data Feedback = Feedback
  { fbName :: String
  , fbTotalPoints :: Int
  , fbPoints :: Int
  , fbRawScore :: Ratio Int
  , fbFinalScore :: Ratio Int
  , fbPenalty :: Maybe Int
  , fbComment :: String
  , fbExercises :: [ExerciseFeedback]
  } deriving (Show)

data ExerciseFeedback = ExerciseFeedback
  { efbName :: String
  , efbTotalPoints :: Int
  , efbPoints :: Int
  , efbItems :: [ItemFeedback]
  } deriving (Show)

data ItemFeedback = ItemFeedback
  { ifbName :: String
  , ifbScore :: Score
  , ifbStatus :: Maybe Bool
  , ifbComment :: String
  } deriving (Show)
