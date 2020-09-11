module Definitions where

data Homework = Homework
  { hwName :: String
  , hwPenalty :: Maybe Int
  , hwExercises :: [Exercise]
  } deriving (Show)

data Exercise = Exercise
  { excName :: String
  , excPoints :: Int
  , excItems :: [Item]
  } deriving (Show)

data Item = Item
  { itName :: String
  , itDependencies :: Maybe [String]
  , itAllowedAxioms :: Maybe [String]
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
