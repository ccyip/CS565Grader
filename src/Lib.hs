{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib
    ( run
    ) where

import Control.Monad
import Data.Char
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List.Split (splitOn)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

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

run :: String -> IO ()
run f = do
  result <- parse pHomework f <$> TIO.readFile f
  case result of
    Left bundle -> error $ errorBundlePretty bundle
    Right hw -> putStrLn $ show hw

pQuoted :: Parser String
pQuoted = char '"' *> someTill (anySingleBut '"') (char '"')

pOption :: Parser (String, String)
pOption = string "- " >> (,) <$> pQuoted <* string " : " <*> pQuoted <* eol

pHomework :: Parser Homework
pHomework = do
  name <- skipManyTill anySingle pStart
  opts <- many pOption
  space
  exercises <- many pExercise
  space
  eof
  return Homework { hwName = name
                  , hwPenalty = read <$> lookup "penalty" opts
                  , hwExercises = exercises
                  }

pStart :: Parser String
pStart = string "----" *> space *> pQuoted <* eol

pExercise :: Parser Exercise
pExercise = do
  name <- string "*> " *> pQuoted <* eol
  points <- string "Possible points: " *> some digitChar <* eol
  space
  items <- some pNamedItem <|> (:[]) <$> pItem ""
  return Exercise { excName = name
                  , excPoints = read points
                  , excItems = items
                  }

pNamedItem :: Parser Item
pNamedItem = string "#> " *> pQuoted <* eol >>= pItem

pItem :: String -> Parser Item
pItem name = do
  manualGrade <- optional pManualGrade
  opts <- many pOption
  space
  let ty = case manualGrade of
        Just _ -> SoundFalsifier
        Nothing -> CompleteChecker
  checkers <- some pTypedChecker <|> (:[]) <$> pChecker ty
  return Item { itName = name
              , itDependencies = splitOn "," <$> lookup "dependencies" opts
              , itAllowedAxioms = splitOn "," <$> lookup "allowed_axioms" opts
              , itManualGrade = manualGrade
              , itCheckers = checkers
              }

pManualGrade :: Parser ManualGrade
pManualGrade = do
  string "Score: "
  score <- choice [ Ungraded <$ string "Ungraded"
                  , BScore True <$ string "true"
                  , BScore False <$ string "false"
                  , NScore . read <$> some digitChar
                  ]
  eol
  string "Comment: "
  comment <- choice [ "" <$ string "None"
                    , pQuoted
                    ]
  eol
  return ManualGrade { mgScore = score, mgComment = comment }

pTypedChecker :: Parser Checker
pTypedChecker = choice [ SoundVerifier <$ string "~Verifier:"
                       , SoundFalsifier <$ string "~Falsifier:"
                       ] <* eol >>= pChecker

pChecker :: CheckerType -> Parser Checker
pChecker ty = do
  skipMany
    $ notFollowedBy (string "Assumptions:" <|> string "Fail:" <|> pHeader)
    *> skipLine
  failure <- optional $ string "Fail:" *> space *> pQuoted
  skipMany
    $ notFollowedBy (string "Assumptions:" <|> pHeader)
    *> skipLine
  ass <- option [] pAssumptions
  return Checker { ckType = ty
                 , ckFailure = failure
                 , ckAssumptions = ass
                 }

pHeader :: Parser Text
pHeader = string "*>" <|> string "#>" <|> string "~"

pFailure :: Parser String
pFailure = string "Fail: " *> pQuoted <* eol

pAssumptions :: Parser [String]
pAssumptions = do
  string "Assumptions:"
  space
  skipMany pClosed
  concat <$> many pAxioms

pClosed :: Parser Text
pClosed = string "Closed under the global context" <* space

pAxioms :: Parser [String]
pAxioms = string "Axioms:" *> eol *> some pAxiom <* space <* skipMany pClosed

pAxiom :: Parser String
pAxiom = do
  notFollowedBy $ eol <|> string "Axioms:" <|> pClosed <|> pHeader
  name <- some $ satisfy (not . isSpace)
  space
  char ':'
  skipLine
  skipMany $ char ' ' *> skipLine
  return name

skipLine :: Parser ()
skipLine = void $ skipManyTill anySingle eol
