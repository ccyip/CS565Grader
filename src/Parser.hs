{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parseHomework
    ) where

import Control.Monad
import Data.Maybe
import Data.Bifunctor
import Data.Char
import Data.Void
import Data.Text (Text)
import Data.List.Split (splitOn)
import Text.Megaparsec
import Text.Megaparsec.Char

import Definitions

type Parser = Parsec Void Text

parseHomework :: String -> Text -> Homework
parseHomework fname txt = case parse pHomework fname txt of
                            Left msg -> error $ errorBundlePretty msg
                            Right hw -> hw

pQuoted :: Parser String
pQuoted = char '"' *> someTill (anySingleBut '"') (char '"')

pOption :: Parser (String, String)
pOption = do
  string "- "
  key <- pQuoted
  space
  string ":"
  space
  value <- pQuoted
  eol
  return (key, value)

pHomework :: Parser Homework
pHomework = do
  name <- skipManyTill anySingle pStart
  opts <- many pOption
  space
  exercises <- many pExercise
  space
  eof
  return Homework { hwName = name
                  , hwPenalty = checkPenalty . read <$> lookup "penalty" opts
                  , hwComment = fromMaybe "" $ lookup "comment" opts
                  , hwAllowedModuleAxioms = readOptionList "allowed_module_axioms" opts
                  , hwAllowedAxioms = readOptionList "allowed_axioms" opts
                  , hwExercises = exercises
                  }
    where checkPenalty x
            | 0 < x && x <= 100 = x
            | otherwise = error "Illegal penalty"

readOptionList :: String -> [(String, String)] -> [String]
readOptionList key opts = maybe [] (splitOn ",") $ lookup key opts

pStart :: Parser String
pStart = string "----" *> space *> pQuoted <* eol

pExercise :: Parser Exercise
pExercise = do
  name <- string "*> " *> pQuoted <* eol
  points <- string "Possible points: " *> some digitChar <* eol
  space
  items <- some pNamedItem <|> (:[]) <$> pItem name
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
              , itDependencies = readOptionList "dependencies" opts
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
  string "Comment:"
  space
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
