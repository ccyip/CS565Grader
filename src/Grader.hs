{-# LANGUAGE NamedFieldPuns #-}

module Grader
    ( grade
    ) where

import Data.List
import Data.Maybe
import Control.Monad.State

import Definitions

data Env = Env
  { evHwName :: String
  , evPoints :: Int
  , evItems :: [(String, Maybe Bool)]
  , evItemName :: String
  , evPenalty :: Maybe Int
  , evAllowedModuleAxioms :: [String]
  , evAllowedAxioms :: [String]
  } deriving (Show)

data CheckersResult
  = Unknown
  | Pass
  | Fail String

type GradeState = State Env

grade :: Homework -> Feedback
grade Homework { hwName = name
               , hwPenalty
               , hwAllowedModuleAxioms
               , hwAllowedAxioms
               , hwExercises
               } =
  Feedback { fbName = name
           , fbTotalPoints = sum $ map efbTotalPoints efbs
           , fbPoints = sum $ map efbPoints efbs
           , fbExercises = efbs
           }
  where efbs = evalState (mapM gradeExercise hwExercises) env
        env = Env { evHwName = name
                  , evPoints = 0
                  , evItems = []
                  , evItemName = ""
                  , evPenalty = hwPenalty
                  , evAllowedModuleAxioms = hwAllowedModuleAxioms
                  , evAllowedAxioms = hwAllowedAxioms
                  }

gradeExercise :: Exercise -> GradeState ExerciseFeedback
gradeExercise Exercise { excName = name
                       , excPoints = points
                       , excItems = items
                       } = do
  modify $ \env -> env { evPoints = points }
  ifbs <- mapM gradeItem items
  return ExerciseFeedback { efbName = name
                          , efbTotalPoints = points
                          , efbPoints = getPoints ifbs
                          , efbItems = ifbs
                          }
    where getPoints xs@(x:_)
            | ifbName x == name = normalScore $ ifbScore x
            | all (== Just True) $ map ifbStatus xs = points
            | otherwise = 0
          normalScore Ungraded = 0
          normalScore (NScore n) = n
          normalScore (BScore b) = if b then points else 0

gradeItem :: Item -> GradeState ItemFeedback
gradeItem item@Item { itName = name
                    , itManualGrade = mg
                    } = do
  modify $ \env -> env { evItemName = name }
  env <- get
  let ifb = go env mg
  modify $ \env -> env { evItems = (name, ifbStatus ifb):evItems env }
  return ifb
    where go env Nothing = gradeItem_ env item
          go env (Just (ManualGrade { mgScore = Ungraded })) = gradeItem_ env item
          go env (Just (ManualGrade { mgScore, mgComment })) =
            ItemFeedback { ifbName = name
                         , ifbScore = mgScore
                         , ifbStatus = case mgScore of
                                         BScore b -> Just b
                                         NScore n -> Just $ n >= evPoints env
                         , ifbComment = mgComment
                         }

gradeItem_ :: Env -> Item -> ItemFeedback
gradeItem_ env Item { itName = name
                    , itCheckers = cks
                    , itDependencies = deps
                    } = go $ foldCheckers env cks
  where go (Fail msg) =
          ItemFeedback { ifbName = name
                       , ifbScore = BScore False
                       , ifbStatus = Just False
                       , ifbComment = msg
                       }
        go Unknown =
          ItemFeedback { ifbName = name
                       , ifbScore = Ungraded
                       , ifbStatus = Nothing
                       , ifbComment = ""
                       }
        go Pass = checkDeps (filterDeps (Just False)) (filterDeps Nothing)
        filterDeps y = filter (\x -> fromJust (lookup x (evItems env)) == y) deps
        checkDeps [] [] =
          ItemFeedback { ifbName = name
                       , ifbScore = BScore True
                       , ifbStatus = Just True
                       , ifbComment = ""
                       }
        checkDeps [] xs =
          ItemFeedback { ifbName = name
                       , ifbScore = Ungraded
                       , ifbStatus = Just False
                       , ifbComment = "Depends on ungraded exercises " ++ intercalate ", " xs
                       }
        checkDeps xs _ =
          ItemFeedback { ifbName = name
                       , ifbScore = BScore False
                       , ifbStatus = Just False
                       , ifbComment = "Depends on incorrect definitions " ++ intercalate ", " xs
                       }

foldCheckers :: Env -> [Checker] -> CheckersResult
foldCheckers env cks = go $ map (\x -> (ckType x, reportChecker env x)) cks
  where go [] = Unknown
        go ((CompleteChecker, Nothing):_) = Pass
        go ((CompleteChecker, Just msg):_) = Fail msg
        go ((SoundVerifier, Nothing):_) = Pass
        go ((SoundVerifier, Just _):xs) = go xs
        go ((SoundFalsifier, Nothing):xs) = go xs
        go ((SoundFalsifier, Just msg):_) = Fail msg

reportChecker :: Env -> Checker -> Maybe String
reportChecker _ Checker { ckFailure = Just msg } = Just msg
reportChecker env Checker { ckFailure = Nothing
                          , ckAssumptions = ass
                          } = go $ reportAxioms env ass
  where go [] = Nothing
        go xs
          | evHwName env ++ "." ++ evItemName env `elem` xs = Just "Admitted"
          | otherwise = Just $ "Used disallowed axioms " ++ intercalate ", " xs

reportAxioms :: Env -> [String] -> [String]
reportAxioms Env { evHwName = hwName
                 , evItems = items
                 , evItemName = name
                 , evAllowedModuleAxioms
                 , evAllowedAxioms
                 } = filter (not . allowed)
  where allowed a = any (allowedPrefix a) evAllowedModuleAxioms
          || a `elem` evAllowedAxioms
          || maybe False (\x -> isJust $ lookup x items) (stripPrefix (hwName ++ ".") a)
        allowedPrefix a prefix = (prefix ++ ".") `isPrefixOf` a
          && prefix ++ "." ++ name /= a
