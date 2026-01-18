{-# LANGUAGE OverloadedStrings #-}

module HSolv.Doc
  ( topicDoc
  , allTopics
  , suggestions
  , commandList
  , functionList
  ) where

import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

topics :: Map Text Text
topics = Map.fromList
  [ ("help", "Commands: :help, :doc <topic>, :suggest <prefix>, simplify <expr>, diff <var> <expr>, solve <var> <expr>, eval <expr> with x=1 y=2, pretty <expr>, :quit. Rich TUI: cabal run hsolv-tui.")
  , ("syntax", "Numbers, variables, + - * / ^, parentheses. Functions: sin, cos, tan, exp, log, sqrt, abs. Constants: pi, e. Booleans: true, false, &&, ||, !, comparisons == != < <= > >=. Conditionals: if <bool> then <num> else <num>.")
  , ("diff", "diff <var> <expr> computes symbolic derivative and then simplifies the result.")
  , ("solve", "solve <var> <expr> solves expr = 0 for quadratic or linear polynomials in <var> (real roots only).")
  , ("eval", "eval <expr> with x=1 y=2 evaluates using Double. Missing variables are errors.")
  , ("simplify", "simplify <expr> applies algebraic/boolean simplifications (identity, neutral elements, constant folding).")
  , ("pretty", "pretty <expr> prints the canonical formatting used by the engine.")
  , ("performance", "Parser and simplifier are strict; use short variable names and avoid deep nesting for fastest throughput.")
  , ("types", "Expressions are typed at the type-level (numeric vs boolean) using GADTs and DataKinds.")
  ]

topicDoc :: Text -> Maybe Text
topicDoc key = Map.lookup key topics

allTopics :: [Text]
allTopics = sort (Map.keys topics)

commandList :: [Text]
commandList =
  [ ":help"
  , ":doc"
  , ":suggest"
  , ":quit"
  , "simplify"
  , "diff"
  , "solve"
  , "eval"
  , "pretty"
  ]

functionList :: [Text]
functionList =
  [ "sin"
  , "cos"
  , "tan"
  , "exp"
  , "log"
  , "sqrt"
  , "abs"
  , "pi"
  , "e"
  ]

suggestions :: [Text]
suggestions = sort (commandList <> functionList)
