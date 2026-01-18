{-# LANGUAGE OverloadedStrings #-}

module HSolv.Command
  ( Command(..)
  , parseCommand
  , runCommand
  , suggestMatches
  , docForInput
  ) where

import Data.Char (isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import HSolv.Diff
import HSolv.Doc
import HSolv.Eval
import HSolv.Expr
import HSolv.Parser
import HSolv.Pretty
import HSolv.Simplify
import HSolv.Solve

data Command
  = CmdHelp
  | CmdDoc Text
  | CmdSuggest Text
  | CmdSimplify SomeExpr
  | CmdDiff Text NumExpr
  | CmdEval SomeExpr (Map Text Double)
  | CmdPretty SomeExpr
  | CmdSolve Text NumExpr
  | CmdQuit

parseCommand :: Text -> Either Text Command
parseCommand raw =
  let line = Text.strip raw
  in if Text.null line
      then Left "empty input"
      else parseCommandLine (stripLeadingColon line)

runCommand :: Command -> Either Text [Text]
runCommand CmdHelp =
  Right [docFallback "help"]
runCommand (CmdDoc topic) =
  Right [docFallback topic]
runCommand (CmdSuggest prefix) =
  Right (suggestMatches prefix)
runCommand (CmdSimplify expr) =
  Right [prettyExpr (simplifySome expr)]
runCommand (CmdDiff var expr) =
  Right [prettyNum (simplifyNum (diff var expr))]
runCommand (CmdEval expr env) =
  case evalSome env expr of
    Left err -> Left err
    Right value -> Right [renderValue value]
runCommand (CmdPretty expr) =
  Right [prettyExpr expr]
runCommand (CmdSolve var expr) =
  case solveQuadratic var expr of
    Left err -> Left err
    Right roots -> Right (map prettyNum roots)
runCommand CmdQuit =
  Right ["bye."]

suggestMatches :: Text -> [Text]
suggestMatches prefix =
  let key = Text.toLower prefix
  in filter (Text.isPrefixOf key . Text.toLower) suggestions

docForInput :: Text -> Text
docForInput raw =
  let line = Text.strip raw
      token = fromMaybe line (Text.stripPrefix ":" line)
      word = Text.toLower (headOrEmpty (Text.words token))
  in case word of
      "" -> docFallback "help"
      "doc" -> case drop 1 (Text.words token) of
        (topic:_) -> docFallback topic
        [] -> docFallback "help"
      "help" -> docFallback "help"
      "simplify" -> docFallback "simplify"
      "diff" -> docFallback "diff"
      "solve" -> docFallback "solve"
      "eval" -> docFallback "eval"
      "pretty" -> docFallback "pretty"
      _ -> docFallback "syntax"

stripLeadingColon :: Text -> Text
stripLeadingColon line = fromMaybe line (Text.stripPrefix ":" line)

parseCommandLine :: Text -> Either Text Command
parseCommandLine line
  | Text.isPrefixOf "help" line = Right CmdHelp
  | Text.isPrefixOf "quit" line = Right CmdQuit
  | Text.isPrefixOf "exit" line = Right CmdQuit
  | Just rest <- Text.stripPrefix "doc" line =
      Right (CmdDoc (Text.strip rest))
  | Just rest <- Text.stripPrefix "suggest" line =
      Right (CmdSuggest (Text.strip rest))
  | Just rest <- Text.stripPrefix "simplify" line =
      CmdSimplify <$> parseSomeText (Text.strip rest)
  | Just rest <- Text.stripPrefix "pretty" line =
      CmdPretty <$> parseSomeText (Text.strip rest)
  | Just rest <- Text.stripPrefix "diff" line =
      parseDiff rest
  | Just rest <- Text.stripPrefix "solve" line =
      parseSolve rest
  | Just rest <- Text.stripPrefix "eval" line =
      parseEval rest
  | otherwise =
      CmdSimplify <$> parseSomeText line

parseDiff :: Text -> Either Text Command
parseDiff rest = do
  let trimmed = Text.strip rest
      (varWord, exprText) = Text.break isSpace trimmed
      exprBody = Text.strip exprText
  if Text.null varWord || Text.null exprBody
    then Left "diff usage: diff <var> <expr>"
    else do
      expr <- parseNumText exprBody
      Right (CmdDiff varWord expr)

parseEval :: Text -> Either Text Command
parseEval rest = do
  let trimmed = Text.strip rest
      (exprText, withPart) = Text.breakOn " with " trimmed
      exprBody = Text.strip exprText
      assignsText = Text.strip (Text.drop 6 withPart)
  expr <- parseSomeText exprBody
  assigns <- if Text.null assignsText
    then Right Map.empty
    else parseAssignments assignsText
  Right (CmdEval expr assigns)

parseSolve :: Text -> Either Text Command
parseSolve rest = do
  let trimmed = Text.strip rest
      (varWord, exprText) = Text.break isSpace trimmed
      exprBody = Text.strip exprText
  if Text.null varWord || Text.null exprBody
    then Left "solve usage: solve <var> <expr> (solves expr = 0)"
    else do
      expr <- parseNumText exprBody
      Right (CmdSolve varWord expr)

parseAssignments :: Text -> Either Text (Map Text Double)
parseAssignments input =
  let parts = Text.words input
      parsed = mapMaybe parseAssignment parts
  in if length parsed /= length parts
      then Left "assignments should look like x=1 y=2.5"
      else Right (Map.fromList parsed)

parseAssignment :: Text -> Maybe (Text, Double)
parseAssignment piece = do
  let (name, rest) = Text.breakOn "=" piece
  valueText <- Text.stripPrefix "=" rest
  if Text.null name || Text.null valueText
    then Nothing
    else case reads (Text.unpack valueText) of
      [(value, "")] -> Just (name, value)
      _ -> Nothing

docFallback :: Text -> Text
docFallback topic =
  case topicDoc topic of
    Just doc -> doc
    Nothing ->
      "Unknown topic. Try: " <> Text.intercalate ", " allTopics

renderValue :: Value -> Text
renderValue (VNum n) = Text.pack (show n)
renderValue (VBool b) = if b then "true" else "false"

headOrEmpty :: [Text] -> Text
headOrEmpty [] = ""
headOrEmpty (x:_) = x
