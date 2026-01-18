{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
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

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  TextIO.putStrLn "hsolv: ultra-fast symbolic math. Type :help for commands."
  repl

repl :: IO ()
repl = do
  TextIO.putStr "hsolv> "
  line <- TextIO.getLine
  if Text.all isSpace line
    then repl
    else case parseCommand line of
      Left err -> TextIO.putStrLn err >> repl
      Right CmdQuit -> TextIO.putStrLn "bye."
      Right cmd -> runCommand cmd >> repl

runCommand :: Command -> IO ()
runCommand CmdHelp =
  TextIO.putStrLn (docFallback "help")
runCommand (CmdDoc topic) =
  TextIO.putStrLn (docFallback topic)
runCommand (CmdSuggest prefix) = do
  let matches = filter (Text.isPrefixOf (Text.toLower prefix) . Text.toLower) suggestions
  TextIO.putStrLn (Text.intercalate "\n" matches)
runCommand (CmdSimplify expr) =
  TextIO.putStrLn (prettyExpr (simplifySome expr))
runCommand (CmdDiff var expr) =
  TextIO.putStrLn (prettyNum (simplifyNum (diff var expr)))
runCommand (CmdEval expr env) = do
  case evalSome env expr of
    Left err -> TextIO.putStrLn err
    Right value -> TextIO.putStrLn (renderValue value)
runCommand (CmdPretty expr) =
  TextIO.putStrLn (prettyExpr expr)
runCommand (CmdSolve var expr) =
  case solveQuadratic var expr of
    Left err -> TextIO.putStrLn err
    Right roots ->
      TextIO.putStrLn (Text.intercalate "\n" (map prettyNum roots))
runCommand CmdQuit =
  pure ()

docFallback :: Text -> Text
docFallback topic =
  case topicDoc topic of
    Just doc -> doc
    Nothing ->
      "Unknown topic. Try: " <> Text.intercalate ", " allTopics

parseCommand :: Text -> Either Text Command
parseCommand raw =
  let line = Text.strip raw
  in parseCommandLine (stripLeadingColon line)

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

renderValue :: Value -> Text
renderValue (VNum n) = Text.pack (show n)
renderValue (VBool b) = if b then "true" else "false"
