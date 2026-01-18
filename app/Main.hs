{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (isSpace)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import HSolv.Command

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
      Left err -> TextIO.putStrLn ("Error: " <> err) >> repl
      Right CmdQuit -> TextIO.putStrLn "bye."
      Right cmd -> do
        case runCommand cmd of
          Left err -> TextIO.putStrLn ("Error: " <> err)
          Right outputs -> mapM_ TextIO.putStrLn outputs
        repl
