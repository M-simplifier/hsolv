{-# LANGUAGE OverloadedStrings #-}

module HSolv.Parser
  ( parseNumText
  , parseBoolText
  , parseSomeText
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.ParserCombinators.ReadP
import HSolv.Expr

parseNumText :: Text -> Either Text NumExpr
parseNumText input = parseWith (skipSpaces *> parseNumExpr <* skipSpaces <* eof) input

parseBoolText :: Text -> Either Text BoolExpr
parseBoolText input = parseWith (skipSpaces *> parseBoolExpr <* skipSpaces <* eof) input

parseSomeText :: Text -> Either Text SomeExpr
parseSomeText input =
  case parseBoolText input of
    Right b -> Right (SomeExpr b)
    Left _ -> SomeExpr <$> parseNumText input

parseWith :: ReadP a -> Text -> Either Text a
parseWith parser input =
  case [result | (result, rest) <- readP_to_S parser (Text.unpack input), all isSpace rest] of
    (result:_) -> Right result
    [] -> Left ("parse error near: " <> Text.take 32 input)

parseNumExpr :: ReadP NumExpr
parseNumExpr = chainl1 parseNumTerm addOp

parseNumTerm :: ReadP NumExpr
parseNumTerm = chainl1 parseNumPow mulOp

parseNumPow :: ReadP NumExpr
parseNumPow = chainr1 parseNumUnary powOp

parseNumUnary :: ReadP NumExpr
parseNumUnary =
  (symbol "-" >> Neg <$> parseNumUnary)
    <++ parseNumAtom

parseNumAtom :: ReadP NumExpr
parseNumAtom =
  parseIf
    <++ parseFunc
    <++ parseConst
    <++ parseNumber
    <++ parseVar
    <++ parens parseNumExpr

parseIf :: ReadP NumExpr
parseIf = do
  _ <- symbol "if"
  cond <- parseBoolExpr
  _ <- symbol "then"
  t <- parseNumExpr
  _ <- symbol "else"
  f <- parseNumExpr
  pure (If cond t f)

parseFunc :: ReadP NumExpr
parseFunc = choice
  [ func "sin" Sin
  , func "cos" Cos
  , func "tan" Tan
  , func "exp" Exp
  , func "log" Log
  , func "sqrt" Sqrt
  , func "abs" Abs
  ]
  where
    func name ctor = do
      _ <- symbol name
      arg <- parens parseNumExpr
      pure (ctor arg)

parseConst :: ReadP NumExpr
parseConst = choice
  [ symbol "pi" >> pure (NumLit (toRational (pi :: Double)))
  , symbol "e" >> pure (NumLit (toRational (exp 1 :: Double)))
  ]

parseNumber :: ReadP NumExpr
parseNumber = do
  digits <- munch1 isDigit
  frac <- option Nothing (do _ <- char '.'; ds <- munch1 isDigit; pure (Just ds))
  let literal = case frac of
        Nothing -> fromInteger (read digits)
        Just ds -> toRational (read (digits <> "." <> ds) :: Double)
  pure (NumLit literal)

parseVar :: ReadP NumExpr
parseVar = do
  name <- identifier
  pure (Var (Text.pack name))

parseBoolExpr :: ReadP BoolExpr
parseBoolExpr = chainl1 parseBoolAnd orOp

parseBoolAnd :: ReadP BoolExpr
parseBoolAnd = chainl1 parseBoolNot andOp

parseBoolNot :: ReadP BoolExpr
parseBoolNot =
  (symbol "!" >> Not <$> parseBoolNot)
    <++ parseBoolAtom

parseBoolAtom :: ReadP BoolExpr
parseBoolAtom =
  parseBoolLit
    <++ parseComparison
    <++ parens parseBoolExpr

parseBoolLit :: ReadP BoolExpr
parseBoolLit =
  (symbol "true" >> pure (BoolLit True))
    <++ (symbol "false" >> pure (BoolLit False))

parseComparison :: ReadP BoolExpr
parseComparison = do
  a <- parseNumExpr
  op <- choice
    [ symbol "==" >> pure Eq
    , symbol "!=" >> pure (\x y -> Not (Eq x y))
    , symbol "<=" >> pure Le
    , symbol "<" >> pure Lt
    , symbol ">=" >> pure Ge
    , symbol ">" >> pure Gt
    ]
  b <- parseNumExpr
  pure (op a b)

addOp :: ReadP (NumExpr -> NumExpr -> NumExpr)
addOp =
  (symbol "+" >> pure Add)
    <++ (symbol "-" >> pure (\a b -> Add a (Neg b)))

mulOp :: ReadP (NumExpr -> NumExpr -> NumExpr)
mulOp =
  (symbol "*" >> pure Mul)
    <++ (symbol "/" >> pure (\a b -> Mul a (Pow b (NumLit (-1)))))

powOp :: ReadP (NumExpr -> NumExpr -> NumExpr)
powOp = symbol "^" >> pure Pow

orOp :: ReadP (BoolExpr -> BoolExpr -> BoolExpr)
orOp = symbol "||" >> pure Or

andOp :: ReadP (BoolExpr -> BoolExpr -> BoolExpr)
andOp = symbol "&&" >> pure And

parens :: ReadP a -> ReadP a
parens parser = between (symbol "(") (symbol ")") parser

symbol :: String -> ReadP String
symbol s = skipSpaces *> string s <* skipSpaces

identifier :: ReadP String
identifier = do
  first <- satisfy isAlpha
  rest <- munch (\c -> isAlphaNum c || c == '_')
  pure (first : rest)
