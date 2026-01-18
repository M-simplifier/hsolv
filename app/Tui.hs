{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Edit
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as V
import HSolv.Command
import Lens.Micro (Lens')

data Name = InputEditor deriving (Eq, Ord, Show)

data AppState = AppState
  { stEditor :: Editor Text Name
  , stOutput :: [Text]
  , stSuggestions :: [Text]
  , stDoc :: Text
  , stHistory :: [Text]
  , stHistoryIndex :: Int
  }

main :: IO ()
main = do
  let initial = AppState
        { stEditor = editor InputEditor (Just 1) ""
        , stOutput = ["hsolv TUI: Enter=run  Tab=complete  Up/Down=history  Esc/Ctrl-C=quit"]
        , stSuggestions = []
        , stDoc = docForInput ""
        , stHistory = []
        , stHistoryIndex = 0
        }
  void (defaultMain app initial)

app :: App AppState e Name
app = App
  { appDraw = drawUI
  , appHandleEvent = handleEvent
  , appStartEvent = pure ()
  , appAttrMap = const theMap
  , appChooseCursor = showFirstCursor
  }

theMap :: AttrMap
theMap = attrMap baseAttr
  [ (editAttr, rgb 230 233 240 `on` rgb 22 24 30)
  , (editFocusedAttr, rgb 230 233 240 `on` rgb 26 30 40)
  , (attrTitle, V.withStyle (rgb 138 180 255 `on` rgb 17 19 24) V.bold)
  , (attrOutput, rgb 230 233 240 `on` rgb 17 19 24)
  , (attrOutputText, rgb 230 233 240 `on` rgb 17 19 24)
  , (attrPrompt, V.withStyle (rgb 120 214 198 `on` rgb 17 19 24) V.bold)
  , (attrError, V.withStyle (rgb 255 120 120 `on` rgb 24 16 16) V.bold)
  , (attrSidebar, rgb 200 205 220 `on` rgb 13 15 20)
  , (attrSuggestion, rgb 120 214 198 `on` rgb 13 15 20)
  , (attrHint, rgb 150 155 170 `on` rgb 17 19 24)
  , (attrMuted, rgb 110 115 130 `on` rgb 13 15 20)
  , (attrInputBorder, rgb 138 180 255 `on` rgb 17 19 24)
  ]
  where
    baseAttr = rgb 230 233 240 `on` rgb 17 19 24

rgb :: Int -> Int -> Int -> V.Color
rgb = V.rgbColor

attrTitle, attrOutput, attrSidebar, attrSuggestion, attrHint, attrMuted, attrInputBorder :: AttrName
attrOutputText, attrError, attrPrompt :: AttrName
attrTitle = attrName "title"
attrOutput = attrName "output"
attrSidebar = attrName "sidebar"
attrSuggestion = attrName "suggestion"
attrHint = attrName "hint"
attrMuted = attrName "muted"
attrInputBorder = attrName "input-border"
attrOutputText = attrName "output-text"
attrError = attrName "error"
attrPrompt = attrName "prompt"

drawUI :: AppState -> [Widget Name]
drawUI st =
  [ withBorderStyle unicodeRounded $
    vBox
      [ vLimitPercent 80 $
          hBox
            [ hLimitPercent 70 $
                withAttr attrOutput $
                  borderWithLabel (withAttr attrTitle (str "Output")) (renderOutput st)
            , vBox
                [ vLimit suggestionsHeight $
                    withAttr attrSidebar $
                      borderWithLabel (withAttr attrTitle (str "Suggestions")) (renderSuggestions st)
                , vLimit docHeight $
                    withAttr attrSidebar $
                      borderWithLabel (withAttr attrTitle (str "Doc")) (padAll 1 (txtWrap (stDoc st)))
                , fill ' '
                ]
            ]
      , vLimit 3 $
          withAttr attrInputBorder $
            borderWithLabel (withAttr attrTitle (str "Input")) (padAll 0 (renderEditor (txt . Text.unlines) True (stEditor st)))
      , withAttr attrHint $
          padLeftRight 1 (txt "Enter=run  Tab=complete  Up/Down=history  Esc/Ctrl-C=quit")
      ]
  ]

suggestionsHeight :: Int
suggestionsHeight = 10

docHeight :: Int
docHeight = 12

renderOutput :: AppState -> Widget Name
renderOutput st =
  let items = take 200 (stOutput st)
      lines' = reverse items
  in padAll 1 (vBox (map renderOutputLine lines'))

renderSuggestions :: AppState -> Widget Name
renderSuggestions st =
  let items = take 8 (stSuggestions st)
      rows = if null items then ["(no matches)"] else items
      renderRow row = withAttr attrSuggestion (txt row)
  in padAll 1 (vBox (map renderRow rows <> [withAttr attrMuted (fill ' ')]))

renderOutputLine :: Text -> Widget Name
renderOutputLine line
  | "Error:" `Text.isPrefixOf` line = withAttr attrError (txtWrap line)
  | ">> " `Text.isPrefixOf` line = withAttr attrPrompt (txtWrap line)
  | otherwise = withAttr attrOutputText (txtWrap line)

handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- get
  let input = Text.strip (Text.intercalate "\n" (getEditContents (stEditor st)))
  if Text.null input
    then pure ()
    else case parseCommand input of
      Right CmdQuit -> halt
      Left err -> put (appendOutput st ("Error: " <> err) input)
      Right cmd ->
        case runCommand cmd of
          Left err -> put (appendOutput st ("Error: " <> err) input)
          Right outputs -> put (appendOutputs st outputs input)
  st' <- get
  put (refreshInput st')
handleEvent (VtyEvent (V.EvKey V.KUp [])) = modify historyPrev
handleEvent (VtyEvent (V.EvKey V.KDown [])) = modify historyNext
handleEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = modify applySuggestion
handleEvent ev = do
  zoom editorLens (handleEditorEvent ev)
  st <- get
  put (refreshInput st)

appendOutput :: AppState -> Text -> Text -> AppState
appendOutput st line input =
  let header = ">> " <> input
      outputs = line : header : stOutput st
  in st
    { stOutput = outputs
    , stHistory = input : stHistory st
    , stHistoryIndex = 0
    }

appendOutputs :: AppState -> [Text] -> Text -> AppState
appendOutputs st outputs input =
  let header = ">> " <> input
      items = reverse outputs <> [header]
  in st
    { stOutput = items <> stOutput st
    , stHistory = input : stHistory st
    , stHistoryIndex = 0
    }

refreshInput :: AppState -> AppState
refreshInput st =
  let input = Text.strip (Text.intercalate "\n" (getEditContents (stEditor st)))
      suggest = suggestMatches (currentToken input)
      doc = docForInput input
  in st { stSuggestions = suggest, stDoc = doc }

currentToken :: Text -> Text
currentToken input =
  case reverse (Text.words input) of
    (tok:_) -> tok
    [] -> input

applySuggestion :: AppState -> AppState
applySuggestion st = case stSuggestions st of
  (s:_) ->
    let input = Text.intercalate "\n" (getEditContents (stEditor st))
        next = replaceLastToken input s
    in refreshInput st { stEditor = editor InputEditor (Just 1) next }
  [] -> st

replaceLastToken :: Text -> Text -> Text
replaceLastToken input suggestion =
  let parts = Text.words input
  in case parts of
      [] -> suggestion
      [_] -> suggestion
      _ ->
        let prefix = Text.unwords (init parts)
        in prefix <> " " <> suggestion

historyPrev :: AppState -> AppState
historyPrev st =
  let idx = stHistoryIndex st
      hist = stHistory st
  in if idx < length hist
      then applyHistory st (idx + 1)
      else st

historyNext :: AppState -> AppState
historyNext st =
  let idx = stHistoryIndex st
  in if idx > 1
      then applyHistory st (idx - 1)
      else st { stHistoryIndex = 0, stEditor = editor InputEditor (Just 1) "" }

applyHistory :: AppState -> Int -> AppState
applyHistory st idx =
  let hist = stHistory st
      value = fromMaybe "" (safeIndex (idx - 1) hist)
  in refreshInput st
      { stHistoryIndex = idx
      , stEditor = editor InputEditor (Just 1) value
      }

safeIndex :: Int -> [a] -> Maybe a
safeIndex n xs
  | n < 0 = Nothing
  | otherwise = case drop n xs of
      (y:_) -> Just y
      [] -> Nothing

editorLens :: Lens' AppState (Editor Text Name)
editorLens f st = fmap (\ed' -> st { stEditor = ed' }) (f (stEditor st))
