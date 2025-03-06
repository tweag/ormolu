{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}

module Ormolu.Live.AceEditor
  ( Input (..),
    Model,
    initialModel,
    Position (..),
    Action (..),
    updateModel,
    viewModel,
  )
where

import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Foldable
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Javascript.JSaddle qualified as JS
import Miso
import Miso.FFI qualified as JS
import Miso.String (ms)
import Ormolu.Live.JSUtil

data Input = Input
  { id :: Text,
    readOnly :: Bool,
    placeholder :: Maybe Text,
    focus :: Bool
  }

newtype Model = Model {editor :: Maybe JS.JSVal}
  deriving stock (Generic)

instance Eq Model where
  _ == _ = True

initialModel :: Model
initialModel = Model {editor = Nothing}

data Position = Position {row :: Int, column :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (JS.FromJSVal)

data Action
  = OnCreated
  | SetEditor JS.JSVal
  | InputChanged Text
  | CursorPositionChanged Position
  | SetInput Text

updateModel :: Input -> Action -> Transition Action Model ()
updateModel input = \case
  OnCreated -> scheduleSub \sink -> do
    editorEl <- JS.getElementById (ms input.id)

    conf <- JS.obj
    conf JS.<# "minLines" $ (25 :: Int)
    conf JS.<# "maxLines" $ (50 :: Int)
    conf JS.<# "readOnly" $ input.readOnly
    conf JS.<# "showPrintMargin" $ False
    conf JS.<# "tabSize" $ (2 :: Int)
    conf JS.<# "useSoftTabs" $ True
    conf JS.<# "highlightActiveLine" $ False
    forM_ input.placeholder $ conf JS.<# "placeholder"
    conf JS.<# "mode" $ "ace/mode/haskell"
    conf JS.<# "theme" $
      isDarkModeEnabled <&> \case
        True -> "ace/theme/github_dark"
        False -> "ace/theme/github_light_default"
    editor <- JS.jsg "ace" ^. JS.js2 "edit" editorEl conf

    if input.readOnly
      then do
        editor
          ^. JS.js "renderer"
            . JS.js "$cursorLayer"
            . JS.js "element"
            . JS.js "style"
            . JS.jss "display" "none"
      else do
        let inputChanged = JS.fun \_ _ _ -> do
              val <- JS.fromJSValUnchecked =<< editor ^. JS.js0 "getValue"
              liftIO $ sink $ InputChanged val
            cursorPositionChanged = JS.fun \_ _ _ -> do
              pos <-
                JS.fromJSValUnchecked
                  =<< editor ^. JS.js "selection" . JS.js0 "getCursor"
              liftIO $ sink $ CursorPositionChanged pos
        void $ editor ^. JS.js "session" . JS.js2 "on" "change" inputChanged
        void $
          editor
            ^. JS.js "session"
              . JS.js "selection"
              . JS.js2 "on" "changeCursor" cursorPositionChanged

    let editorFocus = JS.fun \_ _ _ -> void $ editor ^. JS.js0 "focus"
    when input.focus . void $
      editor ^. JS.js "renderer" . JS.js2 "once" "afterRender" editorFocus

    liftIO $ sink $ SetEditor editor
  SetEditor editor -> do
    #editor .= Just editor
  InputChanged {} -> pure ()
  CursorPositionChanged {} -> pure ()
  SetInput input -> do
    editor <- use #editor
    forM_ editor \editor ->
      scheduleIO_ $ void $ editor ^. JS.js2 "setValue" input (-1 :: Int)

viewModel :: Input -> Model -> View Action
viewModel input _model =
  nodeHtmlKeyed
    (ms "div")
    (Miso.toKey input.id)
    [ id_ $ ms input.id,
      class_ $ ms "is-size-6",
      onCreated OnCreated
    ]
    []
