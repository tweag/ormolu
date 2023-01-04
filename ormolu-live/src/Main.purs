module Main (main) where

import Prelude

import Ace as Ace
import Ace.EditSession as AceSession
import Ace.Editor as AceEditor
import Ace.Selection as AceSelection
import Control.Alternative (guard)
import Control.Monad.Error.Class (throwError)
import Data.Array (intersperse)
import Data.Foldable (for_)
import Data.Lens (cloneLens, (%~), (.~), (^.))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.String as S
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = HA.runHalogenAff $ runUI component unit =<< HA.awaitBody

type OrmoluInput =
  { inputStr :: String
  , checkIdempotence :: Boolean
  , unsafeMode :: Boolean
  , formatBackpack :: Boolean
  , showAST :: Boolean
  }

type OrmoluOutput =
  { fmtStr :: String
  , inputAST :: String
  , outputAST :: String
  }

type State =
  { input :: OrmoluInput
  , output :: OrmoluOutput
  , inputCursor :: Ace.Position
  , inProgress :: Maybe OrmoluInput
  , notifyWorker :: OrmoluInput -> Effect Unit
  , inputEditor :: Maybe Ace.Editor
  , outputEditor :: Maybe Ace.Editor
  }

data Action
  = Initialize
  | ModifyInput (OrmoluInput -> OrmoluInput)
  | InputCursorChanged Ace.Position
  | SetOutput OrmoluOutput
  | OrmoluWorkerReady

component :: forall query input output. H.Component query input output Aff
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval H.defaultEval
        { initialize = Just Initialize
        , handleAction = handleAction
        }
    }
  where
  initialState :: State
  initialState =
    { input:
        { inputStr: ""
        , checkIdempotence: true
        , unsafeMode: false
        , formatBackpack: false
        , showAST: false
        }
    , output: mempty
    , inputCursor: Ace.Position { row: 0, column: 0 }
    , inProgress: Nothing
    , notifyWorker: mempty
    , inputEditor: Nothing
    , outputEditor: Nothing
    }

  render state =
    HH.section [ HP.classes [ HH.ClassName "section" ] ]
      [ HH.div [ HP.classes [ HH.ClassName "container", HH.ClassName "is-fluid" ] ] $
          [ HH.h1 [ HP.classes [ HH.ClassName "title" ] ] [ HH.text "Ormolu Live" ]
          , HH.div [ HP.classes [ HH.ClassName "content" ] ]
              [ HH.p_
                  [ HH.text $ "Version " <> metadata.version <> ", commit "
                  , HH.a
                      [ HP.href $ "https://github.com/tweag/ormolu/commit/" <> metadata.rev
                      , HP.target "blank"
                      ]
                      [ HH.span [ HP.classes [ HH.ClassName "is-family-code" ] ]
                          [ HH.text $ S.take 7 metadata.rev ]
                      ]
                  , HH.text $ ", using ghc-lib-parser " <> metadata.ghcAPIVersion
                  ]
              , HH.p_
                  [ HH.a
                      [ HP.classes [ HH.ClassName "button", HH.ClassName "is-link", HH.ClassName "is-light" ]
                      , HP.href "https://github.com/tweag/ormolu"
                      , HP.target "blank"
                      ]
                      [ HH.text "See the GitHub repository" ]
                  ]
              , HH.div_ $ intersperse HH.br_
                  [ checkbox (prop (Proxy :: _ "checkIdempotence"))
                      "Check idempotence (formatting twice is the same as formatting once)"
                  , checkbox (prop (Proxy :: _ "unsafeMode"))
                      "Unsafe mode (don't ensure that formatting preserves the AST)"
                  , checkbox (prop (Proxy :: _ "formatBackpack"))
                      "Format a Backpack signature"
                  , checkbox (prop (Proxy :: _ "showAST"))
                      "Show internal parse result"
                  ]
              ]
          , HH.div [ HP.classes [ HH.ClassName "columns" ] ]
              [ HH.div [ HP.classes [ HH.ClassName "column" ] ]
                  [ HH.div [ HP.ref aceInputRef, HP.classes [ HH.ClassName "is-size-6" ] ]
                      [ HH.text "Loading Ormolu WASM..." ]
                  , HH.text $ "Cursor: "
                  , let
                      Ace.Position { row, column } = state.inputCursor
                    in
                      HH.span [ HP.classes [ HH.ClassName "is-family-monospace" ] ]
                        [ HH.text $ show (row + 1) <> ":" <> show (column + 1) ]
                  ]
              , HH.div [ HP.classes [ HH.ClassName "column" ], HP.style "position: relative;" ]
                  [ HH.div [ HP.ref aceOutputRef, HP.classes [ HH.ClassName "is-size-6" ] ] []
                  , HH.button [ HP.id "copy-btn" ] [ HH.text "Copy" ]
                  ]
              ]
          ] <> do
            guard state.input.showAST
            [ HH.div [ HP.classes [ HH.ClassName "columns" ] ]
                [ astBox state.output.inputAST
                , astBox state.output.outputAST
                ]
            ]
      , HH.div [ HP.classes [ HH.ClassName "container", HH.ClassName "is-fluid", HH.ClassName "mt-4" ] ]
          [ HH.div [ HP.classes [ HH.ClassName "content", HH.ClassName "has-text-centered" ] ]
              [ HH.text
                  """
                  Note that this website is entirely client-side;
                  in particular, your input is never sent to a remote server.
                  """
              ]
          ]
      ]
    where
    checkbox l desc = HH.label [ HP.classes [ HH.ClassName "checkbox" ] ]
      [ HH.input
          [ HP.type_ HP.InputCheckbox
          , HP.checked (state ^. prop (Proxy :: _ "input") <<< cloneLens l)
          , HE.onChecked $ ModifyInput <<< (cloneLens l .~ _)
          ]
      , HH.text $ " " <> desc
      ]

    astBox ast = HH.pre
      [ HP.classes [ HH.ClassName "column", HH.ClassName "is-family-code" ] ]
      [ HH.text ast ]

  handleAction = case _ of
    Initialize -> do
      let
        mkAceEditor ref = do
          el <- maybe (throwError $ error "Missing text box.") pure
            =<< H.getHTMLElementRef ref
          liftEffect do
            editor <- Ace.editNode el Ace.ace
            AceEditor.setMinLines 25 editor
            AceEditor.setMaxLines 50 editor
            AceEditor.setReadOnly true editor
            AceEditor.setShowPrintMargin false editor
            AceEditor.setTheme "ace/theme/chrome" editor
            session <- AceEditor.getSession editor
            AceSession.setTabSize 2 session
            AceSession.setUseSoftTabs true session
            AceSession.setMode "ace/mode/haskell" session
            pure { editor, session }
      do
        { emitter, listener } <- liftEffect HS.create
        { editor, session } <- mkAceEditor aceInputRef
        liftEffect do
          AceEditor.setPlaceholder "Type or paste Haskell code here" editor
          AceSession.onChange session \_ -> do
            str <- AceSession.getValue session
            HS.notify listener $ ModifyInput _ { inputStr = str }
          selection <- AceEditor.getSelection editor
          AceSelection.onChangeCursor selection $
            HS.notify listener <<< InputCursorChanged =<< AceSelection.getCursor selection
        _ <- H.subscribe emitter
        H.modify_ _ { inputEditor = Just editor }
      do
        { editor, session } <- mkAceEditor aceOutputRef
        liftEffect do
          AceEditor.setHighlightActiveLine false editor
          newClipboard (QuerySelector "#copy-btn") $ AceSession.getValue session
        H.modify_ _ { outputEditor = Just editor }
      do
        { emitter, listener } <- liftEffect HS.create
        postMessage <- liftEffect $ spawnOrmoluWorker
          do HS.notify listener OrmoluWorkerReady
          do HS.notify listener <<< SetOutput
        _ <- H.subscribe emitter
        H.modify_ _ { notifyWorker = postMessage }
    ModifyInput f -> do
      H.modify_ $ prop (Proxy :: _ "input") %~ f
      format
    InputCursorChanged pos ->
      H.modify_ _ { inputCursor = pos }
    SetOutput o -> do
      { input, inProgress, outputEditor } <- H.get
      liftEffect $ for_ outputEditor $
        AceSession.setValue o.fmtStr <=< AceEditor.getSession
      H.modify_ _ { output = o, inProgress = Nothing }
      when (Just input /= inProgress) format
    OrmoluWorkerReady -> do
      { inputEditor } <- H.get
      liftEffect $ for_ inputEditor \editor -> do
        AceSession.setValue "" =<< AceEditor.getSession editor
        AceEditor.setReadOnly false editor
        AceEditor.focus editor
    where
    format = do
      { input, inProgress, notifyWorker } <- H.get
      when (isNothing inProgress) do
        liftEffect $ notifyWorker input
        H.modify_ _ { inProgress = Just input }

  aceInputRef = H.RefLabel "aceInput"
  aceOutputRef = H.RefLabel "aceOutput"

type Metadata =
  { version :: String
  , rev :: String
  , ghcAPIVersion :: String
  }

foreign import metadata :: Metadata

foreign import spawnOrmoluWorker
  :: Effect Unit -- init callback
  -> (OrmoluOutput -> Effect Unit)
  -> Effect (OrmoluInput -> Effect Unit)

foreign import newClipboard
  :: QuerySelector -> Effect String -> Effect Unit
