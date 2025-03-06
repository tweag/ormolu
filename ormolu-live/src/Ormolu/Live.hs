{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}

module Ormolu.Live
  ( app,
    prerenderTo,
  )
where

import Control.Lens
import Control.Monad (guard, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Bool (bool)
import Data.Foldable (toList)
import Data.Generics.Labels ()
import Data.List (intersperse)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.Types.PackageName (PackageName)
import GHC.Clock (getMonotonicTime)
import GHC.Driver.Ppr (showSDocUnsafe)
import GHC.Generics (Generic)
import GHC.Hs.Dump qualified as Dump
import Lucid qualified as L
import Miso
import Miso.String (ms)
import Ormolu qualified as O
import Ormolu.Config qualified as O
import Ormolu.Exception qualified as O
import Ormolu.Fixity qualified as O
import Ormolu.Live.AceEditor qualified as AceEditor
import Ormolu.Live.CommitRev (commitRev)
import Ormolu.Live.JSUtil
import Ormolu.Parser qualified as O
import Ormolu.Parser.Result as O
import Ormolu.Terminal qualified as O
import Text.Printf (printf)
import UnliftIO.Exception

data Model = Model
  { loading :: Bool,
    input :: FormatInput,
    output :: Maybe FormatOutput,
    inProgress :: Maybe FormatInput,
    inputEditor :: AceEditor.Model,
    inputCursor :: AceEditor.Position,
    outputEditor :: AceEditor.Model
  }
  deriving stock (Eq, Generic)

initialModel :: Model
initialModel =
  Model
    { loading = True,
      input =
        FormatInput
          { src = "",
            cfg =
              OrmoluLiveConfig
                { ormoluCfg =
                    O.defaultConfig
                      { O.cfgDependencies = liveDependencies,
                        O.cfgCheckIdempotence = True
                      },
                  showParseResult = False
                }
          },
      output = Nothing,
      inProgress = Nothing,
      inputEditor = AceEditor.initialModel,
      inputCursor = AceEditor.Position 0 0,
      outputEditor = AceEditor.initialModel
    }

data Action
  = Init
  | SetOutput FormatOutput
  | UpdateConfig (OrmoluLiveConfig -> OrmoluLiveConfig)
  | CopyOutputToClipboard
  | ActionInputEditor AceEditor.Action
  | ActionOutputEditor AceEditor.Action

app :: JSM ()
app =
  miso \_uri ->
    App
      { initialAction = Init,
        model = initialModel,
        update = fromTransition . updateModel,
        view = viewModel,
        events = defaultEvents,
        subs = [],
        mountPoint = Nothing,
        logLevel = Off
      }

updateModel :: Action -> Transition Action Model ()
updateModel = \case
  Init -> do
    #loading .= False
  SetOutput o -> do
    #output ?= o
    zoom #outputEditor . mapAction ActionOutputEditor $
      AceEditor.updateModel outputEditorInput $
        AceEditor.SetInput o.result
    justCompleted <- #inProgress <<.= Nothing
    input <- use #input
    when (justCompleted /= Just input) scheduleFormat
  UpdateConfig f -> do
    #input . #cfg %= f
    scheduleFormat
  CopyOutputToClipboard -> do
    output <- uses #output $ maybe "" (.result)
    scheduleIO_ $ writeToClipboard output
  ActionInputEditor a -> do
    zoom #inputEditor . mapAction ActionInputEditor $
      AceEditor.updateModel inputEditorInput a
    case a of
      AceEditor.InputChanged input -> do
        #input . #src .= input
        scheduleFormat
      AceEditor.CursorPositionChanged pos -> do
        #inputCursor .= pos
      _ -> pure ()
  ActionOutputEditor a -> do
    zoom #outputEditor . mapAction ActionOutputEditor $
      AceEditor.updateModel outputEditorInput a
  where
    scheduleFormat =
      use #inProgress >>= \case
        Just _ -> pure ()
        Nothing -> do
          input <- use #input
          #inProgress ?= input
          scheduleIO $ SetOutput <$> format input

viewModel :: Model -> View Action
viewModel model =
  div_
    []
    [ section_
        [class_ "section"]
        [ div_ [class_ "container is-fluid"] . mconcat $
            [ [ h1_ [class_ "title"] [text "Ormolu Live"],
                infoAndConfig
              ],
              if model.loading
                then [p_ [] [text "Loading WASM..."]]
                else
                  inputOutputEditors
                    : [astDump | model.input.cfg.showParseResult],
              [ div_
                  [class_ "content has-text-centered"]
                  [ text $
                      "Note that this website is entirely client-side; "
                        <> "in particular, your input is never sent to a remote server."
                  ]
              ]
            ]
        ]
    ]
  where
    infoAndConfig =
      div_
        [class_ "content"]
        [ p_
            []
            [ text $ "Version " <> VERSION_ormolu <> ", commit ",
              a_
                [href_ $ ms $ "https://github.com/tweag/ormolu/commit/" <> commitRev, target_ "blank"]
                [span_ [class_ "is-family-code"] [text $ ms $ T.take 7 commitRev]],
              text $ ", using ghc-lib-parser " <> VERSION_ghc_lib_parser
            ],
          p_
            []
            [ a_
                [class_ "button is-link is-light", href_ "https://github.com/tweag/ormolu", target_ "blank"]
                [text "See the GitHub repository"]
            ],
          div_ [] . intersperse (br_ []) $
            [ configCheckbox
                (#ormoluCfg . #cfgCheckIdempotence)
                "Check idempotence (formatting twice is the same as formatting once)",
              configCheckbox
                (#ormoluCfg . #cfgUnsafe)
                "Unsafe mode (don't ensure that formatting preserves the AST)",
              configCheckbox
                (#ormoluCfg . #cfgSourceType . iso (== O.SignatureSource) (bool O.ModuleSource O.SignatureSource))
                "Format a Backpack signature",
              configCheckbox
                #showParseResult
                "Show internal parse result"
            ]
        ]

    inputOutputEditors =
      div_
        [class_ "columns"]
        [ div_
            [class_ "column"]
            [ ActionInputEditor
                <$> AceEditor.viewModel inputEditorInput model.inputEditor,
              text "Cursor: ",
              span_
                [class_ "is-family-monospace"]
                [ let pos = model.inputCursor
                   in text $ ms $ show (pos.row + 1) <> ":" <> show (pos.column + 1)
                ]
            ],
          div_
            [class_ "column is-relative"]
            [ ActionOutputEditor
                <$> AceEditor.viewModel outputEditorInput model.outputEditor,
              text $ ms case model.output <&> (.elapsed) of
                Just d -> printf "Processing time: %.0f ms" (d * 1000)
                Nothing -> "" :: String,
              button_
                [ id_ "copy-btn",
                  class_ "button copy-output",
                  styleInline_ "position: absolute; top: 20px; right: 20px;",
                  onClick CopyOutputToClipboard
                ]
                [text "Copy"]
            ]
        ]

    astDump =
      div_
        [class_ "columns"]
        [ div_
            [class_ "column is-half"]
            [ pre_
                [class_ "is-family-code"]
                [text $ ms ast | ast <- toList $ model.output >>= (.inputAST)]
            ],
          div_
            [class_ "column is-half"]
            [ pre_
                [class_ "is-family-code"]
                [text $ ms ast | ast <- toList $ model.output >>= (.outputAST)]
            ]
        ]

    -- Utilities for checkboxes
    checkbox fromModel action desc =
      label_
        [class_ "checkbox"]
        [ input_
            [ type_ "checkbox",
              checked_ $ fromModel model,
              onChecked \(Checked c) -> action c
            ],
          text $ " " <> desc
        ]
    configCheckbox (cloneLens -> l) =
      checkbox (^. #input . #cfg . l) \c -> UpdateConfig $ l .~ c

inputEditorInput :: AceEditor.Input
inputEditorInput =
  AceEditor.Input
    { id = "input-editor",
      readOnly = False,
      placeholder = Just "Type or paste Haskell code here",
      focus = True
    }

outputEditorInput :: AceEditor.Input
outputEditorInput =
  AceEditor.Input
    { id = "output-editor",
      readOnly = True,
      placeholder = Nothing,
      focus = False
    }

--------------------------------------------------------------------------------
-- Formatting

type OrmoluConfig = O.Config O.RegionIndices

data OrmoluLiveConfig = OrmoluLiveConfig
  { ormoluCfg :: OrmoluConfig,
    showParseResult :: Bool
  }
  deriving stock (Show, Eq, Generic)

data FormatInput = FormatInput
  { src :: Text,
    cfg :: OrmoluLiveConfig
  }
  deriving stock (Show, Eq, Generic)

data FormatOutput = FormatOutput
  { result :: Text,
    inputAST :: Maybe Text,
    outputAST :: Maybe Text,
    -- | Time to format in seconds
    elapsed :: Double
  }
  deriving stock (Show, Eq, Generic)

format :: (MonadIO m) => FormatInput -> m FormatOutput
format input = liftIO do
  t0 <- getMonotonicTime
  !res <- tryAnyDeep $ O.ormolu input.cfg.ormoluCfg "<input>" input.src
  t1 <- getMonotonicTime
  let result = case res of
        Right t -> t
        Left e -> case fromException e of
          Just oe -> O.runTermPure . O.printOrmoluException $ oe
          Nothing -> T.pack . show $ e
      elapsed = t1 - t0
  inputAST <- runMaybeT do
    guard input.cfg.showParseResult
    prettyAST input
  outputAST <- runMaybeT do
    guard input.cfg.showParseResult
    Right src <- pure res
    prettyAST input {src}
  pure FormatOutput {..}

-- | We want to make as many packages as possible available by default, so we
-- only exclude packages that contain modules with the same name as in certain
-- "priority" packages, in order to avoid imprecise fixities.
liveDependencies :: Set PackageName
liveDependencies =
  Map.keysSet $ Map.filterWithKey nonConflicting hackageInfo
  where
    O.HackageInfo hackageInfo = O.hackageInfo
    priorityPkgs = Set.fromList ["base", "lens"]
    priorityModules =
      Set.unions . fmap Map.keysSet $
        Map.restrictKeys hackageInfo priorityPkgs
    nonConflicting pkgName (Map.keysSet -> modules) =
      pkgName `Set.member` priorityPkgs
        || modules `Set.disjoint` priorityModules

prettyAST :: (MonadIO m) => FormatInput -> m Text
prettyAST input = do
  let pfixityMap = O.packageFixityMap O.defaultDependencies
  (_, eSnippets) <- O.parseModule cfgWithDeltas pfixityMap "<input>" input.src
  pure case eSnippets of
    Left e -> T.pack $ show e
    Right snippets -> T.unlines $ showSnippet <$> snippets
  where
    cfgWithDeltas =
      O.regionIndicesToDeltas (length (T.lines input.src)) <$> input.cfg.ormoluCfg
    showSnippet = \case
      O.ParsedSnippet O.ParseResult {..} ->
        T.pack
          . showSDocUnsafe
          . Dump.showAstData Dump.NoBlankSrcSpan Dump.NoBlankEpAnnotations
          $ prParsedSource
      O.RawSnippet r -> r

--------------------------------------------------------------------------------
-- Pre-rendering

prerenderTo :: FilePath -> IO ()
prerenderTo path = L.renderToFile path $ L.doctypehtml_ do
  L.head_ do
    L.meta_ [L.charset_ "utf-8"]
    L.meta_ [L.name_ "viewport", L.content_ "width=device-width, initial-scale=1"]
    L.title_ "Ormolu Live"
    L.link_ [L.rel_ "stylesheet", L.href_ "bulma.min.css"]
  L.body_ do
    L.toHtml $ viewModel initialModel
    L.script_ [L.src_ "jsaddle.js"] T.empty
    L.script_ [L.src_ "index.js", L.type_ "module"] T.empty
