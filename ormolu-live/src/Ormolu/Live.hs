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
import Data.Foldable (toList)
import Data.Generics.Labels ()
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.Types.PackageName (PackageName, mkPackageName)
import GHC.Clock (getMonotonicTime)
import GHC.Driver.Ppr (showSDocUnsafe)
import GHC.Generics (Generic)
import GHC.Hs.Dump qualified as Dump
import Lucid qualified as L
import Miso
import Ormolu qualified as O
import Ormolu.Config qualified as O
import Ormolu.Exception qualified as O
import Ormolu.Fixity qualified as O
import Ormolu.Fixity.Parser (parseDotOrmolu)
import Ormolu.Live.AceEditor qualified as AceEditor
import Ormolu.Live.CommitRev (commitRev)
import Ormolu.Live.JSUtil
import Ormolu.Parser qualified as O
import Ormolu.Parser.Result as O
import Ormolu.Terminal qualified as O
import Text.Megaparsec (errorBundlePretty)
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
                      { O.cfgDependencies = O.defaultDependencies,
                        O.cfgCheckIdempotence = True
                      },
                  showParseResult = False,
                  overrideDeps = False,
                  depsText = "base",
                  overrideDotOrmolu = False,
                  dotOrmoluText = "infixr 9 .\n"
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
    [class_ "min-h-screen bg-neutral-50 text-neutral-800 dark:bg-neutral-950 dark:text-neutral-200"]
    [ header_
        [ class_ $
            "sticky top-0 z-10 border-b border-neutral-200 bg-white/80 backdrop-blur "
              <> "dark:border-neutral-800 dark:bg-neutral-900/80"
        ]
        [ div_
            [class_ "mx-auto flex max-w-7xl items-center justify-between gap-4 px-4 py-3 sm:px-6"]
            [ div_
                [class_ "flex items-baseline gap-3"]
                [ h1_
                    [class_ "text-xl font-semibold tracking-tight text-neutral-900 dark:text-white"]
                    [text "Ormolu Live"],
                  span_
                    [class_ "hidden text-sm text-neutral-500 sm:inline dark:text-neutral-400"]
                    [text "Format Haskell in your browser"]
                ],
              a_
                [ class_ $
                    "inline-flex items-center gap-2 rounded-md border border-neutral-300 bg-white px-3 py-1.5 "
                      <> "text-sm font-medium text-neutral-700 shadow-sm transition hover:bg-neutral-100 "
                      <> "dark:border-neutral-700 dark:bg-neutral-800 dark:text-neutral-200 dark:hover:bg-neutral-700",
                  href_ "https://github.com/tweag/ormolu",
                  target_ "blank"
                ]
                [text "GitHub"]
            ]
        ],
      main_
        [class_ "mx-auto flex max-w-7xl flex-col gap-6 px-4 py-6 sm:px-6"] . mconcat $
        [ [infoAndConfig],
          if model.loading
            then [loadingCard]
            else
              inputOutputEditors
                : [astDump | model.input.cfg.showParseResult],
          [ p_
              [class_ "pt-2 text-center text-sm text-neutral-500 dark:text-neutral-400"]
              [ text $
                  "This website is entirely client-side; "
                    <> "your input is never sent to a remote server."
              ]
          ]
        ]
    ]
  where
    card =
      div_
        [ class_ $
            "rounded-xl border border-neutral-200 bg-white shadow-sm "
              <> "dark:border-neutral-800 dark:bg-neutral-900"
        ]

    loadingCard =
      div_
        [class_ "flex items-center justify-center gap-3 rounded-xl border border-neutral-200 bg-white p-10 text-neutral-500 shadow-sm dark:border-neutral-800 dark:bg-neutral-900 dark:text-neutral-400"]
        [ span_ [class_ "size-2 animate-ping rounded-full bg-neutral-400"] [],
          text "Loading WASM…"
        ]

    infoAndConfig =
      card
        [ div_
            [class_ "flex flex-col gap-4 p-5"]
            [ p_
                [class_ "text-sm text-neutral-600 dark:text-neutral-400"]
                [ text $ "Version " <> VERSION_ormolu <> ", commit ",
                  a_
                    [ class_ "font-mono text-neutral-800 underline decoration-neutral-300 underline-offset-2 hover:decoration-neutral-500 dark:text-neutral-200",
                      href_ $ "https://github.com/tweag/ormolu/commit/" <> commitRev,
                      target_ "blank"
                    ]
                    [text . T.take 7 $ commitRev],
                  text $ ", using ghc-lib-parser " <> VERSION_ghc_lib_parser
                ],
              div_
                [class_ "grid gap-2.5 sm:grid-cols-2"]
                [ configCheckbox
                    (#ormoluCfg . #cfgCheckIdempotence)
                    "Check idempotence"
                    (Just "Ensure that formatting twice is the same as formatting once."),
                  configCheckbox
                    (#ormoluCfg . #cfgUnsafe)
                    "Unsafe mode"
                    (Just "Don't ensure that formatting preserves the AST."),
                  configCheckbox
                    #showParseResult
                    "Show internal parse result"
                    Nothing,
                  configCheckbox
                    #overrideDeps
                    "Specify dependencies"
                    ( Just $
                        "Override the set of packages assumed to be in scope. "
                          <> "This affects operator fixity resolution. "
                          <> "Enter package names separated by spaces or commas."
                    ),
                  configCheckbox
                    #overrideDotOrmolu
                    "Specify .ormolu file"
                    ( Just $
                        "Provide the contents of an .ormolu file: fixity overrides "
                          <> "and module re-export declarations, which affect formatting."
                    )
                ],
              dependenciesField,
              dotOrmoluField
            ]
        ]

    -- A styled multi-line text field used by the reveal-on-checkbox config
    -- sections. @below@ is rendered under the field (a hint or an error).
    configTextarea rows placeholder (cloneLens -> valueLens) below =
      div_
        [class_ "flex flex-col gap-1.5"]
        [ textarea_
            [ class_ $
                "w-full rounded-md border border-neutral-300 bg-neutral-50 px-3 py-2 font-mono text-sm "
                  <> "text-neutral-800 shadow-inner outline-none transition placeholder:text-neutral-400 "
                  <> "focus:border-neutral-400 focus:ring-2 focus:ring-neutral-200 "
                  <> "dark:border-neutral-700 dark:bg-neutral-950 dark:text-neutral-200 "
                  <> "dark:placeholder:text-neutral-600 dark:focus:border-neutral-500 dark:focus:ring-neutral-800",
              rows_ rows,
              placeholder_ placeholder,
              value_ $ model ^. #input . #cfg . valueLens,
              onInput $ \t -> UpdateConfig $ valueLens .~ t
            ]
            [],
          below
        ]

    hint t =
      p_
        [class_ "text-xs text-neutral-500 dark:text-neutral-400"]
        [text t]

    dependenciesField
      | not model.input.cfg.overrideDeps = text ""
      | otherwise =
          configTextarea
            "2"
            "e.g. base lens servant optics"
            #depsText
            (hint "Separate package names with spaces or commas.")

    dotOrmoluField
      | not model.input.cfg.overrideDotOrmolu = text ""
      | otherwise =
          configTextarea
            "4"
            "e.g. infixr 5 +++"
            #dotOrmoluText
            dotOrmoluBelow
      where
        dotOrmoluBelow =
          case parseDotOrmolu "<.ormolu>" model.input.cfg.dotOrmoluText of
            Right _ ->
              hint "One fixity override or module re-export per line."
            Left errBundle ->
              pre_
                [class_ "code-surface rounded-md bg-red-50 px-3 py-2 text-xs text-red-700 dark:bg-red-950/40 dark:text-red-300"]
                [text . T.pack $ errorBundlePretty errBundle]

    sourceTypeToggle =
      let current = model.input.cfg.ormoluCfg.cfgSourceType
          segment sourceType label =
            let active = current == sourceType
             in button_
                  [ type_ "button",
                    class_ $
                      "rounded px-2 py-0.5 text-xs font-medium transition "
                        <> if active
                          then "bg-white text-neutral-900 shadow-sm dark:bg-neutral-700 dark:text-white"
                          else "text-neutral-500 hover:text-neutral-700 dark:text-neutral-400 dark:hover:text-neutral-200",
                    onClick . UpdateConfig $ #ormoluCfg . #cfgSourceType .~ sourceType
                  ]
                  [text label]
       in div_
            [ class_ $
                "inline-flex gap-1 rounded-md border border-neutral-200 bg-neutral-100 p-0.5 "
                  <> "dark:border-neutral-700 dark:bg-neutral-800"
            ]
            [ segment O.ModuleSource "Source code",
              segment O.SignatureSource "Backpack signature"
            ]

    panelLabel t =
      div_
        [class_ "flex items-center justify-between border-b border-neutral-200 px-4 py-2 dark:border-neutral-800"]
        [ span_
            [class_ "text-xs font-semibold uppercase tracking-wide text-neutral-500 dark:text-neutral-400"]
            [text t]
        ]

    inputOutputEditors =
      div_
        [class_ "grid gap-6 lg:grid-cols-2"]
        [ card
            [ div_
                [class_ "flex items-center justify-between gap-3 border-b border-neutral-200 px-4 py-2 dark:border-neutral-800"]
                [ span_
                    [class_ "text-xs font-semibold uppercase tracking-wide text-neutral-500 dark:text-neutral-400"]
                    [text "Input"],
                  sourceTypeToggle
                ],
              div_
                [class_ "p-1"]
                [ ActionInputEditor
                    <$> AceEditor.viewModel inputEditorInput model.inputEditor
                ],
              div_
                [class_ "border-t border-neutral-200 px-4 py-2 text-xs text-neutral-500 dark:border-neutral-800 dark:text-neutral-400"]
                [ text "Cursor ",
                  span_
                    [class_ "font-mono text-neutral-700 dark:text-neutral-300"]
                    [ let pos = model.inputCursor
                       in text $ T.pack $ show (pos.row + 1) <> ":" <> show (pos.column + 1)
                    ]
                ]
            ],
          card
            [ div_
                [class_ "flex items-center justify-between border-b border-neutral-200 px-4 py-2 dark:border-neutral-800"]
                [ span_
                    [class_ "text-xs font-semibold uppercase tracking-wide text-neutral-500 dark:text-neutral-400"]
                    [text "Output"],
                  button_
                    [ id_ "copy-btn",
                      class_ $
                        "rounded-md border border-neutral-300 bg-white px-2.5 py-1 text-xs font-medium "
                          <> "text-neutral-700 shadow-sm transition hover:bg-neutral-100 "
                          <> "dark:border-neutral-700 dark:bg-neutral-800 dark:text-neutral-200 dark:hover:bg-neutral-700",
                      onClick CopyOutputToClipboard
                    ]
                    [text "Copy"]
                ],
              div_
                [class_ "p-1"]
                [ ActionOutputEditor
                    <$> AceEditor.viewModel outputEditorInput model.outputEditor
                ],
              div_
                [class_ "border-t border-neutral-200 px-4 py-2 text-xs text-neutral-500 dark:border-neutral-800 dark:text-neutral-400"]
                [ text $ T.pack case model.output <&> (.elapsed) of
                    Just d -> printf "Processing time: %.0f ms" (d * 1000)
                    Nothing -> "\x00a0"
                ]
            ]
        ]

    astDump =
      div_
        [class_ "grid gap-6 lg:grid-cols-2"]
        [ card
            [ panelLabel "Input AST",
              pre_
                [class_ "code-surface p-4 text-xs leading-relaxed text-neutral-700 dark:text-neutral-300"]
                [text ast | ast <- toList $ model.output >>= (.inputAST)]
            ],
          card
            [ panelLabel "Output AST",
              pre_
                [class_ "code-surface p-4 text-xs leading-relaxed text-neutral-700 dark:text-neutral-300"]
                [text ast | ast <- toList $ model.output >>= (.outputAST)]
            ]
        ]

    -- A small "(?)" help badge that reveals @tip@ as a Material-style light
    -- card tooltip on hover. Implemented purely with CSS (group-hover), so
    -- there is no browser delay and no JS.
    helpBadge tip =
      span_
        [class_ "group relative ml-1 inline-flex"]
        [ span_
            [ class_ $
                "inline-flex size-4 shrink-0 select-none items-center justify-center "
                  <> "rounded-full border border-neutral-300 text-[10px] font-semibold leading-none "
                  <> "text-neutral-500 transition group-hover:border-neutral-400 group-hover:text-neutral-700 "
                  <> "dark:border-neutral-600 dark:text-neutral-400 dark:group-hover:border-neutral-500 dark:group-hover:text-neutral-200"
            ]
            [text "?"],
          span_
            [ class_ $
                "pointer-events-none absolute bottom-full left-1/2 z-20 mb-2 w-64 -translate-x-1/2 "
                  <> "rounded-lg border border-neutral-200 bg-white px-3 py-2 text-xs font-normal leading-relaxed "
                  <> "text-neutral-600 opacity-0 shadow-lg transition-opacity duration-150 group-hover:opacity-100 "
                  <> "dark:border-neutral-700 dark:bg-neutral-800 dark:text-neutral-300"
            ]
            [ text tip,
              -- little arrow pointing down at the badge
              span_
                [ class_ $
                    "absolute left-1/2 top-full size-2 -translate-x-1/2 -translate-y-1/2 rotate-45 "
                      <> "border-b border-r border-neutral-200 bg-white "
                      <> "dark:border-neutral-700 dark:bg-neutral-800"
                ]
                []
            ]
        ]

    -- Utilities for checkboxes. @mTip@, when present, appends a "(?)" help
    -- badge carrying a hover tooltip.
    checkbox fromModel action desc mTip =
      label_
        [class_ "flex cursor-pointer items-start gap-2.5 text-sm text-neutral-700 dark:text-neutral-300"]
        [ input_
            [ type_ "checkbox",
              class_ "mt-0.5 size-4 shrink-0 rounded border-neutral-300 accent-neutral-800 dark:border-neutral-600 dark:accent-neutral-300",
              checked_ $ fromModel model,
              onChecked \(Checked c) -> action c
            ],
          span_
            [class_ "inline-flex items-center"]
            (text desc : foldMap (\tip -> [helpBadge tip]) mTip)
        ]
    configCheckbox (cloneLens -> l) desc mTip =
      checkbox (^. #input . #cfg . l) (\c -> UpdateConfig $ l .~ c) desc mTip

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
    showParseResult :: Bool,
    -- | Whether the user-specified dependency override is active. When 'False',
    -- the default dependencies are used. When 'True', the packages named in
    -- 'depsText' are used, which affects fixity resolution.
    overrideDeps :: Bool,
    -- | The user-specified list of dependencies (whitespace/comma separated).
    -- Kept independently of 'overrideDeps' so it persists across toggling.
    depsText :: Text,
    -- | Whether the user-specified @.ormolu@ file override is active.
    overrideDotOrmolu :: Bool,
    -- | The user-specified @.ormolu@ file contents (fixity overrides and
    -- module re-exports). Kept independently of 'overrideDotOrmolu' so it
    -- persists across toggling.
    dotOrmoluText :: Text
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

-- | Parse a whitespace/comma-separated list of package names into a set. An
-- empty override yields an empty set (i.e. only what Ormolu assumes
-- unconditionally), which is a legitimate experiment.
parseDependencies :: Text -> Set PackageName
parseDependencies =
  Set.fromList
    . map (mkPackageName . T.unpack)
    . filter (not . T.null)
    . T.split (\c -> c == ',' || c == ' ' || c == '\n' || c == '\t' || c == '\r')

format :: (MonadIO m) => FormatInput -> m FormatOutput
format input = liftIO do
  let withDeps cfg
        | input.cfg.overrideDeps =
            cfg {O.cfgDependencies = parseDependencies input.cfg.depsText}
        | otherwise = cfg
      -- Apply the user-provided .ormolu file when active and it parses; a
      -- parse error simply leaves the overrides untouched (the field shows the
      -- error inline).
      withDotOrmolu cfg
        | input.cfg.overrideDotOrmolu,
          Right (fixityOverrides, moduleReexports) <-
            parseDotOrmolu "<.ormolu>" input.cfg.dotOrmoluText =
            cfg
              { O.cfgFixityOverrides = fixityOverrides,
                O.cfgModuleReexports = moduleReexports
              }
        | otherwise = cfg
      ormoluCfg = withDotOrmolu . withDeps $ input.cfg.ormoluCfg
  t0 <- getMonotonicTime
  !res <- tryAnyDeep $ O.ormolu ormoluCfg "<input>" input.src
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
    L.link_ [L.rel_ "stylesheet", L.href_ "app.css"]
  L.body_ do
    L.toHtml $ viewModel initialModel
    L.script_ [L.src_ "jsaddle.js"] T.empty
    L.script_ [L.src_ "index.js", L.type_ "module"] T.empty
