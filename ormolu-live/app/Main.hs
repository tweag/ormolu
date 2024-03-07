{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.DeepSeq (force)
import Control.Exception qualified as E
import Control.Lens
import Data.Bool (bool)
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.List (intersperse)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.Types.PackageName (PackageName)
import GHC.Driver.Ppr (showSDocUnsafe)
import GHC.Generics (Generic)
import GHC.Hs.Dump qualified as Dump
import Language.Javascript.JSaddle.Wasm qualified as JSaddle.Wasm
import Miso
import Miso.String (MisoString, fromMisoString, ms)
import Ormolu qualified as O
import Ormolu.Config qualified as O
import Ormolu.Exception qualified as O
import Ormolu.Fixity qualified as O
import Ormolu.Parser qualified as O
import Ormolu.Parser.Result as O
import Ormolu.Terminal qualified as O
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Exception

type OrmoluConfig = O.Config O.RegionIndices

data Output
  = FormattingFailed SomeException
  | FormattingSucceeded Text
  deriving stock (Show, Generic)

instance Eq Output where
  FormattingSucceeded t == FormattingSucceeded t' = t == t'
  FormattingFailed _ == FormattingFailed _ = True
  _ == _ = False

data Model = Model
  { input :: MisoString,
    output :: Output,
    config :: OrmoluConfig,
    showParseResult :: Bool
  }
  deriving stock (Show, Eq, Generic)

data Action
  = NoOp
  | SetInput MisoString
  | SetOutput Output
  | Format
  | UpdateConfig (OrmoluConfig -> OrmoluConfig)
  | SetShowParseResult Bool

foreign export javascript "hs_start" main :: IO ()

main :: IO ()
main = JSaddle.Wasm.run $ startApp App {..}
  where
    initialAction = NoOp
    model = Model {..}
      where
        input = ""
        output = FormattingSucceeded ""
        config = O.defaultConfig {O.cfgCheckIdempotence = True}
        showParseResult = False
    update = fromTransition . updateModel
    view = viewModel
    events = defaultEvents
    subs = []
    mountPoint = Nothing
    logLevel = Off

updateModel :: Action -> Transition Action Model ()
updateModel = \case
  NoOp -> pure ()
  SetInput t -> do
    #input .= t
    format
  SetOutput o ->
    #output .= o
  Format -> do
    input <- fromMisoString <$> use #input
    runOrmolu input $ scheduleIO . fmap SetOutput
  UpdateConfig f -> do
    #config %= f
    format
  SetShowParseResult b ->
    #showParseResult .= b
  where
    format = scheduleIO $ pure Format
    runOrmolu input schedule = do
      config <- use #config
      schedule $
        tryAnyDeep (O.ormolu config "<input>" input) <&> \case
          Right t -> FormattingSucceeded t
          Left e -> FormattingFailed e

viewModel :: Model -> View Action
viewModel model@Model {..} =
  div_
    []
    [ link_ [rel_ "stylesheet", href_ "bulma.min.css"],
      section_ [class_ "section"] . pure . div_ [class_ "container is-fluid"] $
        [ h1_ [class_ "title"] [text "Ormolu Live"],
          div_
            [class_ "content"]
            [ p_
                []
                [ text $ "Version " <> VERSION_ormolu <> ", commit ",
                  a_
                    [href_ $ "https://github.com/tweag/ormolu/commit/" <> "XXX", target_ "blank"]
                    [span_ [class_ "is-family-code"] [text . ms . T.take 7 $ "XXX"]],
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
                    #cfgCheckIdempotence
                    "Check idempotence (formatting twice is the same as formatting once)",
                  configCheckbox
                    #cfgUnsafe
                    "Unsafe mode (don't ensure that formatting preserves the AST)",
                  configCheckbox
                    (#cfgSourceType . iso (== O.SignatureSource) (bool O.ModuleSource O.SignatureSource))
                    "Format a Backpack signature",
                  checkbox (^. #showParseResult) SetShowParseResult "Show internal parse result"
                ]
            ],
          div_
            [class_ "columns"]
            [ div_
                [class_ "column is-half is-flex"]
                [ textarea_
                    [class_ "textarea is-family-code", onInput SetInput, rows_ "20", autofocus_ True]
                    [text input]
                ],
              div_ [class_ "column is-half is-flex"] [out]
            ]
        ]
          <> [ div_
                 [class_ "columns"]
                 [ pre_
                     [class_ "column is-half is-family-code"]
                     [ text . ms . prettyAST config . fromMisoString $ input
                     ],
                   pre_
                     [class_ "column is-half is-family-code"]
                     [text . ms . prettyAST config $ m | FormattingSucceeded m <- [output]]
                 ]
               | showParseResult
             ]
    ]
  where
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
    configCheckbox (cloneLens -> l) = checkbox (^. #config . l) \c -> UpdateConfig $ l .~ c

    out = case output of
      FormattingSucceeded t ->
        pre_ [class_ "is-family-code is-flex-grow-1"] [text t]
      FormattingFailed e
        | Just oe <- fromException e ->
            pre_
              [class_ "is-flex-grow-1 content has-background-danger-light has-text-danger-dark"]
              [text . ms . O.runTermPure . O.printOrmoluException $ oe]
        | otherwise ->
            pre_
              [class_ "is-flex-grow-1 content has-background-danger-light has-text-danger-dark"]
              [text . ms . show $ e]

-- Wizer

foreign export ccall evaluateFixityInfo :: IO ()

evaluateFixityInfo :: IO ()
evaluateFixityInfo =
  void . E.evaluate $ force (O.hackageInfo, liveDepencencies)

-- actual logic

-- | We want to make as many packages as possible available by default, so we
-- only exclude packages that contain modules with the same name as in certain
-- "priority" packages, in order to avoid imprecise fixities.
liveDepencencies :: Set PackageName
liveDepencencies =
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

prettyAST :: OrmoluConfig -> Text -> Text
prettyAST cfg src = unsafePerformIO do
  let pfixityMap = O.packageFixityMap O.defaultDependencies
  (_, eSnippets) <-
    O.parseModule cfgWithDeltas pfixityMap "<input>" src
  pure case eSnippets of
    Left e -> T.pack $ show e
    Right snippets -> T.unlines $ showSnippet <$> snippets
  where
    cfgWithDeltas = O.regionIndicesToDeltas (length (T.lines src)) <$> cfg
    showSnippet = \case
      O.ParsedSnippet O.ParseResult {..} ->
        T.pack
          . showSDocUnsafe
          . Dump.showAstData Dump.NoBlankSrcSpan Dump.NoBlankEpAnnotations
          $ prParsedSource
      O.RawSnippet r -> r
