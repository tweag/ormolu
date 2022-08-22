module Main (main) where

import Control.Lens
import Data.Bool (bool)
import Data.Foldable (toList)
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.List (intersperse)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Development.GitRev
import GHC.Driver.Ppr (showSDocUnsafe)
import GHC.Generics (Generic)
import qualified GHC.Hs.Dump as Dump
import GHC.SyntaxHighlighter
import qualified Language.Javascript.JSaddle.Warp.Extra as JSaddleWarp
import Miso
import Miso.String (MisoString, fromMisoString, ms)
import qualified Ormolu as O
import qualified Ormolu.Config as O
import Ormolu.Fixity
import qualified Ormolu.Parser as O
import qualified Ormolu.Parser.Result as O
import qualified Ormolu.Utils as O
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Exception

type Output = Either (Either String O.OrmoluException) Text

type OrmoluConfig = O.Config O.RegionIndices

data Model = Model
  { input :: MisoString,
    output :: Output,
    config :: OrmoluConfig,
    showParseResult :: Bool
  }
  deriving stock (Show, Eq, Generic)

data Action
  = Setup
  | SetInput MisoString
  | SetOutput Output
  | Format
  | UpdateConfig (OrmoluConfig -> OrmoluConfig)
  | SetShowParseResult Bool

main :: IO ()
main = JSaddleWarp.run 8080 "www" $ startApp App {..}
  where
    initialAction = Setup
    model = Model {..}
      where
        input = ""
        output = Right ""
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
  Setup ->
    -- Format something with an unusual operator in order to fill the
    -- fixity map cache
    runOrmolu "1++++++1" $ scheduleIO_ . void
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
        tryAnyDeep (O.ormolu config "<input>" input)
          <&> _Left %~ extractOrmoluException

viewModel :: Model -> View Action
viewModel model@Model {..} =
  div_
    []
    [ link_ [rel_ "stylesheet", href_ "bulma.min.css"],
      link_ [rel_ "stylesheet", href_ "editor.css"],
      script_ [] "new ClipboardJS('.copy-output');",
      section_ [class_ "section"] . pure . div_ [class_ "container is-fluid"] $
        [ h1_ [class_ "title"] [text "Ormolu Live"],
          div_
            [class_ "content"]
            [ p_
                []
                [ text $ "Version " <> VERSION_ormolu <> ", commit ",
                  a_
                    [href_ $ "https://github.com/tweag/ormolu/commit/" <> $gitHash, target_ "blank"]
                    [span_ [class_ "is-family-code"] [text . ms . T.take 7 $ $gitHash]],
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
                [ div_
                    [class_ "editor"]
                    [ div_
                        [class_ "line-numbers"]
                        (replicate (editorLineNumbers . fromMisoString $ input) (span_ [] [])),
                      textarea_
                        [class_ "is-family-code", onInput SetInput, rows_ "20", autofocus_ True]
                        [text input]
                    ]
                ],
              div_
                [id_ "output", class_ "column is-half is-flex card is-shadowless is-radiusless"]
                [ out,
                  div_
                    [class_ "card-content is-overlay"]
                    [ button_
                        [class_ "button copy-output", data_ "clipboard-target" "#output", styleInline_ "left:90%;margin-right:20px;"]
                        [text "Copy"]
                    ]
                ]
            ]
        ]
          <> [ div_
                 [class_ "columns"]
                 [ pre_
                     [class_ "column is-half is-family-code"]
                     [ text . ms . prettyAST . fromMisoString $ input
                     ],
                   pre_
                     [class_ "column is-half is-family-code"]
                     [text . ms . prettyAST . T.unpack $ m | m <- toList output]
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
      Right t ->
        pre_ [class_ "is-family-code is-flex-grow-1"]
          . maybe (pure . text . ms $ t) (tokenToHtml <$>)
          . tokenizeHaskell
          $ t
      Left e ->
        pre_
          [class_ "is-flex-grow-1 content has-background-danger-light has-text-danger-dark"]
          [text . ms . showOrmoluException $ e]
    tokenToHtml (token, t) = span_ (maybeToList $ class_ <$> tokenClass token) [text $ ms t]
    tokenClass = \case
      KeywordTok -> Just "has-text-link"
      PragmaTok -> Just "has-text-grey"
      ConstructorTok -> Just "has-text-primary-dark"
      CharTok -> Just "has-text-success"
      StringTok -> Just "has-text-success"
      CommentTok -> Just "has-text-grey"
      OperatorTok -> Just "has-text-warning-dark"
      SymbolTok -> Just "has-text-warning-dark"
      _ -> Nothing

    showOrmoluException = \case
      Right oe ->
        unlines case oe of
          O.OrmoluParsingFailed s m ->
            [ "The GHC parser failed:",
              "",
              O.showOutputable s,
              "",
              m
            ]
          O.OrmoluOutputParsingFailed s m ->
            [ "Parsing of formatted code failed:",
              "",
              O.showOutputable s,
              "",
              m
            ]
          O.OrmoluASTDiffers _ ss ->
            [ "AST of input and AST of formatted code differ. Please, consider reporting the bug.",
              ""
            ]
              <> do O.showOutputable <$> ss
          O.OrmoluNonIdempotentOutput _ ->
            ["Formatting is not idempotent. Please, consider reporting the bug."]
          O.OrmoluUnrecognizedOpts os ->
            [ "The following GHC options were not recognized:",
              "",
              unwords $ toList os
            ]
          O.OrmoluCabalFileParsingFailed _ -> error "unreachable"
          O.OrmoluMissingStdinInputFile -> error "unreachable"
          O.OrmoluFixityOverridesParseError _ -> error "unreachable"
      Left e -> e

    prettyAST t = case parseModule t of
      Left e ->
        T.pack $ showOrmoluException e
      Right (_, Left (srcSpan, msg)) ->
        T.pack $ show (srcSpan, msg)
      Right (_, Right snippets) ->
        T.unlines . fmap printSnippet $ snippets
      where
        parseModule =
          unsafePerformIO
            . do mapped . _Left %~ extractOrmoluException
            . tryAny
            . O.parseModule configWithDeltas defaultFixityMap "<input>"
        configWithDeltas = O.regionIndicesToDeltas (length (lines t)) <$> config

        printSnippet = \case
          O.ParsedSnippet O.ParseResult {..} ->
            T.pack
              . showSDocUnsafe
              . Dump.showAstData Dump.NoBlankSrcSpan Dump.NoBlankEpAnnotations
              $ prParsedSource
          O.RawSnippet r -> r

    editorLineNumbers text = 1 + T.count "\n" text

extractOrmoluException :: SomeException -> Either String O.OrmoluException
extractOrmoluException = \case
  (fromException -> Just oe) -> Right oe
  e -> Left . displayException $ e

-- | The default fixity map, using the default value for the popularity
-- ratio threshold, and an empty list of dependencies.
defaultFixityMap :: LazyFixityMap
defaultFixityMap = buildFixityMap defaultStrategyThreshold mempty
