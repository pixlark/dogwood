{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module DW.LSP where

import Control.Monad (join)
import DW.AST (AST (..))
import DW.AST qualified as A
import DW.AST.Visit qualified as AV
import DW.Common
import DW.Error (displayError, displayErrorColorless)
import DW.TypedAST (TST (..))
import DW.TypedAST qualified as T
import DW.TypedAST.Visit qualified as TV
import DW.Util (safeHead, (<$$>))
import Data.Aeson qualified as J
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.SortedList qualified as SortedList
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import GHC.Generics (Generic)
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

type Runner = IO (Either (Text, [Err]) (Text, AST A.Stmt, TST T.Stmt))

newtype UserConfig = UserConfig {serverPath :: Maybe Text}
  deriving (Generic, J.ToJSON, J.FromJSON, Show)

inSpan :: Int -> Span -> Bool
inSpan pos (Span start len) = pos >= start && pos < start + len

typeOfVariableAtSpan :: TST T.Stmt -> Span -> Maybe T.TypeExpr
typeOfVariableAtSpan tStmt span = runPureEff $ execState Nothing $ TV.runStmtVisitor visitor tStmt
  where
    visitor :: (State (Maybe T.TypeExpr) :> es) => TV.Visitor (Eff es)
    visitor = TV.defaultVisitor {TV.onStmt, TV.onExpr, TV.onLValue}
    onStmt (T.TST (T.Let {type_ = T.TST ty _, name = T.TST _ nSpan}) _) recurse = do
      when (span == nSpan) do
        put $ Just ty
      recurse
    onStmt _ recurse = recurse
    onExpr (T.TST (T.Variable ty _) eSpan) recurse = do
      when (span == eSpan) do
        put $ Just ty
      recurse
    onExpr _ recurse = recurse
    onLValue (T.TST (T.LVariable ty _) lSpan) recurse = do
      when (span == lSpan) do
        put $ Just ty
      recurse

variableAtPosition :: AST A.Stmt -> Int -> Maybe (AST Text)
variableAtPosition stmt pos = runPureEff $ execState Nothing $ AV.runStmtVisitor visitor stmt
  where
    visitor :: (State (Maybe (AST Text)) :> es) => AV.Visitor (Eff es)
    visitor = AV.defaultVisitor {AV.onStmt, AV.onExpr, AV.onLValue}
    onStmt (A.AST (A.Let {name = name@(A.AST _ span)}) _) recurse = do
      when (pos `inSpan` span) do
        traceShow (A.spanOf name) put $ Just name
      recurse
    onStmt _ recurse = recurse
    onExpr (A.AST (A.Variable name) span) recurse = do
      when (pos `inSpan` span) do
        put $ Just (A.AST name span)
      recurse
    onExpr _ recurse = recurse
    onLValue (A.AST (A.LVariable name) span) recurse = do
      when (pos `inSpan` span) do
        put $ Just (A.AST name span)
      recurse

typeExprAtPosition :: AST A.Stmt -> Int -> Maybe (AST A.TypeExpr)
typeExprAtPosition stmt pos = safeHead $ runPureEff $ execState [] $ AV.runStmtVisitor visitor stmt
  where
    visitor :: (State [AST A.TypeExpr] :> es) => AV.Visitor (Eff es)
    visitor = AV.defaultVisitor {AV.onTypeExpr}
    onTypeExpr typeExpr@(A.AST _ span) recurse = do
      when (pos `inSpan` span) do
        modify (typeExpr :)
      recurse

italic :: Text -> Text
italic t = "*" <> t <> "*"

bold :: Text -> Text
bold t = "**" <> t <> "**"

handleHover :: AST A.Stmt -> TST T.Stmt -> Int -> Maybe Text
handleHover stmt tStmt pos = tryTypeExpr <|> tryVariable
  where
    -- first, check if they're hovering a type expression, in which case
    -- we can just display information about that type
    tryTypeExpr = italic . Text.show <$> typeExprAtPosition stmt pos
    -- then, check if they're hovering a variable, in which case we figure out
    -- what type that variable is and display it
    tryVariable = do
      A.AST name span <- variableAtPosition stmt pos
      ty <- typeOfVariableAtSpan tStmt span
      return $ LazyText.toStrict $ format "{} : {}" (bold name, italic $ Text.show ty)

lspPositionToAbsolutePosition :: Text -> Position -> Maybe Int
lspPositionToAbsolutePosition source (Position line ch) =
  (+)
    <$> moveToLine (fromIntegral line) 0
    <*> Just (fromIntegral ch)
  where
    moveToLine :: Int -> Int -> Maybe Int
    moveToLine 0 start = Just start
    moveToLine n start | n > 0 = do
      idx <- Text.findIndex (== '\n') (Text.drop start source)
      moveToLine (n - 1) (start + idx + 1)
    moveToLine _ _ = Nothing

absolutePositionToLspPosition :: Text -> Int -> Maybe Position
absolutePositionToLspPosition _ pos | pos < 0 = Nothing
absolutePositionToLspPosition source pos = Just $ tupToPos $ moveToPos pos 0 (0, 0)
  where
    moveToPos :: Int -> Int -> (Int, Int) -> (Int, Int)
    moveToPos to n tup | to == n = tup
    moveToPos to n (line, ch)
      | to > 0 =
          moveToPos to (n + 1) $
            if source `Text.index` n == '\n' then (line + 1, 0) else (line, ch + 1)
    moveToPos _ _ _ = error "unreachable"
    tupToPos (line, ch) = Position (fromIntegral line) (fromIntegral ch)

sendDiagnostics :: Uri -> LspT (Runner, UserConfig) IO ()
sendDiagnostics file = do
  (runner, _) <- traceShow file getConfig
  maybeRunResult <- liftIO runner
  case maybeRunResult of
    Left (source, errs) -> do
      diagnostics <- forM errs $ \err@(Err _ (Span spanStart spanLen)) -> do
        let range =
              -- Range at which the message applies
              traceShow (spanStart, spanLen) Range
                <$> absolutePositionToLspPosition source spanStart
                <*> absolutePositionToLspPosition source (spanStart + spanLen)
        case traceShowId range of
          Nothing -> return []
          Just range -> do
            let
              severity = Just DiagnosticSeverity_Error
              code = Nothing
              codeDescription = Nothing
              source_ = Nothing
              message = displayErrorColorless source err -- Text.show kind
              tags = Nothing
              relatedInformation = Nothing
              data_ = Nothing
              diagnostic = Diagnostic range severity code codeDescription source_ message tags relatedInformation data_
            return [diagnostic]
      publish (concat diagnostics)
    Right _ -> trace "publishing zero diagnostics!" publish []
  where
    publish diagnostics =
      let diagnosticsBySource = Map.singleton Nothing (SortedList.toSortedList diagnostics)
       in publishDiagnostics 100 (toNormalizedUri file) Nothing diagnosticsBySource

handlers :: Handlers (LspM (Runner, UserConfig))
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_ -> do
        return (),
      requestHandler SMethod_TextDocumentHover $ \req responder -> do
        (runner, _) <- getConfig
        maybeRunResult <- liftIO runner
        let result = case maybeRunResult of
              Left _ -> InR Null
              Right (source, ast, typedAST) -> do
                let TRequestMessage _ _ _ (HoverParams _ pos _) = req
                    realPos = lspPositionToAbsolutePosition source pos
                    responseText = traceShow (pos, realPos) join $ handleHover ast typedAST <$> realPos
                case responseText of
                  Nothing -> InR Null
                  Just responseText -> InL $ Hover (InL $ mkMarkdown responseText) Nothing
        responder $ Right result,
      notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
        let TNotificationMessage _ _ (DidOpenTextDocumentParams doc) = msg
        sendDiagnostics doc._uri,
      notificationHandler SMethod_TextDocumentDidSave $ \msg -> do
        let TNotificationMessage _ _ (DidSaveTextDocumentParams doc _) = msg
        trace "hello" sendDiagnostics doc._uri,
      notificationHandler SMethod_WorkspaceDidChangeWatchedFiles $ \msg -> do
        let TNotificationMessage _ _ (DidChangeWatchedFilesParams files) = msg
        forM_ files $ \file -> do
          sendDiagnostics file._uri
    ]

run :: Runner -> IO ExitCode
run runner = do
  let serverDefinition =
        ServerDefinition
          { defaultConfig = (runner, UserConfig {serverPath = Nothing}),
            parseConfig = \_old v -> do
              (runner,) <$> case J.fromJSON v of
                J.Error e -> Left (Text.pack e)
                J.Success cfg -> Right cfg,
            onConfigChange = const $ pure (),
            configSection = "dogwood",
            doInitialize = \env _ -> pure $ Right env,
            staticHandlers = const handlers,
            interpretHandler = \env -> Iso (runLspT env) liftIO,
            options = defaultOptions
          }
  exitCode <- runServer serverDefinition
  return case exitCode of
    0 -> ExitSuccess
    i -> ExitFailure i
