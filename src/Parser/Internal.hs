{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Parser.Internal where

import AST (AST (..))
import qualified AST as A
import Control.Monad (forM, msum)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import Effectful (Eff, runPureEff, (:>))
import Effectful.Error.Static (CallStack, Error, HasCallStack, runError, runErrorNoCallStack, throwError, tryError)
import Effectful.State.Static.Local (State, evalState, get, gets, modify, put, runState, state)
import qualified Effectful.State.Static.Local
import Error
import Lexer
import Text.Printf

data Parser = Parser {current :: Token, lexer :: Lexer, lastTokenEnd :: Int}
  deriving (Show)

-- type ParserM a = ExceptT Err (State Parser) a
type ParserE a = Eff '[State Parser, Error Err] a

advance :: (State Parser :> es, Error Err :> es) => Eff es ()
advance = do
  parser <- get
  let (Token _ (Span lastTokenStart lastTokenLength)) = parser.current
      lastTokenEnd = lastTokenStart + lastTokenLength
  (token, lexer') <- runState parser.lexer nextToken
  modify (\p -> p {current = token, lexer = lexer', lastTokenEnd})
  return ()

makeParser :: Lexer -> Result Parser
makeParser lexer = parser' <$ result
  where
    parser = Parser {current = Token {kind = Eof, span = Span 0 0}, lexer, lastTokenEnd = 0}
    -- "prime the pump"
    (result, parser') = runPureEff $ runState parser $ runErrorNoCallStack advance

makeParserCallStack :: Lexer -> Either (CallStack, Err) Parser
makeParserCallStack lexer = parser' <$ result
  where
    parser = Parser {current = Token {kind = Eof, span = Span 0 0}, lexer, lastTokenEnd = 0}
    -- "prime the pump"
    (result, parser') = runPureEff $ runState parser $ runError advance

throwSpan :: (HasCallStack, Error Err :> es) => Span -> ErrorKind -> Eff es a
throwSpan span kind = throwError $ Err kind span

expectKeyword :: (State Parser :> es, Error Err :> es) => Text -> Eff es ()
expectKeyword keyword = do
  current <- gets current
  if current.kind == Keyword keyword
    then advance
    else throwSpan (current.span) $ ExpectedKeyword keyword

expectGlyph :: (State Parser :> es, Error Err :> es) => Text -> Eff es ()
expectGlyph glyph = do
  current <- gets current
  if current.kind == Glyph glyph
    then advance
    else throwSpan (current.span) $ ExpectedGlyph glyph

readSymbol :: (State Parser :> es, Error Err :> es) => Eff es (AST Text)
readSymbol = produceSpannedAST $ do
  current <- gets current
  case current.kind of
    Symbol sym -> do advance; returnWrap sym
    _ -> throwSpan (current.span) $ ExpectedSymbol

matchKeyword :: (State Parser :> es, Error Err :> es) => Text -> Eff es Bool
matchKeyword keyword = do
  current <- gets current
  if current.kind == Keyword keyword
    then do advance; return True
    else return False

matchGlyph :: (State Parser :> es, Error Err :> es) => Text -> Eff es Bool
matchGlyph glyph = do
  current <- gets current
  if current.kind == Glyph glyph
    then do advance; return True
    else return False

spanStart :: (State Parser :> es) => Eff es Int
spanStart = do
  (Token _ (Span start _)) <- gets current
  return start

makeSpan :: (State Parser :> es) => Int -> Eff es Span
makeSpan start = do
  end <- gets lastTokenEnd
  return $ Span start (end - start)

data SeparatorConfig es a = SeparatorConfig
  { trailing :: Bool,
    separator :: TokenKind,
    consume :: Eff es (Maybe a)
  }

parseSeparatedSequence :: (State Parser :> es, Error Err :> es) => SeparatorConfig es a -> Eff es [a]
parseSeparatedSequence SeparatorConfig {trailing, separator, consume} = parseSeparatedSequence' [] False
  where
    -- todo: can't seem to make this type signature work with ScopedTypeVariables
    -- parseSeparatedSequence' :: (State Parser :> es, Error Err :> es) => [a] -> Bool -> Eff es [a]
    parseSeparatedSequence' sequence expecting = do
      consumed <- consume
      current <- gets current
      case consumed of
        Nothing ->
          if expecting
            then throwSpan (current.span) ExpectedAnotherElementOfSequence
            else
              if trailing && current.kind == separator
                then do advance; return sequence
                else return sequence
        Just x ->
          let sequence' = sequence ++ [x]
           in if current.kind == separator
                then do
                  advance
                  parseSeparatedSequence' sequence' (not trailing)
                else return sequence'

parseNamespacedIdentifier :: (State Parser :> es, Error Err :> es) => Eff es A.ValueTypeExpr
parseNamespacedIdentifier = do
  pieces <- parseSeparatedSequence SeparatorConfig {trailing = False, separator = Glyph "::", consume = consumeSymbol}
  return $ A.NamespacedIdentifier pieces
  where
    consumeSymbol = do
      current <- gets current
      case current.kind of
        Symbol sym -> do advance; return (Just sym)
        _ -> return Nothing

data MaybeSpanned a = AlreadySpanned (AST a) | NotSpanned a

direct :: AST a -> MaybeSpanned a
direct = AlreadySpanned

wrap :: a -> MaybeSpanned a
wrap = NotSpanned

returnDirect :: AST a -> Eff es (MaybeSpanned a)
returnDirect = return . direct

returnWrap :: a -> Eff es (MaybeSpanned a)
returnWrap = return . wrap

produceSpannedAST :: (State Parser :> es) => Eff es (MaybeSpanned a) -> Eff es (AST a)
produceSpannedAST f = do
  spanStart <- spanStart
  value <- f
  span <- makeSpan spanStart
  case value of
    AlreadySpanned x -> return x
    NotSpanned x -> return $ AST x span

parseTypeExpr :: (HasCallStack, State Parser :> es, Error Err :> es) => Eff es (AST A.TypeExpr)
parseTypeExpr = produceSpannedAST $ do
  reference <- matchGlyph "&"
  current <- gets current
  valueExpr <- case current.kind of
    Keyword "void" -> do advance; return A.Void
    Keyword "bool" -> do advance; return A.Bool
    Keyword "int" -> do advance; return A.Int
    Symbol _ -> parseNamespacedIdentifier
    _ -> throwSpan (current.span) ExpectedTypeExpr
  return $ NotSpanned A.TypeExpr {reference, valueExpr}

parseAtom :: (HasCallStack, State Parser :> es, Error Err :> es) => Eff es (AST A.Expr)
parseAtom = produceSpannedAST $ do
  current <- gets current
  case current.kind of
    Keyword "undefined" -> do advance; returnWrap A.UndefinedLit
    Keyword "void" -> do advance; returnWrap A.VoidLit
    Keyword "true" -> do advance; returnWrap (A.BoolLit True)
    Keyword "false" -> do advance; returnWrap (A.BoolLit False)
    IntLiteral n -> do advance; returnWrap (A.IntLit n)
    Symbol sym -> do advance; returnWrap (A.Variable sym)
    Glyph "(" -> do
      advance
      (AST expr _) <- parseExpr
      expectGlyph ")"
      returnWrap expr
    _ -> throwSpan (current.span) ExpectedExpr

parsePostfix :: (HasCallStack, State Parser :> es, Error Err :> es) => Eff es (AST A.Expr)
parsePostfix = produceSpannedAST $ do
  left <- parseAtom
  openParen <- matchGlyph "("
  if not openParen
    then returnDirect left
    else do
      arguments <-
        parseSeparatedSequence
          ( SeparatorConfig
              { trailing = True,
                separator = Glyph ",",
                consume = do
                  cur <- gets current
                  if cur.kind == Glyph ")"
                    then return Nothing
                    else do
                      expr <- parseExpr
                      return $ Just expr
              }
          )
      expectGlyph ")"
      returnWrap $ A.FunctionCall left arguments

parseUnary :: (HasCallStack, State Parser :> es, Error Err :> es) => Eff es (AST A.Expr)
parseUnary = produceSpannedAST $ do
  current <- gets current
  let operator =
        case current.kind of
          Glyph "+" -> Just A.Plus
          Glyph "-" -> Just A.Minus
          Glyph "!" -> Just A.Not
          _ -> Nothing
  case operator of
    Just op -> do
      advance
      inner <- parseUnary
      returnWrap $ A.UnaryOperator op inner
    _ -> do expr <- parsePostfix; returnDirect expr

parseBinary ::
  (State Parser :> es, Error Err :> es) =>
  Eff es (AST A.Expr) ->
  [(TokenKind, AST A.Expr -> AST A.Expr -> A.Expr)] ->
  Eff es (AST A.Expr)
parseBinary nextParser operators = produceSpannedAST $ do
  left <- nextParser
  current <- gets current
  results <- forM operators $ \(operator, combiner) ->
    if current.kind == operator
      {- HLINT ignore -}
      then do
        advance
        right <- recurse
        return $ Just $ wrap $ combiner left right
      else return Nothing
  case msum results of
    Just operator -> return operator
    Nothing -> returnDirect left
  where
    recurse = parseBinary nextParser operators

parseBinaryMultiplicative :: (HasCallStack, State Parser :> es, Error Err :> es) => Eff es (AST A.Expr)
parseBinaryMultiplicative =
  parseBinary
    parseUnary
    [ (Glyph "*", A.BinaryOperator A.Multiply),
      (Glyph "/", A.BinaryOperator A.Divide)
    ]

parseBinaryAdditive :: (HasCallStack, State Parser :> es, Error Err :> es) => Eff es (AST A.Expr)
parseBinaryAdditive =
  parseBinary
    parseBinaryMultiplicative
    [ (Glyph "+", A.BinaryOperator A.Plus),
      (Glyph "-", A.BinaryOperator A.Minus)
    ]

parseBinaryComparison :: (HasCallStack, State Parser :> es, Error Err :> es) => Eff es (AST A.Expr)
parseBinaryComparison =
  parseBinary
    parseBinaryAdditive
    [ (Glyph "<", A.BinaryOperator A.LessThan),
      (Glyph "<=", A.BinaryOperator A.LessThanOrEqual),
      (Glyph ">", A.BinaryOperator A.GreaterThan),
      (Glyph ">=", A.BinaryOperator A.GreaterThanOrEqual)
    ]

parseBinaryEquality :: (HasCallStack, State Parser :> es, Error Err :> es) => Eff es (AST A.Expr)
parseBinaryEquality =
  parseBinary
    parseBinaryComparison
    [ (Glyph "==", A.BinaryOperator A.Equal),
      (Glyph "!=", A.BinaryOperator A.NotEqual)
    ]

parseBinaryAnd :: (HasCallStack, State Parser :> es, Error Err :> es) => Eff es (AST A.Expr)
parseBinaryAnd =
  parseBinary
    parseBinaryEquality
    [ (Glyph "&&", A.BinaryOperator A.And)
    ]

parseBinaryOr :: (HasCallStack, State Parser :> es, Error Err :> es) => Eff es (AST A.Expr)
parseBinaryOr =
  parseBinary
    parseBinaryAnd
    [ (Glyph "||", A.BinaryOperator A.Or)
    ]

parseIfExpr :: (HasCallStack, State Parser :> es, Error Err :> es) => Eff es (AST A.Expr)
parseIfExpr = produceSpannedAST $ do
  (bodies, elseBody) <- parseIfPart
  returnWrap $ A.IfChain bodies elseBody
  where
    parseIfPart :: (State Parser :> es, Error Err :> es) => Eff es (NonEmpty (AST A.Expr, AST A.Expr), Maybe (AST A.Expr))
    parseIfPart = do
      expectKeyword "if"
      condition <- parseExpr
      body <- parseExpr
      hasElse <- matchKeyword "else"
      if hasElse
        then do
          cur <- gets current
          if cur.kind == Keyword "if"
            then do
              (bodies, elseBody) <- parseIfPart
              let bodies' = (condition, body) <| bodies
              return (bodies', elseBody)
            else do
              elseBody <- parseExpr
              return (NE.singleton (condition, body), Just elseBody)
        else return (NE.singleton (condition, body), Nothing)

parseExpr :: (HasCallStack, State Parser :> es, Error Err :> es) => Eff es (AST A.Expr)
parseExpr = do
  cur <- gets current
  case cur.kind of
    Keyword "if" -> do parseIfExpr
    Glyph "{" -> do
      body <- parseBody
      return $ A.ExprBody <$> body
    _ -> parseBinaryOr

parseLet :: (HasCallStack, State Parser :> es, Error Err :> es) => Eff es (AST A.Stmt)
parseLet = produceSpannedAST $ do
  expectKeyword "let"
  name <- readSymbol
  expectGlyph ":"
  type_ <- parseTypeExpr
  expectGlyph "="
  value <- parseExpr
  returnWrap $ A.Let {name, type_, value}

attempt :: (HasCallStack, State Parser :> es, Error Err :> es) => Eff es (Maybe a) -> Eff es (Maybe a)
attempt f = do
  parser <- get
  (result, parser') <- run
  case result of
    Left (_, e) -> do put parser'; throwError e
    Right Nothing -> do put parser; return Nothing
    Right j -> do put parser'; return j
  where
    run = do
      result <- tryError f
      parser' <- get
      return (result, parser')

tryParseLValue :: (HasCallStack, State Parser :> es, Error Err :> es) => Eff es (Maybe (AST A.LValue))
tryParseLValue =
  fmap invert $ produceSpannedAST $ do
    cur <- gets current
    case cur.kind of
      Symbol sym -> do
        advance
        cur <- gets current
        if cur.kind == Glyph "="
          then
            returnWrap $ Just $ A.LVariable sym
          else returnWrap Nothing
      _ -> returnWrap Nothing
  where
    invert (AST (Just x) span) = Just (AST x span)
    invert (AST Nothing _) = Nothing

parseBody :: (HasCallStack, State Parser :> es, Error Err :> es) => Eff es (AST A.Body)
parseBody = produceSpannedAST $ do
  expectGlyph "{"
  stmts <- parseStmts []
  returnWrap $ A.Body stmts
  where
    parseStmts stmts = do
      matched <- matchGlyph "}"
      if matched
        then return stmts
        else do
          stmt <- parseStmt
          parseStmts $ stmts ++ [stmt]

parseStmt :: (HasCallStack, State Parser :> es, Error Err :> es) => Eff es (AST A.Stmt)
parseStmt = produceSpannedAST $ do
  cur <- gets current
  case cur.kind of
    Keyword "let" -> do
      let_ <- parseLet
      expectGlyph ";"
      returnDirect let_
    Keyword "return" -> do
      advance
      matched <- matchGlyph ";"
      if matched
        then returnWrap $ A.Return Nothing
        else do
          value <- parseExpr
          expectGlyph ";"
          returnWrap $ A.Return $ Just value
    Keyword "break" -> do
      advance
      expectGlyph ";"
      returnWrap A.Break
    Keyword "loop" -> do
      advance
      body <- parseBody
      returnWrap $ A.Loop body
    _ -> do
      -- clone the parser and attempt to parse an lvalue followed by an equals sign
      -- if it works, we continue with that parser
      -- otherwise, we abort and continue with our existing parser
      -- this basically is just an easy way to get some lookahead
      -- TODO: make sure that Data.Text optimizes the clone of the source into a pointer
      --       copy instead of literally copying all the text over
      lvalue <- attempt tryParseLValue
      case lvalue of
        Just lvalue -> do
          expectGlyph "="
          value <- parseExpr
          expectGlyph ";"
          returnWrap $ A.Assign {lvalue, value}
        Nothing -> do
          expr <- parseExpr
          semicolon <- matchGlyph ";"
          -- semicolon <- case expr of
          --   -- allow omitting the semicolon after certain expressions that
          --   -- get used like statements
          --   AST (A.IfChain _ _) _ -> matchGlyph ";"
          --   AST (A.ExprBody _) _ -> matchGlyph ";"
          --   _ -> do expectGlyph ";"; return True
          returnWrap $ A.ExprStmt {value = expr, semicolon}

runParse :: Text -> ParserE a -> Result a
runParse source f = case makeParser lexer of
  Left e -> Left e
  Right parser -> run parser
  where
    lexer = makeLexer source
    run p = runPureEff $ runErrorNoCallStack $ evalState p f

runParseCallStack :: Text -> ParserE a -> Either (CallStack, Err) a
runParseCallStack source f = case makeParserCallStack lexer of
  Left e -> Left e
  Right parser -> run parser
  where
    lexer = makeLexer source
    run p = runPureEff $ runError $ evalState p f
