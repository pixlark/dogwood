{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser.Internal where

import AST (AST (..))
import qualified AST as A
import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import Error
import Lexer
import Text.Printf

data Parser = Parser {current :: Token, lexer :: Lexer, lastTokenEnd :: Int}
  deriving (Show)

type ParserM a = ExceptT ParseError (State Parser) a

advance :: ParserM ()
advance = ExceptT $ state $ \parser ->
  let (Token _ (Span lastTokenStart lastTokenLength)) = parser.current
      lastTokenEnd = lastTokenStart + lastTokenLength
   in case runState (runExceptT nextToken) parser.lexer of
        (Right tok, lexer') -> (Right (), parser {current = tok, lexer = lexer', lastTokenEnd})
        (Left e, lexer') -> (Left e, parser {lexer = lexer', lastTokenEnd})

makeParser :: Lexer -> Result Parser
makeParser lexer = parser' <$ result
  where
    parser = Parser {current = Token {kind = Eof, span = Span 0 0}, lexer, lastTokenEnd = 0}
    -- "prime the pump"
    (result, parser') = runState (runExceptT advance) parser

throwSpan :: Span -> ParseErrorKind -> ParserM a
throwSpan span kind = throwE $ ParseError kind span

expectKeyword :: Text -> ParserM ()
expectKeyword keyword = do
  current <- gets current
  if current.kind == Keyword keyword
    then advance
    else throwSpan (current.span) $ ExpectedKeyword keyword

expectGlyph :: Text -> ParserM ()
expectGlyph glyph = do
  current <- gets current
  if current.kind == Glyph glyph
    then advance
    else throwSpan (current.span) $ ExpectedGlyph glyph

readSymbol :: ParserM (AST Text)
readSymbol = produceSpannedAST $ do
  current <- gets current
  case current.kind of
    Symbol sym -> do advance; returnWrap sym
    _ -> throwSpan (current.span) $ ExpectedSymbol

matchKeyword :: Text -> ParserM Bool
matchKeyword keyword = do
  current <- gets current
  if current.kind == Keyword keyword
    then do advance; return True
    else return False

matchGlyph :: Text -> ParserM Bool
matchGlyph glyph = do
  current <- gets current
  if current.kind == Glyph glyph
    then do advance; return True
    else return False

spanStart :: ParserM Int
spanStart = do
  (Token _ (Span start _)) <- gets current
  return start

makeSpan :: Int -> ParserM Span
makeSpan start = do
  end <- gets lastTokenEnd
  return $ Span start (end - start)

data SeparatorConfig a = SeparatorConfig
  { trailing :: Bool,
    separator :: TokenKind,
    consume :: ParserM (Maybe a)
  }

parseSeparatedSequence :: forall a. SeparatorConfig a -> ParserM [a]
parseSeparatedSequence SeparatorConfig {trailing, separator, consume} = parseSeparatedSequence' [] False
  where
    parseSeparatedSequence' :: [a] -> Bool -> ParserM [a]
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

parseNamespacedIdentifier :: ParserM A.ValueTypeExpr
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

returnDirect :: AST a -> ParserM (MaybeSpanned a)
returnDirect = return . direct

returnWrap :: a -> ParserM (MaybeSpanned a)
returnWrap = return . wrap

produceSpannedAST :: ParserM (MaybeSpanned a) -> ParserM (AST a)
produceSpannedAST f = do
  spanStart <- spanStart
  value <- f
  span <- makeSpan spanStart
  case value of
    AlreadySpanned x -> return x
    NotSpanned x -> return $ AST x span

parseTypeExpr :: ParserM (AST A.TypeExpr)
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

parseAtom :: ParserM (AST A.Expr)
parseAtom = produceSpannedAST $ do
  current <- gets current
  case current.kind of
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

parsePostfix :: ParserM (AST A.Expr)
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
                  closeParen <- matchGlyph ")"
                  if closeParen
                    then return Nothing
                    else do
                      expr <- parseExpr
                      return $ Just expr
              }
          )
      returnWrap $ A.FunctionCall left arguments

parseUnary :: ParserM (AST A.Expr)
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

parseBinary :: ParserM (AST A.Expr) -> [(TokenKind, AST A.Expr -> AST A.Expr -> A.Expr)] -> ParserM (AST A.Expr)
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

parseBinaryMultiplicative :: ParserM (AST A.Expr)
parseBinaryMultiplicative =
  parseBinary
    parseUnary
    [ (Glyph "*", A.BinaryOperator A.Multiply),
      (Glyph "/", A.BinaryOperator A.Divide)
    ]

parseBinaryAdditive :: ParserM (AST A.Expr)
parseBinaryAdditive =
  parseBinary
    parseBinaryMultiplicative
    [ (Glyph "+", A.BinaryOperator A.Plus),
      (Glyph "-", A.BinaryOperator A.Minus)
    ]

parseBinaryComparison :: ParserM (AST A.Expr)
parseBinaryComparison =
  parseBinary
    parseBinaryAdditive
    [ (Glyph "<", A.BinaryOperator A.LessThan),
      (Glyph "<=", A.BinaryOperator A.LessThanOrEqual),
      (Glyph ">", A.BinaryOperator A.GreaterThan),
      (Glyph ">=", A.BinaryOperator A.GreaterThanOrEqual)
    ]

parseBinaryEquality :: ParserM (AST A.Expr)
parseBinaryEquality =
  parseBinary
    parseBinaryComparison
    [ (Glyph "==", A.BinaryOperator A.Equal),
      (Glyph "!=", A.BinaryOperator A.NotEqual)
    ]

parseBinaryAnd :: ParserM (AST A.Expr)
parseBinaryAnd =
  parseBinary
    parseBinaryEquality
    [ (Glyph "&&", A.BinaryOperator A.And)
    ]

parseBinaryOr :: ParserM (AST A.Expr)
parseBinaryOr =
  parseBinary
    parseBinaryAnd
    [ (Glyph "||", A.BinaryOperator A.Or)
    ]

parseIfExpr :: ParserM (AST A.Expr)
parseIfExpr = produceSpannedAST $ do
  (bodies, elseBody) <- parseIfPart
  returnWrap $ A.IfChain bodies elseBody
  where
    parseIfPart :: ParserM (NonEmpty (AST A.Expr, AST A.Expr), Maybe (AST A.Expr))
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

parseExpr :: ParserM (AST A.Expr)
parseExpr = do
  cur <- gets current
  case cur.kind of
    Keyword "if" -> do parseIfExpr
    Glyph "{" -> do
      body <- parseBody
      return $ A.ExprBody <$> body
    _ -> parseBinaryOr

parseLet :: ParserM (AST A.Stmt)
parseLet = produceSpannedAST $ do
  expectKeyword "let"
  name <- readSymbol
  expectGlyph ":"
  type_ <- parseTypeExpr
  expectGlyph "="
  value <- parseExpr
  returnWrap $ A.Let {name, type_, value}

attempt :: ParserM (Maybe a) -> ParserM (Maybe a)
attempt f = do
  parser <- get
  let (result, parser') = run parser
  case result of
    Left e -> do put parser'; throwE e
    Right Nothing -> do return Nothing
    Right j -> do put parser'; return j
  where
    run = runState $ runExceptT f

tryParseLValue :: ParserM (Maybe (AST A.LValue))
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

parseBody :: ParserM (AST A.Body)
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

parseStmt :: ParserM (AST A.Stmt)
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

runParse :: Text -> ParserM a -> Result a
runParse source f = case makeParser lexer of
  Left e -> Left e
  Right parser -> run parser
  where
    lexer = makeLexer source
    run = evalState $ runExceptT f
