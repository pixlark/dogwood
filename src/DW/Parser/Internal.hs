module DW.Parser.Internal where

import DW.AST (AST (..))
import DW.AST qualified as A
import DW.Common
import DW.Error.Internal.ErrorsEffect (throwErrsWithCallStacks)
import DW.Lexer.Internal (Lexer, Token (..), TokenKind (..), makeLexer, nextToken)
import DW.Util (ifM, whenM, (<$$>))

import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.List.NonEmpty qualified as NE

data Parser = Parser {current :: Token, lexer :: Lexer, lastTokenEnd :: Int}
  deriving (Show)

advance :: (State Parser :> es, Errors Err :> es) => Eff es ()
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
    (result, parser') = runPureEff $ runState parser $ runErrorsNoCallStack advance

makeParserCallStack :: Lexer -> Either [(CallStack, Err)] Parser
makeParserCallStack lexer = parser' <$ result
  where
    parser = Parser {current = Token {kind = Eof, span = Span 0 0}, lexer, lastTokenEnd = 0}
    -- "prime the pump"
    (result, parser') = runPureEff $ runState parser $ runErrors advance

expectKeyword :: (State Parser :> es, Errors Err :> es) => Text -> Eff es ()
expectKeyword keyword = do
  current <- gets current
  if current.kind == Keyword keyword
    then advance
    else throwSpan current.span $ ExpectedKeyword keyword

expectGlyph :: (State Parser :> es, Errors Err :> es) => Text -> Eff es ()
expectGlyph glyph = do
  current <- gets current
  if current.kind == Glyph glyph
    then advance
    else throwSpan current.span $ ExpectedGlyph glyph

readSymbol :: (State Parser :> es, Errors Err :> es) => Eff es (AST Text)
readSymbol = produceSpannedAST $ do
  current <- gets current
  case current.kind of
    Symbol sym -> do advance; returnWrap sym
    _ -> throwSpan current.span ExpectedSymbol

tryReadSymbol :: (State Parser :> es, Errors Err :> es) => Eff es (Maybe (AST Text))
tryReadSymbol =
  sequence <$> produceSpannedAST do
    current <- gets current
    case current.kind of
      Symbol sym -> do advance; returnWrap (Just sym)
      _ -> returnWrap Nothing

matchKeyword :: (State Parser :> es, Errors Err :> es) => Text -> Eff es Bool
matchKeyword keyword = do
  current <- gets current
  if current.kind == Keyword keyword
    then do advance; return True
    else return False

matchGlyph :: (State Parser :> es, Errors Err :> es) => Text -> Eff es Bool
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

parseSeparatedSequence :: (State Parser :> es, Errors Err :> es) => SeparatorConfig es a -> Eff es [a]
parseSeparatedSequence SeparatorConfig {trailing, separator, consume} = parseSeparatedSequence' [] False
  where
    -- todo: can't seem to make this type signature work with ScopedTypeVariables
    -- parseSeparatedSequence' :: (State Parser :> es, Errors Err :> es) => [a] -> Bool -> Eff es [a]
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

parseNamespacedIdentifier :: (State Parser :> es, Errors Err :> es) => Eff es A.ValueTypeExpr
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

parseTypeExpr :: (HasCallStack, State Parser :> es, Errors Err :> es) => Eff es (AST A.TypeExpr)
parseTypeExpr = produceSpannedAST $ do
  reference <- matchGlyph "&"
  cur <- gets current
  valueExpr <- case cur.kind of
    Keyword "any" -> do advance; return A.Any
    Keyword "void" -> do advance; return A.Void
    Keyword "bool" -> do advance; return A.Bool
    Keyword "int" -> do advance; return A.Int
    Keyword "fn" -> do
      advance
      expectGlyph "("
      paramTypes <-
        parseSeparatedSequence
          ( SeparatorConfig
              { trailing = True,
                separator = Glyph ",",
                consume = do
                  cur <- gets current
                  if cur.kind == Glyph ")"
                    then return Nothing
                    else do expr <- parseTypeExpr; return (Just expr)
              }
          )
      expectGlyph ")"
      expectGlyph "->"
      retType <- parseTypeExpr
      return $ A.Function paramTypes retType
    Symbol _ -> parseNamespacedIdentifier
    _ -> throwSpan cur.span ExpectedTypeExpr
  return $ NotSpanned A.TypeExpr {reference, valueExpr}

parseAtom :: (HasCallStack, State Parser :> es, Errors Err :> es) => Eff es (AST A.Expr)
parseAtom = produceSpannedAST $ do
  current <- gets current
  case current.kind of
    Keyword "void" -> do advance; returnWrap A.VoidLit
    Keyword "true" -> do advance; returnWrap (A.BoolLit True)
    Keyword "false" -> do advance; returnWrap (A.BoolLit False)
    IntLiteral n -> do advance; returnWrap (A.IntLit n)
    Symbol sym -> do advance; returnWrap (A.Variable sym)
    Keyword "if" -> parseIfExpr >>= returnDirect
    Keyword "fn" -> parseLambda >>= returnDirect
    Glyph "(" -> do
      advance
      (AST expr _) <- parseExpr
      expectGlyph ")"
      returnWrap expr
    _ -> throwSpan current.span ExpectedExpr

parsePostfix :: (HasCallStack, State Parser :> es, Errors Err :> es) => Eff es (AST A.Expr)
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

parseUnary :: (HasCallStack, State Parser :> es, Errors Err :> es) => Eff es (AST A.Expr)
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

{- HLINT ignore "Use <$>" -}
parseBinary ::
  (State Parser :> es, Errors Err :> es)
  => Eff es (AST A.Expr)
  -> [(TokenKind, AST A.Expr -> AST A.Expr -> A.Expr)]
  -> Eff es (AST A.Expr)
parseBinary nextParser operators = produceSpannedAST $ do
  left <- nextParser
  current <- gets current
  results <- forM operators $ \(operator, combiner) ->
    if current.kind == operator
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

parseBinaryMultiplicative :: (HasCallStack, State Parser :> es, Errors Err :> es) => Eff es (AST A.Expr)
parseBinaryMultiplicative =
  parseBinary
    parseUnary
    [ (Glyph "*", A.BinaryOperator A.Multiply),
      (Glyph "/", A.BinaryOperator A.Divide),
      (Glyph "%", A.BinaryOperator A.Modulo)
    ]

parseBinaryAdditive :: (HasCallStack, State Parser :> es, Errors Err :> es) => Eff es (AST A.Expr)
parseBinaryAdditive =
  parseBinary
    parseBinaryMultiplicative
    [ (Glyph "+", A.BinaryOperator A.Plus),
      (Glyph "-", A.BinaryOperator A.Minus)
    ]

parseBinaryComparison :: (HasCallStack, State Parser :> es, Errors Err :> es) => Eff es (AST A.Expr)
parseBinaryComparison =
  parseBinary
    parseBinaryAdditive
    [ (Glyph "<", A.BinaryOperator A.LessThan),
      (Glyph "<=", A.BinaryOperator A.LessThanOrEqual),
      (Glyph ">", A.BinaryOperator A.GreaterThan),
      (Glyph ">=", A.BinaryOperator A.GreaterThanOrEqual)
    ]

parseBinaryEquality :: (HasCallStack, State Parser :> es, Errors Err :> es) => Eff es (AST A.Expr)
parseBinaryEquality =
  parseBinary
    parseBinaryComparison
    [ (Glyph "==", A.BinaryOperator A.Equal),
      (Glyph "!=", A.BinaryOperator A.NotEqual)
    ]

parseBinaryAnd :: (HasCallStack, State Parser :> es, Errors Err :> es) => Eff es (AST A.Expr)
parseBinaryAnd =
  parseBinary
    parseBinaryEquality
    [ (Glyph "&&", A.BinaryOperator A.And)
    ]

parseBinaryOr :: (HasCallStack, State Parser :> es, Errors Err :> es) => Eff es (AST A.Expr)
parseBinaryOr =
  parseBinary
    parseBinaryAnd
    [ (Glyph "||", A.BinaryOperator A.Or)
    ]

parseIfExpr :: (HasCallStack, State Parser :> es, Errors Err :> es) => Eff es (AST A.Expr)
parseIfExpr = produceSpannedAST $ do
  (bodies, elseBody) <- parseIfPart
  returnWrap $ A.IfChain bodies elseBody
  where
    parseIfPart :: (State Parser :> es, Errors Err :> es) => Eff es (NonEmpty (AST A.Expr, AST A.Expr), Maybe (AST A.Expr))
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

parseLambda :: (HasCallStack, State Parser :> es, Errors Err :> es) => Eff es (AST A.Expr)
parseLambda = produceSpannedAST $ do
  expectKeyword "fn"
  expectGlyph "("
  params <- parseParams
  expectGlyph ")"
  returnType <- ifM (matchGlyph "->") (Just <$> parseTypeExpr) (return Nothing)
  cur <- gets current
  when (cur.kind /= Glyph "{") do expectGlyph ":"
  body <- parseExpr
  returnWrap A.Lambda {params, returnType, body}
  where
    parseParams :: (HasCallStack, State Parser :> es, Errors Err :> es) => Eff es [(AST A.TypeExpr, AST Text)]
    parseParams =
      parseSeparatedSequence
        ( SeparatorConfig
            { trailing = True,
              separator = Glyph ",",
              consume = do
                cur <- gets current
                if cur.kind == Glyph ")"
                  then return Nothing
                  else do
                    name <- readSymbol
                    expectGlyph ":"
                    ty <- parseTypeExpr
                    return $ Just (ty, name)
            }
        )

parseExpr :: (HasCallStack, State Parser :> es, Errors Err :> es) => Eff es (AST A.Expr)
parseExpr = do
  cur <- gets current
  case cur.kind of
    Keyword "builtin" -> produceSpannedAST $ do
      advance
      (AST name _) <- readSymbol
      returnWrap $ A.Builtin name
    Glyph "{" -> do
      body <- parseBody
      return $ A.ExprBody <$> body
    _ -> parseBinaryOr

parseLet :: (HasCallStack, State Parser :> es, Errors Err :> es) => Eff es (AST A.Stmt)
parseLet = produceSpannedAST $ do
  expectKeyword "let"
  name <- readSymbol
  colon <- matchGlyph ":"
  type_ <-
    if colon
      then Just <$> parseTypeExpr
      else return Nothing
  expectGlyph "="
  value <- parseExpr
  returnWrap $ A.Let {name, type_, value}

parseTopLevelLet :: (HasCallStack, State Parser :> es, Errors Err :> es) => Eff es (AST A.TopLevelStmt)
parseTopLevelLet = produceSpannedAST do
  expectKeyword "let"
  name <- readSymbol
  colon <- matchGlyph ":"
  ty <-
    if colon
      then Just <$> parseTypeExpr
      else return Nothing
  expectGlyph "="
  value <- parseExpr
  returnWrap $ A.TLet {name, ty, value}

attempt :: (HasCallStack, State Parser :> es, Errors Err :> es) => Eff es (Maybe a) -> Eff es (Maybe a)
attempt f = do
  parser <- get
  (result, parser') <- run
  case result of
    Left errs -> do put parser'; throwErrsWithCallStacks errs
    Right Nothing -> do put parser; return Nothing
    Right j -> do put parser'; return j
  where
    run = do
      result <- tryErr f
      parser' <- get
      return (result, parser')

tryParseLValue :: (HasCallStack, State Parser :> es, Errors Err :> es) => Eff es (Maybe (AST A.LValue))
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

parseBody :: (HasCallStack, State Parser :> es, Errors Err :> es) => Eff es (AST A.Body)
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

parseStmt :: (HasCallStack, State Parser :> es, Errors Err :> es) => Eff es (AST A.Stmt)
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
          returnWrap $ A.ExprStmt {value = expr, semicolon}

parseTopLevelStmt :: (HasCallStack, State Parser :> es, Errors Err :> es) => Eff es (AST A.TopLevelStmt)
parseTopLevelStmt = do
  cur <- gets current
  case cur.kind of
    Keyword "let" -> do
      let_ <- parseTopLevelLet
      expectGlyph ";"
      return let_
    _ -> throwSpan cur.span (ExpectedKeyword "let")

parseTopLevel :: (HasCallStack, State Parser :> es, Errors Err :> es) => Eff es (AST A.TopLevel)
parseTopLevel = produceSpannedAST do
  topLevel <- A.TopLevel <$> parseTopLevel' []
  returnWrap topLevel
  where
    parseTopLevel' acc = do
      current <- gets current
      if current.kind == Eof
        then return acc
        else do
          stmt <- parseTopLevelStmt
          parseTopLevel' (acc ++ [stmt])

runParser ::
  (HasCallStack, Errors Err :> es)
  => Text
  -> Eff (State Parser : es) a
  -> Eff es a
runParser source f = do
  let lexer = makeLexer source
  let parser = makeParser lexer
  parser <- case parser of
    Left e -> throwErrs e
    Right p -> return p
  evalState parser f
