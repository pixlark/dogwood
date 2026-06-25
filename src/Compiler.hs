{-# LANGUAGE TupleSections #-}

module Compiler where

import AST (SyntaxTree (..))
import Common
import Control.Monad (join)
import qualified Data.List.NonEmpty as NE
import IR
import LexicalScopes hiding (lookupVariable)
import qualified LexicalScopes
import Typechecker (runTypecheck, unifies)
import TypedAST
import Util

newtype VarId = VarId Int
  deriving (Show, Eq)

data Compiler = Compiler
  { nameCounter :: Int,
    blockCounter :: Int,
    varCounter :: Int,
    program :: Program,
    currentBlock :: BlockId,
    scopes :: LexicalScopes (VarId, BlockId),
    variables :: [((VarId, BlockId), Name)],
    -- | If we're inside a loop, this points to the basic block that follows the loop
    -- | (in other words, where we jump when we hit a break statement)
    currentBreakBlocks :: [BlockId]
  }
  deriving (Show, Eq)

lookupVariable :: (State Compiler :> es, Error Err :> es) => Span -> Text -> Eff es (VarId, BlockId)
lookupVariable span name = zoomState scopes (\s t -> t {scopes = s}) (LexicalScopes.lookupVariable span name)

insertAssoc :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)]
insertAssoc key value list = filter ((/= key) . fst) list ++ [(key, value)]

mkCompiler :: Compiler
mkCompiler =
  Compiler
    { nameCounter = 0,
      blockCounter = 1,
      varCounter = 0,
      program = Program [(BlockId 0, mkBlock)],
      currentBlock = BlockId 0,
      scopes = mkScopes,
      variables = [],
      currentBreakBlocks = []
    }

mkName :: (State Compiler :> es) => Eff es Name
mkName = do
  compiler <- get
  let name = Name compiler.nameCounter
      compiler' = compiler {nameCounter = compiler.nameCounter + 1}
  put compiler'
  return name

mkBlockId :: (State Compiler :> es) => Eff es BlockId
mkBlockId = do
  compiler <- get
  let blockId = BlockId compiler.blockCounter
      compiler' = compiler {blockCounter = compiler.blockCounter + 1}
  put compiler'
  return blockId

mkVarId :: (State Compiler :> es) => Eff es VarId
mkVarId = do
  compiler <- get
  let varId = VarId compiler.varCounter
      compiler' = compiler {varCounter = compiler.varCounter + 1}
  put compiler'
  return varId

allocateBlock :: (State Compiler :> es) => Eff es BlockId
allocateBlock = do
  id <- mkBlockId
  compiler@Compiler {program = Program blocks} <- get
  let compiler' = compiler {program = Program (blocks ++ [(id, mkBlock)])}
  put compiler'
  return id

allocateBlockWithId :: (State Compiler :> es) => BlockId -> Eff es ()
allocateBlockWithId id = do
  compiler@Compiler {program = Program blocks} <- get
  let compiler' = compiler {program = Program (blocks ++ [(id, mkBlock)])}
  put compiler'

setControl :: (State Compiler :> es, Error Err :> es) => Control -> Span -> Eff es ()
setControl control span = do
  (Program blocks) <- gets program
  currentBlock <- gets currentBlock
  let block = lookup currentBlock blocks
  (Block insts _) <- case block of
    Nothing -> throwSpan span InternalCompilerError
    Just block -> return block
  let block' = Block insts control
  let program' = Program (insertAssoc currentBlock block' blocks)
  modify \compiler -> compiler {program = program'}

switchToBlock :: (State Compiler :> es) => BlockId -> Eff es ()
switchToBlock id = do
  compiler <- get
  put compiler {currentBlock = id}

emitWithName :: (State Compiler :> es, Error Err :> es) => Name -> TypeExpr -> RHS -> Span -> Eff es Name
emitWithName name ty rhs span = do
  let ssa = SSA ty name rhs
  (Program blocks) <- gets program
  currentBlock <- gets currentBlock
  let block = lookup currentBlock blocks
  (Block insts ctl) <- case block of
    Nothing -> throwSpan span InternalCompilerError
    Just block -> return block
  let insts' = insts ++ [ssa]
  let block' = Block insts' ctl
  let program' = Program (insertAssoc currentBlock block' blocks)
  modify \compiler -> compiler {program = program'}
  return name

emit :: (State Compiler :> es, Error Err :> es) => TypeExpr -> RHS -> Span -> Eff es Name
emit ty rhs span = do
  name <- mkName
  emitWithName name ty rhs span

compileBody :: (State Compiler :> es, Error Err :> es) => Body -> Span -> Eff es Name
compileBody (Body ty stmts) span = do
  -- each body opens a new lexical scope
  modify (\c -> c {scopes = pushScope c.scopes})

  results <- forM stmts compileStmt
  let result = join $ safeLast results
  name <- case result of
    Nothing | ty `unifies` mkVoid -> do
      emit mkVoid RVoid span
    Nothing -> throwSpan span InternalCompilerError
    Just name -> return name

  -- close the lexical scope
  do
    c <- get
    scopes' <- popScope c.scopes `orThrowSpan` (span, InternalCompilerError)
    put (c {scopes = scopes'})

  return name

-- | Compile the expression into the current program. Returns name of the local SSA form that contains
-- | the final value of the expression.
compileExpr :: (State Compiler :> es, Error Err :> es) => TST Expr -> Eff es Name
compileExpr (TST UndefinedLit _) = undefined
compileExpr (TST VoidLit span) = emit mkVoid RVoid span
compileExpr (TST (BoolLit b) span) = emit mkBool (RBool b) span
compileExpr (TST (IntLit n) span) = emit mkInt (RInt n) span
compileExpr (TST (Variable ty name) span) = do
  (varId, originalBlock) <- lookupVariable span name
  compiler <- get
  if originalBlock == compiler.currentBlock
    -- defined locally so we're good
    then lookup (varId, originalBlock) compiler.variables `orThrowSpan` (span, InternalCompilerError)
    -- otherwise we need to use the phi function
    else emit ty RPhiPlaceholder span
compileExpr (TST (BinaryOperator ty op l r) span) = do
  lName <- compileExpr l
  rName <- compileExpr r
  emit ty (RBinOp op lName rName) span
compileExpr (TST (UnaryOperator ty op v) span) = do
  name <- compileExpr v
  emit ty (RUnaryOp op name) span
compileExpr (TST (FunctionCall ty fn args) span) = do
  fnName <- compileExpr fn
  argNames <- mapM compileExpr args
  emit ty (RCall fnName argNames) span
compileExpr (TST (ExprBody body) span) = compileBody body span
compileExpr (TST (IfChain ty bodies elseBody) span) = do
  -- pre-allocate ids for each condition
  conditionIds <- mapM (const mkBlockId) bodies
  -- pre-allocate an id for the else body
  -- if there is no else body, we just generate an empty block and rely on later passes to clean that up
  elseBlockId <- mkBlockId
  -- pre-allocate an id for what comes after the entire if chain
  postChainId <- mkBlockId
  -- this block concludes by jumping to the first if condition
  let firstConditionId = NE.head conditionIds
  setControl (Jump firstConditionId) span
  allocateBlockWithId firstConditionId
  switchToBlock firstConditionId
  -- then we go through all the bodies one-by-one
  results <- forM (bodies `NE.zip` NE.fromList (NE.tail conditionIds ++ [elseBlockId])) $
    \((condition, body), nextConditionId) -> do
      -- compile the condition
      resultName <- compileExpr condition
      -- allocate a block for the body of this branch
      bodyId <- allocateBlock
      -- if the condition is true, jump to the body block
      -- otherwise, jump to the next block in the chain
      setControl (JumpIf resultName bodyId nextConditionId) (spanOf condition)
      switchToBlock bodyId
      -- then compile the body
      bodyResultName <- compileExpr body
      -- when the body is done, we should jump past the entire chain
      setControl (Jump postChainId) span
      allocateBlockWithId nextConditionId
      switchToBlock nextConditionId
      -- we save the name of the body's result for later (to use in the phi instruction)
      return (bodyId, bodyResultName)
  -- generate the else body (even if it's empty)
  elseResultName <- case elseBody of
    Nothing -> emit mkVoid RVoid span
    Just elseBody -> compileExpr elseBody
  setControl (Jump postChainId) span
  allocateBlockWithId postChainId
  switchToBlock postChainId
  -- now, to get the result out we have to use the phi function
  let phiPairs = results `NE.append` NE.singleton (elseBlockId, elseResultName)
  emit ty (RPhi phiPairs) span

compileStmt :: (State Compiler :> es, Error Err :> es) => TST Stmt -> Eff es (Maybe Name)
compileStmt (TST (Let (TST name _) _ value) _) = do
  valName <- compileExpr value
  varId <- mkVarId
  compiler <- get
  let blockId = compiler.currentBlock
  let scopes' = bindNewVariable name (varId, blockId) compiler.scopes
  let variables' = compiler.variables ++ [((varId, blockId), valName)]
  put compiler {scopes = scopes', variables = variables'}
  return Nothing
compileStmt (TST (Assign (TST (LVariable _ name) span) value) _) = do
  valName <- compileExpr value
  (varId, _) <- lookupVariable span name
  compiler <- get
  let variables' = compiler.variables ++ [((varId, compiler.currentBlock), valName)]
  put compiler {variables = variables'}
  return Nothing
compileStmt (TST (ExprStmt expr semicolon) _) = do
  name <- compileExpr expr
  if semicolon then return Nothing else return (Just name)
compileStmt (TST (Return _) _) = undefined
compileStmt (TST (Loop (TST body bodySpan)) span) = do
  -- allocate a new basic block to hold the body of the loop
  loopBlock <- allocateBlock
  -- also pre-allocate a block id for the block that will follow the loop
  -- (this is where we'll jump to when we hit a break statement)
  postLoopBlockId <- mkBlockId
  modify (\c -> c {currentBreakBlocks = postLoopBlockId : c.currentBreakBlocks})
  -- the current block should conclude by jumping to the new loop block
  setControl (Jump loopBlock) span
  -- switch our context so that we're emitting instructions into the loop block
  switchToBlock loopBlock
  -- and now we can actually compile the body of the loop itself
  _ <- compileBody body bodySpan
  modify (\c -> c {currentBreakBlocks = tail c.currentBreakBlocks})
  -- the loop concludes by unconditionally jumping to its beginning
  setControl (Jump loopBlock) span
  -- now we can allocate the post-loop block
  allocateBlockWithId postLoopBlockId
  -- and switch our context to it
  switchToBlock postLoopBlockId
  return Nothing
compileStmt (TST Break span) = do
  currentBreakBlock <- gets (safeHead . currentBreakBlocks)
  case currentBreakBlock of
    Nothing -> throwSpan span InternalCompilerError
    Just currentBreakBlock -> do
      setControl (Jump currentBreakBlock) span
      -- now we switch to a block that is unreachable
      -- this serves two purposes:
      --  1. if the programmer has unreachable statements after the break statement,
      --     this serves as a place to dump them.
      --  2. when we bubble back up to the loop compiler, it's going to assume that the current
      --     block isn't complete, and set its control flow instruction to return to the top of
      --     the loop. so if we don't switch to a new (unreachable) block, then it'll overwrite the
      --     control flow instruction we just wrote
      -- ideally once we introduce an optimizer pass, it will be able to trivially tell from the CFG
      -- that this block is never executed, and eliminate it before any code generation is actually done
      nextBlock <- allocateBlock
      switchToBlock nextBlock
  return Nothing

runCompiler :: Text -> Result Program
runCompiler source = do
  tst <- runTypecheck source
  compiler <- runPureEff $ runErrorNoCallStack $ execState mkCompiler $ compileStmt tst
  return compiler.program
