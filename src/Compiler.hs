{-# LANGUAGE TupleSections #-}

module Compiler where

import AST (SyntaxTree (..))
import Common
import Control.Monad (join)
import Data.List (findIndex)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import IR
import LexicalScopes hiding (lookupVariable)
import qualified LexicalScopes
import Typechecker (runTypecheck, runTypecheckCallStack, unifies)
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
    -- `currentDef` in the Braun construction
    -- each entry in this map represents the SSA name associated with a particular AST variable
    -- in a particular block. these get used to fill out phi functions
    variables :: [((VarId, BlockId), Name)],
    sealed :: [BlockId],
    -- | If we're inside a loop, this points to the basic block that follows the loop
    -- | (in other words, where we jump when we hit a break statement)
    currentBreakBlocks :: [BlockId]
  }
  deriving (Show, Eq)

lookupVariable :: (HasCallStack, State Compiler :> es, Error Err :> es) => Span -> Text -> Eff es (VarId, BlockId)
lookupVariable span name = zoomState scopes (\s t -> t {scopes = s}) (LexicalScopes.lookupVariable span name)

insertAssoc :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)]
insertAssoc key value list = case idx of
  Nothing -> list ++ [(key, value)]
  Just idx -> take idx list ++ [(key, value)] ++ drop (idx + 1) list
  where
    idx = findIndex ((== key) . fst) list

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
      sealed = [BlockId 0],
      currentBreakBlocks = []
    }

isSealed :: (State Compiler :> es) => BlockId -> Eff es Bool
isSealed blockId = do
  sealed <- gets sealed
  return $ blockId `elem` sealed

markSealed :: (HasCallStack, State Compiler :> es, Error Err :> es) => BlockId -> Span -> Eff es ()
markSealed blockId span = do
  already <- isSealed blockId
  when already $ throwSpan span InternalCompilerError
  modify (\c -> c {sealed = c.sealed ++ [blockId]})

getBlock :: (HasCallStack, State Compiler :> es, Error Err :> es) => BlockId -> Span -> Eff es Block
getBlock id span = do
  (Program blocks) <- gets program
  lookup id blocks `orThrowSpan` (span, InternalCompilerError)

addPredecessor :: (HasCallStack, State Compiler :> es, Error Err :> es) => BlockId -> BlockId -> Span -> Eff es ()
addPredecessor from to span = do
  seal <- isSealed to
  when seal $ throwSpan span InternalCompilerError
  (Program blocks) <- gets program
  (Block phis insts ctl preds) <- getBlock to span
  let block' = Block phis insts ctl (preds ++ [from])
  let program' = Program $ insertAssoc to block' blocks
  modify (\c -> c {program = program'})

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

setControl :: (HasCallStack, State Compiler :> es, Error Err :> es) => Control -> Span -> Eff es ()
setControl control span = do
  currentBlock <- gets currentBlock

  -- when we make a new jump, that creates a new edge in the CFG
  -- so wherever we're jumping to, we should add ourself to its predecessors
  case control of
    Jump target -> do
      addPredecessor currentBlock target span
    JumpIf _ target1 target2 -> do
      addPredecessor currentBlock target1 span
      addPredecessor currentBlock target2 span
    Halt -> return ()

  (Program blocks) <- gets program
  let block = lookup currentBlock blocks
  (Block phis insts _ preds) <- case block of
    Nothing -> throwSpan span InternalCompilerError
    Just block -> return block
  let block' = Block phis insts control preds
  let program' = Program (insertAssoc currentBlock block' blocks)
  modify \compiler -> compiler {program = program'}

switchToBlock :: (State Compiler :> es) => BlockId -> Eff es ()
switchToBlock id = do
  compiler <- get
  put compiler {currentBlock = id}

emitWithName :: (HasCallStack, State Compiler :> es, Error Err :> es) => Name -> TypeExpr -> RHS -> Span -> Eff es Name
emitWithName name ty rhs span = do
  let ssa = SSA ty name rhs
  (Program blocks) <- gets program
  currentBlock <- gets currentBlock
  let block = lookup currentBlock blocks
  (Block phis insts ctl preds) <- case block of
    Nothing -> throwSpan span InternalCompilerError
    Just block -> return block
  let insts' = insts ++ [ssa]
  let block' = Block phis insts' ctl preds
  let program' = Program (insertAssoc currentBlock block' blocks)
  modify \compiler -> compiler {program = program'}
  return name

emit :: (HasCallStack, State Compiler :> es, Error Err :> es) => TypeExpr -> RHS -> Span -> Eff es Name
emit ty rhs span = do
  name <- mkName
  emitWithName name ty rhs span

compileBody :: (HasCallStack, State Compiler :> es, Error Err :> es) => Body -> Span -> Eff es Name
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

addPhiToBlock :: (HasCallStack, State Compiler :> es, Error Err :> es) => BlockId -> TypeExpr -> NonEmpty (BlockId, Name) -> Span -> Eff es Name
addPhiToBlock blockId ty pairs span = do
  resultName <- mkName
  (Block phis insts ctl preds) <- getBlock blockId span
  let phis' = phis ++ [Phi ty resultName pairs]
  let block' = Block phis' insts ctl preds
  (Program blocks) <- gets program
  let blocks' = insertAssoc blockId block' blocks
  let program' = Program blocks'
  modify (\c -> c {program = program'})
  return resultName

addPhi :: (HasCallStack, State Compiler :> es, Error Err :> es) => TypeExpr -> NonEmpty (BlockId, Name) -> Span -> Eff es Name
addPhi ty pairs span = do
  blockId <- gets currentBlock
  addPhiToBlock blockId ty pairs span

writeVariable :: (HasCallStack, State Compiler :> es, Error Err :> es) => VarId -> BlockId -> Name -> Span -> Eff es ()
writeVariable varId blockId name span = do
  vars <- gets variables
  when (isJust $ lookup (varId, blockId) vars) $ throwSpan span InternalCompilerError
  let vars' = vars ++ [((varId, blockId), name)]
  modify (\c -> c {variables = vars'})

readVariable :: (HasCallStack, State Compiler :> es, Error Err :> es) => VarId -> BlockId -> TypeExpr -> Span -> Eff es Name
readVariable varId blockId ty span = do
  vars <- gets variables
  let localName = lookup (varId, blockId) vars
  case localName of
    -- This variable is already mapped in this block, so we can
    -- do local value numbering
    Just localName -> return localName
    -- Otherwise, we need to do global value numbering
    Nothing -> readVariableRecursive varId blockId
  where
    readVariableRecursive varId blockId = do
      sealed <- isSealed blockId
      (Block phis insts ctl preds) <- getBlock blockId span
      name <-
        if
          | sealed -> do
              return undefined
          | length preds == 1 -> do
              -- only one predecessor, so we don't need to introduce a phi instruction
              readVariable varId (head preds) ty span
          | otherwise -> do
              -- _ <- addPhiToBlock blockId ty (NE.singleton (blockId, ))
              return undefined
      -- now that we've mapped this variable in this block, we don't need to do it again
      -- so mark it down for later
      writeVariable varId blockId name span
      return name
    getPhiOperands varId name = do
      return undefined

-- | Compile the expression into the current program. Returns name of the local SSA form that contains
-- | the final value of the expression.
compileExpr :: (HasCallStack, State Compiler :> es, Error Err :> es) => TST Expr -> Eff es Name
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
    else undefined
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
  -- pre-allocate blocks for each condition
  conditionIds <- mapM (const allocateBlock) bodies
  -- pre-allocate an id for the else body
  -- if there is no else body, we just generate an empty block and rely on later passes to clean that up
  elseBlockId <- allocateBlock
  -- pre-allocate an id for what comes after the entire if chain
  postChainId <- allocateBlock

  -- this block concludes by jumping to the first if condition
  let firstConditionId = NE.head conditionIds
  setControl (Jump firstConditionId) span
  markSealed firstConditionId span
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
      markSealed bodyId (spanOf body)
      switchToBlock bodyId
      -- then compile the body
      bodyResultName <- compileExpr body

      -- when the body is done, we should jump past the entire chain
      setControl (Jump postChainId) span
      markSealed nextConditionId span
      switchToBlock nextConditionId

      -- we save the name of the body's result for later (to use in the phi instruction)
      return (bodyId, bodyResultName)
  -- generate the else body (even if it's empty)
  elseResultName <- case elseBody of
    Nothing -> emit mkVoid RVoid span
    Just elseBody -> compileExpr elseBody
  setControl (Jump postChainId) span
  switchToBlock postChainId
  -- now, to get the result out we have to use the phi function
  let phiPairs = results `NE.append` NE.singleton (elseBlockId, elseResultName)
  addPhi ty phiPairs span

compileStmt :: (HasCallStack, State Compiler :> es, Error Err :> es) => TST Stmt -> Eff es (Maybe Name)
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
  postLoopBlockId <- allocateBlock
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

compileAndCheck :: (HasCallStack, State Compiler :> es, Error Err :> es) => TST Stmt -> Eff es (Maybe Name)
compileAndCheck stmt = do
  r <- compileStmt stmt
  (Program blocks) <- gets program
  forM_ blocks $ \(id, _) -> do
    seal <- isSealed id
    -- unless seal $ throwSpan (spanOf stmt) InternalCompilerError
    return undefined
  return r

runCompiler :: Text -> Result Program
runCompiler source = do
  tst <- runTypecheck source
  compiler <- runPureEff $ runErrorNoCallStack $ execState mkCompiler $ compileAndCheck tst
  return compiler.program

runCompilerCallStack :: Text -> Either (CallStack, Err) Program
runCompilerCallStack source = do
  tst <- runTypecheckCallStack source
  compiler <- runPureEff $ runError $ execState mkCompiler $ compileAndCheck tst
  return compiler.program

execCompilerCallStack :: Text -> Either (CallStack, Err) Compiler
execCompilerCallStack source = do
  tst <- runTypecheckCallStack source
  runPureEff $ runError $ execState mkCompiler $ compileAndCheck tst
