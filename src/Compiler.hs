{-# LANGUAGE TupleSections #-}

module Compiler where

import AST (SyntaxTree (..))
import Common
import Control.Monad (join)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.List (findIndex)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import IR
import LexicalScopes hiding (lookupVariable)
import qualified LexicalScopes
import Logging (Log, log_, runLog, standardLogger)
import Typechecker (runTypecheck, runTypecheckCallStack, unifies)
import TypedAST
import Util

data PhiReference = PhiReference {name :: Name, forVariable :: VarId, inBlock :: BlockId}
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
    variables :: HashMap (VarId, BlockId) Name,
    variableTypes :: HashMap VarId TypeExpr,
    variableSpans :: HashMap VarId Span,
    incompletePhis :: HashMap BlockId [PhiReference],
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
      variables = HashMap.empty,
      variableTypes = HashMap.empty,
      variableSpans = HashMap.empty,
      incompletePhis = HashMap.empty,
      sealed = [BlockId 0],
      currentBreakBlocks = []
    }

isSealed :: (State Compiler :> es) => BlockId -> Eff es Bool
isSealed blockId = do
  sealed <- gets sealed
  return $ blockId `elem` sealed

getBlock :: (HasCallStack, State Compiler :> es, Error Err :> es) => BlockId -> Span -> Eff es Block
getBlock id span = do
  (Program blocks) <- gets program
  lookup id blocks `orThrowSpan` (span, InternalCompilerError)

modifyBlock :: (HasCallStack, State Compiler :> es, Error Err :> es) => BlockId -> Span -> (Block -> Eff es Block) -> Eff es ()
modifyBlock id span f = do
  (Program blocks) <- gets program
  block <- getBlock id span
  block' <- f block
  let program' = Program $ insertAssoc id block' blocks
  modify (\c -> c {program = program'})

addPredecessor :: (HasCallStack, State Compiler :> es, Error Err :> es) => BlockId -> BlockId -> Span -> Eff es ()
addPredecessor from to span = do
  seal <- isSealed to
  when seal $ throwSpan span InternalCompilerError
  modifyBlock to span $ \block@Block {predecessors} -> do
    return block {predecessors = predecessors ++ [from]}

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

  modifyBlock currentBlock span $ \block -> do
    return block {control}

switchToBlock :: (State Compiler :> es) => BlockId -> Eff es ()
switchToBlock id = do
  compiler <- get
  put compiler {currentBlock = id}

emitWithName :: (HasCallStack, State Compiler :> es, Error Err :> es) => Name -> TypeExpr -> RHS -> Span -> Eff es Name
emitWithName name ty rhs span = do
  let ssa = SSA ty name rhs span
  currentBlock <- gets currentBlock
  modifyBlock currentBlock span $ \block@Block {instructions} -> do
    return block {instructions = instructions ++ [ssa]}
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

writeVariable :: (HasCallStack, State Compiler :> es, Error Err :> es) => VarId -> BlockId -> Name -> Span -> Eff es ()
writeVariable varId blockId name span = do
  vars <- gets variables
  when (((/= name) <$> HashMap.lookup (varId, blockId) vars) `orElse` False) $ throwSpan span InternalCompilerError
  modify (\c -> c {variables = HashMap.insert (varId, blockId) name vars})

addEmptyPhi :: (HasCallStack, State Compiler :> es, Error Err :> es) => BlockId -> VarId -> Span -> Eff es PhiReference
addEmptyPhi blockId varId span = do
  compiler <- get
  ty <- HashMap.lookup varId compiler.variableTypes `orThrowSpan` (span, InternalCompilerError)
  name <- mkName
  let phi = Phi ty name [] span
  modifyBlock blockId span $ \block -> do
    return block {phis = block.phis ++ [phi]}
  let phiRef = PhiReference name varId blockId
  return phiRef

addCompletePhi :: (HasCallStack, State Compiler :> es, Error Err :> es) => BlockId -> TypeExpr -> [(BlockId, Name)] -> Span -> Eff es Name
addCompletePhi blockId ty operands span = do
  name <- mkName
  let phi = Phi ty name operands span
  modifyBlock blockId span $ \block -> do
    return block {phis = block.phis ++ [phi]}
  return name

setPhiOperands :: (HasCallStack, State Compiler :> es, Error Err :> es) => PhiReference -> [(BlockId, Name)] -> Span -> Eff es ()
setPhiOperands phiRef operands span = do
  block <- getBlock phiRef.inBlock span
  Phi ty name _ phiSpan <- case [phi | phi@(Phi _ n _ _) <- block.phis, n == phiRef.name] of
    [phi] -> return phi
    _ -> throwSpan span InternalCompilerError
  let phi' = Phi ty name operands phiSpan
      phis' = [if n == name then phi' else phi | phi@(Phi _ n _ _) <- block.phis]
  modifyBlock phiRef.inBlock span $ \block ->
    return block {phis = phis'}

setIncompletePhi :: (HasCallStack, State Compiler :> es) => BlockId -> PhiReference -> Eff es ()
setIncompletePhi blockId phiRef = do
  incompletePhis <- gets incompletePhis
  let phisForBlock = HashMap.lookup blockId incompletePhis `orElse` []
      phisForBlock' = phisForBlock ++ [phiRef]
      incompletePhis' = HashMap.insert blockId phisForBlock' incompletePhis
  modify (\c -> c {incompletePhis = incompletePhis'})

spanForVariable :: (HasCallStack, State Compiler :> es, Error Err :> es) => VarId -> Span -> Eff es Span
spanForVariable varId span = do
  variableSpans <- gets variableSpans
  HashMap.lookup varId variableSpans `orThrowSpan` (span, InternalCompilerError)

readVariable :: (HasCallStack, State Compiler :> es, Error Err :> es) => VarId -> BlockId -> Span -> Eff es Name
readVariable varId blockId span = do
  variables <- gets variables
  case HashMap.lookup (varId, blockId) variables of
    -- local value numbering
    -- (this variable is already bound to an SSA value in this block)
    Just name -> return name
    -- global value numbering
    Nothing -> readVariableRecursive varId blockId span

readVariableRecursive :: (HasCallStack, State Compiler :> es, Error Err :> es) => VarId -> BlockId -> Span -> Eff es Name
readVariableRecursive varId blockId span = do
  blockIsSealed <- isSealed blockId
  Block {predecessors} <- getBlock blockId span
  varSpan <- spanForVariable varId span
  name <-
    if
      | not blockIsSealed -> do
          -- The current block doesn't have all its predecessors determined
          -- yet. So we add an empty phi and will come back to it later when
          -- the block is sealed.
          phiRef <- addEmptyPhi blockId varId varSpan
          setIncompletePhi blockId phiRef
          return phiRef.name
      | length predecessors == 1 -> do
          -- If we know we only have one predecessor, we can just recurse
          -- into that predecessor.
          readVariable varId (head predecessors) span
      | otherwise -> do
          -- Multiple predecessors means we need a phi instruction.

          -- We emit an empty phi instruction so that if addPhiOperands
          -- loops back around to this block, the recursion will terminate.
          phiRef <- addEmptyPhi blockId varId varSpan
          writeVariable varId blockId phiRef.name span

          -- Then we fill out the phi's operands by recursing through
          -- each predecessor (basically the same as in the last branch,
          -- except multiple times).
          recursivelySetPhiOperands phiRef span
          return phiRef.name
  writeVariable varId blockId name span
  return name

recursivelySetPhiOperands :: (HasCallStack, State Compiler :> es, Error Err :> es) => PhiReference -> Span -> Eff es ()
recursivelySetPhiOperands phiRef span = do
  Block {predecessors} <- getBlock phiRef.inBlock span
  operands <- forM predecessors $ \pred -> do
    name <- readVariable phiRef.forVariable pred span
    return (pred, name)
  setPhiOperands phiRef operands span

markSealed :: (HasCallStack, State Compiler :> es, Error Err :> es) => BlockId -> Span -> Eff es ()
markSealed blockId span = do
  -- go back and fill in any incomplete phi instructions
  incompletePhis <- gets incompletePhis
  forM_ (HashMap.lookup blockId incompletePhis `orElse` []) $ \phiRef -> do
    recursivelySetPhiOperands phiRef span

  -- then mark this block as sealed
  already <- isSealed blockId
  when already $ throwSpan span InternalCompilerError
  modify (\c -> c {sealed = c.sealed ++ [blockId]})

markCurrentBlockSealed :: (HasCallStack, State Compiler :> es, Error Err :> es) => Span -> Eff es ()
markCurrentBlockSealed span = do
  currentBlock <- gets currentBlock
  markSealed currentBlock span

-- | Compile the expression into the current program. Returns name of the local SSA form that contains
-- | the final value of the expression.
compileExpr :: (HasCallStack, State Compiler :> es, Error Err :> es) => TST Expr -> Eff es Name
compileExpr (TST UndefinedLit _) = undefined
compileExpr (TST VoidLit span) = emit mkVoid RVoid span
compileExpr (TST (BoolLit b) span) = emit mkBool (RBool b) span
compileExpr (TST (IntLit n) span) = emit mkInt (RInt n) span
compileExpr (TST (Variable _ name) span) = do
  (varId, _) <- lookupVariable span name
  currentBlock <- gets currentBlock
  readVariable varId currentBlock span
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
  switchToBlock firstConditionId

  -- then we go through all the bodies one-by-one
  results <- forM (bodies `NE.zip` NE.fromList (NE.tail conditionIds ++ [elseBlockId])) $
    \((condition, body), nextConditionId) -> do
      -- compile the condition
      resultName <- compileExpr condition
      -- we know the condition won't have any other predecessors
      markCurrentBlockSealed (spanOf condition)

      -- allocate a block for the body of this branch
      bodyId <- allocateBlock
      -- if the condition is true, jump to the body block
      -- otherwise, jump to the next block in the chain
      setControl (JumpIf resultName bodyId nextConditionId) (spanOf condition)
      switchToBlock bodyId
      -- then compile the body
      bodyResultName <- compileExpr body
      -- we know the body won't have any other predecessors
      markCurrentBlockSealed (spanOf body)
      -- when the body is done, we should jump past the entire chain
      setControl (Jump postChainId) (spanOf body)
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
  let operands = NE.toList $ results `NE.append` NE.singleton (elseBlockId, elseResultName)
  currentBlock <- gets currentBlock
  addCompletePhi currentBlock ty operands span

compileStmt :: (HasCallStack, State Compiler :> es, Error Err :> es) => TST Stmt -> Eff es (Maybe Name)
compileStmt (TST (Let (TST name span) (TST ty _) value) _) = do
  valName <- compileExpr value
  varId <- mkVarId
  compiler <- get
  let blockId = compiler.currentBlock
  let scopes' = bindNewVariable name (varId, blockId) compiler.scopes
  let variables' = HashMap.insert (varId, blockId) valName compiler.variables
  let variableTypes' = HashMap.insert varId ty compiler.variableTypes
  let variableSpans' = HashMap.insert varId span compiler.variableSpans
  put
    compiler
      { scopes = scopes',
        variables = variables',
        variableTypes = variableTypes',
        variableSpans = variableSpans'
      }
  return Nothing
compileStmt (TST (Assign (TST (LVariable _ name) span) value) _) = do
  valName <- compileExpr value
  (varId, _) <- lookupVariable span name
  compiler <- get
  let variables' = HashMap.insert (varId, compiler.currentBlock) valName compiler.variables
  put compiler {variables = variables'}
  return Nothing
compileStmt (TST (ExprStmt expr semicolon) _) = do
  name <- compileExpr expr
  if semicolon then return Nothing else return (Just name)
compileStmt (TST (Return _) _) = undefined
compileStmt (TST (Loop (TST body bodySpan)) span) = do
  -- allocate a new basic block to hold the body of the loop
  loopBlock <- allocateBlock
  -- also pre-allocate a block for the block that will follow the loop
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
  markCurrentBlockSealed span
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

compileProgram :: (HasCallStack, State Compiler :> es, Error Err :> es) => TST Stmt -> Eff es ()
compileProgram stmt@(TST _ span) = do
  _ <- compileStmt stmt
  compiler <- get
  -- seal the final block
  markSealed compiler.currentBlock span

compileAndCheck :: (HasCallStack, State Compiler :> es, Error Err :> es, Log :> es) => TST Stmt -> Eff es ()
compileAndCheck stmt = do
  log_ "hello"
  compileProgram stmt
  (Program blocks) <- gets program
  forM_ blocks $ \(id, _) -> do
    seal <- isSealed id
    -- unless seal $ throwSpan (spanOf stmt) InternalCompilerError
    return undefined

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
