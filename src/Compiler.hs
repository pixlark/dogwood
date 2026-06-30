{-# LANGUAGE TupleSections #-}

module Compiler where

import Common hiding (scribe)
import Control.Monad (join)
import Data.Function (on)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.List (findIndex, nub, nubBy)
import qualified Data.Text.Lazy
import IR
import LexicalScopes hiding (lookupVariable)
import qualified LexicalScopes
import qualified Logging
import LowerPass (runLowerPass)
import Parser (parseStmt, runParse, runParseCallStack)
import Typechecker (runTypecheck, runTypecheckCallStack, unifies)
import TypedAST
import Util

scribe :: (HasCallStack, State Compiler :> es, Log :> es) => Data.Text.Lazy.Text -> Eff es ()
scribe msg = do
  currentBlock <- gets currentBlock
  Logging.scribe $ format "[{}] {}" (Shown currentBlock, msg)

data PhiReference = PhiReference {name :: Name, forVariable :: VarId, inBlock :: BlockId}
  deriving (Eq)

instance Show PhiReference where
  show (PhiReference {name, forVariable, inBlock}) = Data.Text.Lazy.unpack $ format "φ({} for {} in {})" (Shown name, Shown forVariable, Shown inBlock)

data UserReference
  = PhiUser Name BlockId
  | SSAUser Name BlockId
  | ControlUser BlockId
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
    userMap :: HashMap Name [UserReference],
    sealed :: [BlockId],
    -- | If we're inside a loop, this points to the basic block that follows the loop
    -- | (in other words, where we jump when we hit a break statement)
    currentBreakBlocks :: [BlockId]
  }
  deriving (Show, Eq)

addUser :: (HasCallStack, State Compiler :> es) => Name -> UserReference -> Eff es ()
addUser usesName userRef = do
  modify (\c -> c {userMap = addUser' c.userMap})
  where
    addUser' map =
      let existing = HashMap.lookup usesName map `orElse` []
       in HashMap.insert usesName (existing ++ [userRef]) map

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
      userMap = HashMap.empty,
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

setControl :: (HasCallStack, State Compiler :> es, Error Err :> es, Log :> es) => Control -> Span -> Eff es ()
setControl control span = do
  currentBlock <- gets currentBlock

  -- when we make a new jump, that creates a new edge in the CFG
  -- so wherever we're jumping to, we should add ourself to its predecessors
  case control of
    Jump target -> do
      scribe $ format "Jump {} -> {}" (Shown currentBlock, Shown target)
      addPredecessor currentBlock target span
    JumpIf name target1 target2 -> do
      scribe $ format "JumpIf {} -> ({}, {})" (Shown currentBlock, Shown target1, Shown target2)
      addUser name (ControlUser currentBlock)
      addPredecessor currentBlock target1 span
      addPredecessor currentBlock target2 span
    Halt -> return ()

  modifyBlock currentBlock span $ \block -> do
    return block {control}

switchToBlock :: (HasCallStack, State Compiler :> es, Log :> es) => BlockId -> Eff es ()
switchToBlock id = do
  scribe $ format "Switching context to block {}" (Only (Shown id))
  compiler <- get
  put compiler {currentBlock = id}

emitWithName :: (HasCallStack, State Compiler :> es, Error Err :> es) => Name -> TypeExpr -> RHS -> Span -> Eff es Name
emitWithName name ty rhs span = do
  let ssa = SSA ty name rhs span
  currentBlock <- gets currentBlock
  modifyBlock currentBlock span $ \block@Block {instructions} -> do
    return block {instructions = instructions ++ [ssa]}

  -- update the users map (if we reference any names on our RHS)
  let usesNames = nub $ case rhs of
        RBinOp _ left right -> [left, right]
        RUnaryOp _ name -> [name]
        RCall fn args -> fn : args
        _ -> []
  unless (null usesNames) $ do
    forM_ usesNames $ \usesName -> addUser usesName (SSAUser name currentBlock)

  return name

emit :: (HasCallStack, State Compiler :> es, Error Err :> es) => TypeExpr -> RHS -> Span -> Eff es Name
emit ty rhs span = do
  name <- mkName
  emitWithName name ty rhs span

compileBody :: (HasCallStack, State Compiler :> es, Error Err :> es, Log :> es) => Body -> Span -> Eff es Name
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
  let phi = Phi {ty, name, operands = [], span}
  modifyBlock blockId span $ \block -> do
    return block {phis = block.phis ++ [phi]}
  let phiRef = PhiReference name varId blockId
  return phiRef

addCompletePhi :: (HasCallStack, State Compiler :> es, Error Err :> es) => BlockId -> TypeExpr -> [(BlockId, Name)] -> Span -> Eff es Name
addCompletePhi blockId ty operands span = do
  name <- mkName
  let phi = Phi {ty, name, operands, span}
  modifyBlock blockId span $ \block -> do
    return block {phis = block.phis ++ [phi]}
  return name

getPhi :: (HasCallStack, State Compiler :> es, Error Err :> es) => PhiReference -> Span -> Eff es Phi
getPhi phiRef span = do
  -- find the block that this phi reference points to
  block <- getBlock phiRef.inBlock span
  -- find the phi in the block that the phi reference points to
  case [phi | phi@Phi {name} <- block.phis, name == phiRef.name] of
    [phi] -> return phi
    _ -> throwSpan span InternalCompilerError

setPhiOperands :: (HasCallStack, State Compiler :> es, Error Err :> es) => PhiReference -> [(BlockId, Name)] -> Span -> Eff es ()
setPhiOperands phiRef operands span = do
  -- find the block that this phi reference points to
  block <- getBlock phiRef.inBlock span
  -- find the phi in the block that the phi reference points to
  phi <- getPhi phiRef span
  -- update the phi to contains the new operands
  let phi' = phi {operands}
      phis' = [if p.name == phi.name then phi' else p | p <- block.phis]
  -- modify the block
  modifyBlock phiRef.inBlock span $ \block ->
    return block {phis = phis'}
  -- update the users map
  forM_ (snd <$> operands) $ \usesName -> addUser usesName (PhiUser phi.name phiRef.inBlock)

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

readVariable :: (HasCallStack, State Compiler :> es, Error Err :> es, Log :> es) => VarId -> BlockId -> Span -> Eff es Name
readVariable varId blockId span = do
  variables <- gets variables
  case HashMap.lookup (varId, blockId) variables of
    -- local value numbering
    -- (this variable is already bound to an SSA value in this block)
    Just name -> return name
    -- global value numbering
    Nothing -> readVariableRecursive varId blockId span

readVariableRecursive :: (HasCallStack, State Compiler :> es, Error Err :> es, Log :> es) => VarId -> BlockId -> Span -> Eff es Name
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
          scribe $ format "Generating incomplete phi {}" (Only (Shown phiRef))
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
          scribe $ format "Generating phi {}" (Only (Shown phiRef))
          writeVariable varId blockId phiRef.name span

          -- Then we fill out the phi's operands by recursing through
          -- each predecessor (basically the same as in the last branch,
          -- except multiple times).
          recursivelySetPhiOperands phiRef span
          return phiRef.name
  writeVariable varId blockId name span
  return name

recursivelySetPhiOperands :: (HasCallStack, State Compiler :> es, Error Err :> es, Log :> es) => PhiReference -> Span -> Eff es ()
recursivelySetPhiOperands phiRef span = do
  Block {predecessors} <- getBlock phiRef.inBlock span
  operands <- forM predecessors $ \pred -> do
    name <- readVariable phiRef.forVariable pred span
    return (pred, name)
  setPhiOperands phiRef operands span
  tryRemoveTrivialPhi phiRef span

data PhiReduction = PRUnreachable | PRNoReduce | PRReduceTo Name
  deriving (Show)

tryRemoveTrivialPhi :: (HasCallStack, State Compiler :> es, Error Err :> es, Log :> es) => PhiReference -> Span -> Eff es ()
tryRemoveTrivialPhi phiRef span = do
  phi <- getPhi phiRef span
  let operands' = nub $ filter (/= phi.name) $ snd <$> phi.operands
      reduceTo = case operands' of
        [] -> PRUnreachable
        [name'] -> PRReduceTo name'
        _ -> PRNoReduce

  scribe $ format "phiRef {} reduction status: {}" (Shown phiRef, Shown reduceTo)

  case reduceTo of
    PRUnreachable -> return ()
    PRNoReduce -> return ()
    PRReduceTo reduceTo -> do
      users <- (`orElse` []) . HashMap.lookup phi.name <$> gets userMap
      let users' = filter (\case PhiUser n _ -> n /= phi.name; _ -> False) users
      forM_ users' $ \user -> do
        case user of
          PhiUser userName blockId -> replacePhiUser blockId userName phi.name reduceTo
          SSAUser userName blockId -> replaceSSAUser blockId userName phi.name reduceTo
          ControlUser blockId -> replaceControlUser blockId phi.name reduceTo
  where
    replacePhiUser blockId userName replaceName withName = do
      block <- getBlock blockId span
      userPhi <- block.phis `getSingleElement` (\p -> p.name == userName) `orThrowSpan` (span, InternalCompilerError)
      let userPhi' = userPhi {operands = map (\(id, n) -> (id, if n == replaceName then withName else n)) userPhi.operands}
      phis' <- modifyElementBy block.phis (\p -> p.name == userName) (const userPhi') `orThrowSpan` (span, InternalCompilerError)
      modifyBlock blockId span $ \block -> do
        return block {phis = phis'}
    replaceSSAUser blockId userName replaceName withName = do
      return ()
    replaceControlUser blockId replaceName withName = do
      return ()

markSealed :: (HasCallStack, State Compiler :> es, Error Err :> es, Log :> es) => BlockId -> Span -> Eff es ()
markSealed blockId span = withRegion (format "Marking {} as sealed" (Only (Shown blockId))) do
  -- go back and fill in any incomplete phi instructions
  incompletePhis <- gets incompletePhis
  forM_ (HashMap.lookup blockId incompletePhis `orElse` []) $ \phiRef -> do
    scribe $ format "Filling out incomplete phi {}" (Only (Shown phiRef))
    recursivelySetPhiOperands phiRef span

  -- then mark this block as sealed
  already <- isSealed blockId
  when already $ throwSpan span InternalCompilerError
  modify (\c -> c {sealed = c.sealed ++ [blockId]})

markCurrentBlockSealed :: (HasCallStack, State Compiler :> es, Error Err :> es, Log :> es) => Span -> Eff es ()
markCurrentBlockSealed span = do
  currentBlock <- gets currentBlock
  markSealed currentBlock span

-- | Compile the expression into the current program. Returns name of the local SSA form that contains
-- | the final value of the expression.
compileExpr :: (HasCallStack, State Compiler :> es, Error Err :> es, Log :> es) => TST Expr -> Eff es Name
compileExpr (TST UndefinedLit _) = undefined
compileExpr (TST VoidLit span) = emit mkVoid RVoid span
compileExpr (TST (BoolLit b) span) = emit mkBool (RBool b) span
compileExpr (TST (IntLit n) span) = emit mkInt (RInt n) span
compileExpr (TST (Variable _ name) span) = do
  (varId, _) <- lookupVariable span name
  scribe $ format "Looking up variable {} ({})" (name, Shown varId)
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
compileExpr (TST (IfThen ty condition body elseBody) span) = withRegion "Compiling if-then..." do
  -- Allocate a sealed block for the condition
  conditionBlock <- allocateBlock
  scribe $ format "Allocating block {} for the condition" (Only (Shown conditionBlock))
  setControl (Jump conditionBlock) span
  markSealed conditionBlock span

  -- Compile the condition into that block
  resultName <- withRegion "Compiling the condition..." $ compileExpr condition

  -- Allocate a block for the body and the elseBody
  bodyBlock <- allocateBlock
  scribe $ format "Allocating block {} for the body" (Only (Shown bodyBlock))
  elseBodyBlock <- allocateBlock
  scribe $ format "Allocating block {} for the else-body" (Only (Shown elseBodyBlock))

  -- Now whatever the current block is should jump conditionally to those blocks
  setControl (JumpIf resultName bodyBlock elseBodyBlock) span

  -- Now we can mark both of them as sealed (they only have one predecessor each)
  markSealed bodyBlock span
  markSealed elseBodyBlock span

  -- Allocate an unsealed block to follow the if (and grab its result with a phi instruction)
  postBlock <- allocateBlock
  scribe $ format "Allocating block {} for the if-result" (Only (Shown postBlock))

  -- Now we can actually compile into those blocks
  switchToBlock bodyBlock
  bodyResultName <- withRegion "Compiling the body..." $ compileExpr body
  finalBodyBlock <- gets currentBlock
  setControl (Jump postBlock) span

  switchToBlock elseBodyBlock
  elseBodyResultName <- withRegion "Compiling the else-body..." $ compileExpr elseBody
  finalElseBodyBlock <- gets currentBlock
  setControl (Jump postBlock) span

  -- Generate the phi instruction for the post block
  phiName <- addCompletePhi postBlock ty [(finalBodyBlock, bodyResultName), (finalElseBodyBlock, elseBodyResultName)] span
  scribe $ format "Result is collected from phi instruction into {}" (Only (Shown phiName))

  -- Now that the postBlock has all its predecessors determined, we can seal it
  markSealed postBlock span

  -- And switch to it before returning so that the rest of the program compiles into it
  -- (We can do this because we sealed the block, which fulfills our responsibility
  -- to it as its creator).
  switchToBlock postBlock

  return phiName

compileStmt :: (HasCallStack, State Compiler :> es, Error Err :> es, Log :> es) => TST Stmt -> Eff es (Maybe Name)
compileStmt (TST (Let (TST name span) (TST ty _) value) _) = do
  valName <- compileExpr value
  varId <- mkVarId
  compiler <- get
  let blockId = compiler.currentBlock
  scribe $ format "Binding new variable {} ({}) in block {}" (name, Shown varId, Shown blockId)
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
  scribe $ format "Encountered variable assignment to {} ({}) in block {}" (name, Shown varId, Shown compiler.currentBlock)
  let variables' = HashMap.insert (varId, compiler.currentBlock) valName compiler.variables
  put compiler {variables = variables'}
  return Nothing
compileStmt (TST (ExprStmt expr semicolon) _) = do
  name <- compileExpr expr
  if semicolon then return Nothing else return (Just name)
compileStmt (TST (Return _) _) = undefined
compileStmt (TST (Loop (TST body bodySpan)) span) = withRegion "Compiling loop statement..." do
  -- allocate a new basic block to hold the body of the loop
  loopBlock <- allocateBlock
  scribe $ format "Allocated block {} for body of the loop" (Only (Shown loopBlock))
  -- also pre-allocate a block for the block that will follow the loop
  -- (this is where we'll jump to when we hit a break statement)
  postLoopBlockId <- allocateBlock
  scribe $ format "Allocated block {} to follow the loop" (Only (Shown postLoopBlockId))
  modify (\c -> c {currentBreakBlocks = postLoopBlockId : c.currentBreakBlocks})
  -- the current block should conclude by jumping to the new loop block
  setControl (Jump loopBlock) span
  -- switch our context so that we're emitting instructions into the loop block
  switchToBlock loopBlock
  -- and now we can actually compile the body of the loop itself
  _ <- withRegion "Compiling body of loop..." $ compileBody body bodySpan
  modify (\c -> c {currentBreakBlocks = tail c.currentBreakBlocks})
  -- the loop concludes by unconditionally jumping to its beginning
  setControl (Jump loopBlock) span
  -- now that the body of the loop has been compiled, we know the start of the loop
  -- has no other predecessors
  markSealed loopBlock span
  -- same with the post-loop block (all breaks have been compiled)

  markSealed postLoopBlockId span
  switchToBlock postLoopBlockId
  return Nothing
compileStmt (TST Break span) = withRegion "Compiling break statement..." do
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
      markSealed nextBlock span
      scribe $ format "Allocated block {} to dump post-break instructions" (Only (Shown nextBlock))
      switchToBlock nextBlock
  return Nothing

compileProgram :: (HasCallStack, State Compiler :> es, Error Err :> es, Log :> es) => TST Stmt -> Eff es ()
compileProgram stmt@(TST _ _) = withRegion "Compiling program..." do
  _ <- compileStmt stmt
  return ()

compileAndCheck :: (HasCallStack, State Compiler :> es, Error Err :> es, Log :> es) => TST Stmt -> Eff es ()
compileAndCheck stmt = do
  compileProgram stmt
  (Program blocks) <- gets program
  forM_ blocks $ \(id, _) -> do
    seal <- isSealed id
    -- unless seal $ throwSpan (spanOf stmt) InternalCompilerError
    return undefined

runCompiler :: (Log :> es) => Text -> Eff es (Result Program)
runCompiler source = case runParse source parseStmt of
  Left err -> return (Left err)
  Right ast -> case runTypecheck (runLowerPass ast) of
    Left err -> return (Left err)
    Right tst -> do
      result <- runErrorNoCallStack $ execState mkCompiler $ compileAndCheck tst
      return $ fmap (.program) result

runCompilerCallStack :: (Log :> es) => Text -> Eff es (Either (CallStack, Err) Program)
runCompilerCallStack source = case runParseCallStack source parseStmt of
  Left err -> return (Left err)
  Right ast -> case runTypecheckCallStack (runLowerPass ast) of
    Left err -> return (Left err)
    Right tst -> do
      result <- runError $ execState mkCompiler $ compileAndCheck tst
      return $ fmap (.program) result

execCompilerCallStack :: (Log :> es) => Text -> Eff es (Either (CallStack, Err) Compiler)
execCompilerCallStack source = case runParseCallStack source parseStmt of
  Left err -> return (Left err)
  Right ast -> case runTypecheckCallStack (runLowerPass ast) of
    Left err -> return (Left err)
    Right tst -> runError $ execState mkCompiler $ compileAndCheck tst
