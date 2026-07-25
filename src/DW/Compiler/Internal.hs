{-# LANGUAGE TupleSections #-}

module DW.Compiler.Internal where

import DW.AST (SyntaxTree (..))
import DW.Common hiding (scribe)
import DW.Compiler.Internal.Compiler
import DW.Compiler.Internal.Lenses
import DW.Compiler.Internal.Types
import DW.IR
import DW.Lens
import DW.NameResolutionPass.Names
import DW.Typechecker (unifies)
import DW.TypedAST
import DW.Util

import Control.Monad (join)
import Data.HashMap.Strict qualified as HashMap

compileBody :: (HasCallStack, State Compiler :> es, Log :> es) => Body -> Span -> Eff es Term
compileBody (Body ty stmts) span = do
  results <- forM stmts compileStmt
  let result = join $ safeLast results
  term <- case result of
    Nothing | ty `unifies` mkVoid -> do
      emit mkVoid RVoid span
    Nothing -> throwICE
    Just term -> return term
  return term

compileLambda ::
  (HasCallStack, State Compiler :> es, Log :> es)
  => TypeExpr -> [(TST TypeExpr, TST VarName)] -> TST Expr -> Span -> Eff es FnId
compileLambda ty params body span = do
  withRegion "Saving compiler state to compile new function..." do
    (fnId, savedCompiler) <- saveCompilerAndResetForNewFn ty

    forM_ (params `zip` [0 ..]) $ \((TST ty _, name), i) -> do
      scribe $ format "Binding parameter {} ({})" (Shown name, Shown (getVarId <$> name))
      bindNewVariable (node name) $ AbstractVariable {name = node name, ty, span = spanOf name}
      blockId <- gets activeBlock

      term <- emit ty (RParameter i) span
      modify (\c -> c {variablesPerBlock = HashMap.insert (node name, blockId) term c.variablesPerBlock})
      scribe $ format "Parameter loaded into term {}" (Only (Shown term))

    scribe $ format "Compiling new function ({})" (Only (Shown fnId))
    resultTerm <- compileExpr body
    setControl (Ret resultTerm)

    scribe "Performing seal check..."
    (FnDef _ blocks) <- getCurrentFunction
    forM_ blocks $ \(blockId, _) -> do
      sealed <- isSealed blockId
      unless sealed throwICE

    scribe "Restoring compiler state"
    restoreSavedCompilerAfterFnCompile savedCompiler
    return fnId

-- | Compile the expression into the current program. Returns term of the local SSA form that contains
-- | the final value of the expression.
compileExpr :: (HasCallStack, State Compiler :> es, Log :> es) => TST Expr -> Eff es Term
compileExpr (TST VoidLit span) = emit mkVoid RVoid span
compileExpr (TST (BoolLit b) span) = emit mkBool (RBool b) span
compileExpr (TST (IntLit n) span) = emit mkInt (RInt n) span
compileExpr (TST (Variable _ name) span) = do
  maybeVar <- lookupVariable name
  case maybeVar of
    Just (AbstractVariable {name}) -> do
      scribe $ format "Looking up variable {} ({})" (Shown name, Shown (getVarId name))
      activeBlock <- gets activeBlock
      determineTermInBlock name activeBlock
    Nothing -> do
      (static, ty) <- lookupStaticVariable name <&> unwrapICE
      emit ty (RLoadStatic static) span
compileExpr (TST (BinaryOperator ty op l r) span) = do
  lTerm <- compileExpr l
  rTerm <- compileExpr r
  emit ty (RBinOp op lTerm rTerm) span
compileExpr (TST (UnaryOperator ty op v) span) = do
  term <- compileExpr v
  emit ty (RUnaryOp op term) span
compileExpr (TST (FunctionCall ty fn args) span) = do
  fnTerm <- compileExpr fn
  argTerms <- mapM compileExpr args
  emit ty (RCall fnTerm argTerms) span
compileExpr (TST (ExprBody body) span) = compileBody body span
compileExpr (TST (IfThen ty condition body elseBody) span) = withRegion "Compiling if-then..." do
  -- Allocate a sealed block for the condition
  conditionBlock <- allocateBlock
  scribe $ format "Allocating block {} for the condition" (Only (Shown conditionBlock))
  setControl (Jump conditionBlock)
  markSealed conditionBlock
  switchToBlock conditionBlock

  -- Compile the condition into that block
  resultTerm <- withRegion "Compiling the condition..." $ compileExpr condition

  -- Allocate a block for the body and the elseBody
  bodyBlock <- allocateBlock
  scribe $ format "Allocating block {} for the body" (Only (Shown bodyBlock))
  elseBodyBlock <- allocateBlock
  scribe $ format "Allocating block {} for the else-body" (Only (Shown elseBodyBlock))

  -- Now whatever the current block is should jump conditionally to those blocks
  setControl (JumpIf resultTerm bodyBlock elseBodyBlock)

  -- Now we can mark both of them as sealed (they only have one predecessor each)
  markSealed bodyBlock
  markSealed elseBodyBlock

  -- Allocate an unsealed block to follow the if (and grab its result with a phi instruction)
  postBlock <- allocateBlock
  scribe $ format "Allocating block {} for the if-result" (Only (Shown postBlock))

  -- Now we can actually compile into those blocks
  switchToBlock bodyBlock
  bodyResultTerm <- withRegion "Compiling the body..." $ compileExpr body
  finalBodyBlock <- gets activeBlock
  setControl (Jump postBlock)

  switchToBlock elseBodyBlock
  elseBodyResultTerm <- withRegion "Compiling the else-body..." $ compileExpr elseBody
  finalElseBodyBlock <- gets activeBlock
  setControl (Jump postBlock)

  -- Generate the phi instruction for the post block
  phiTerm <- addCompletePhi postBlock ty [(finalBodyBlock, bodyResultTerm), (finalElseBodyBlock, elseBodyResultTerm)] span
  scribe $ format "Result is collected from phi instruction into {}" (Only (Shown phiTerm))

  -- Now that the postBlock has all its predecessors determined, we can seal it
  markSealed postBlock

  -- And switch to it before returning so that the rest of the program compiles into it
  -- (We can do this because we sealed the block, which fulfills our responsibility
  -- to it as its creator).
  switchToBlock postBlock

  return phiTerm
compileExpr (TST (Builtin ty bName) span) = do
  bName' <- translateBuiltin bName
  emit ty (RBuiltin bName') span
  where
    translateBuiltin "print" = return "builtin_print"
    translateBuiltin _ = throwICE
compileExpr (TST (Boxed ty value) span) = do
  inner <- compileExpr (TST value span)
  emit mkAny (RBox ty inner) span
compileExpr (TST (Lambda {ty, params, body}) span) = do
  fnId <- compileLambda ty params body span

  emit ty (RLoadFn fnId) span

compileStmt :: (HasCallStack, State Compiler :> es, Log :> es) => TST Stmt -> Eff es (Maybe Term)
compileStmt (TST (Let (TST name span) (TST ty _) value) _) = do
  -- special case for undefined
  -- this should get torn out eventually
  valTerm <- compileExpr value
  blockId <- gets activeBlock
  scribe $ format "Binding new variable {} ({}) in block {}" (Shown name, getVarId name, Shown blockId)
  bindNewVariable name $ AbstractVariable {name, ty, span}
  modify (\c -> c {variablesPerBlock = HashMap.insert (name, blockId) valTerm c.variablesPerBlock})
  return Nothing
compileStmt (TST (Assign (TST (LVariable _ name) _) value) span) = do
  valTerm <- compileExpr value
  maybeVar <- lookupVariable name
  case maybeVar of
    Just (AbstractVariable {name}) -> do
      compiler <- get
      scribe $ format "Encountered variable assignment to {} ({}) in block {}" (Shown name, Shown (getVarId name), Shown compiler.activeBlock)
      let variablesPerBlock' = HashMap.insert (name, compiler.activeBlock) valTerm compiler.variablesPerBlock
      put compiler {variablesPerBlock = variablesPerBlock'}
      return Nothing
    Nothing -> do
      (static, _) <- lookupStaticVariable name <&> unwrapICE
      emitSetStatic static valTerm span
      return Nothing
compileStmt (TST (ExprStmt expr semicolon) _) = do
  term <- compileExpr expr
  if semicolon then return Nothing else return (Just term)
compileStmt (TST (Return _) _) = undefined
compileStmt (TST (Loop (TST body bodySpan)) _) = withRegion "Compiling loop statement..." do
  -- allocate a new basic block to hold the body of the loop
  loopBlock <- allocateBlock
  scribe $ format "Allocated block {} for body of the loop" (Only (Shown loopBlock))
  -- also pre-allocate a block for the block that will follow the loop
  -- (this is where we'll jump to when we hit a break statement)
  postLoopBlockId <- allocateBlock
  scribe $ format "Allocated block {} to follow the loop" (Only (Shown postLoopBlockId))
  modify (\c -> c {currentBreakBlocks = postLoopBlockId : c.currentBreakBlocks})
  -- the current block should conclude by jumping to the new loop block
  setControl (Jump loopBlock)
  -- switch our context so that we're emitting instructions into the loop block
  switchToBlock loopBlock
  -- and now we can actually compile the body of the loop itself
  _ <- withRegion "Compiling body of loop..." $ compileBody body bodySpan
  modify (\c -> c {currentBreakBlocks = tail c.currentBreakBlocks})
  -- the loop concludes by unconditionally jumping to its beginning
  setControl (Jump loopBlock)
  -- now that the body of the loop has been compiled, we know the start of the loop
  -- has no other predecessors
  markSealed loopBlock
  -- same with the post-loop block (all breaks have been compiled)

  markSealed postLoopBlockId
  switchToBlock postLoopBlockId
  return Nothing
compileStmt (TST Break _) = withRegion "Compiling break statement..." do
  currentBreakBlock <- gets (safeHead . currentBreakBlocks)
  case currentBreakBlock of
    Nothing -> throwICE
    Just currentBreakBlock -> do
      setControl (Jump currentBreakBlock)
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
      markSealed nextBlock
      scribe $ format "Allocated block {} to dump post-break instructions" (Only (Shown nextBlock))
      switchToBlock nextBlock
  return Nothing

compileTopLevelValue :: (HasCallStack, State Compiler :> es, Log :> es) => TST Expr -> Eff es StaticInitializer
compileTopLevelValue (TST VoidLit _) = return SIVoid
compileTopLevelValue (TST (IntLit n) _) = return (SIInt n)
compileTopLevelValue (TST (BoolLit b) _) = return (SIBool b)
compileTopLevelValue (TST (Lambda {ty, params, body}) span) = do
  fnId <- compileLambda ty params body span
  return (SIFn fnId)
compileTopLevelValue _ = throwICE -- ConstExprPass should prevent this from ever happening

compileTopLevelStmt :: (HasCallStack, State Compiler :> es, Log :> es) => TST TopLevelStmt -> StaticId -> Eff es ()
compileTopLevelStmt (TST (TLet {name = (TST name _), value}) _) staticId = do
  initializer <- compileTopLevelValue value
  initializeStaticVariable staticId initializer

  -- if this defines a function named main, then mark that as our entry point
  when (getVarText name == "main") do
    case initializer of
      SIFn fnId -> modify (programL % programEntryL ?~ fnId)
      _ -> return ()

compileTopLevel :: (HasCallStack, State Compiler :> es, Log :> es) => TST TopLevel -> Eff es ()
compileTopLevel (TST (TopLevel tStmts) _) = do
  ids <- forM tStmts $ \tStmt -> do
    case tStmt of
      (TST (TLet {name = (TST name _), ty = (TST ty _)}) _) -> do
        bindStaticVariable name ty
  forM_ (tStmts `zip` ids) $ \(tStmt, id) -> do
    compileTopLevelStmt tStmt id

compileProgram :: (HasCallStack, State Compiler :> es, Log :> es) => TST TopLevel -> Eff es ()
compileProgram topLevel = withRegion "Compiling program..." do
  compileTopLevel topLevel
  program <- gets program
  scribe $ format "Program:\n\n{}" (Only (Shown program))

runCompiler :: (HasCallStack, Log :> es) => TST TopLevel -> Eff es Program
runCompiler topLevel = do
  compiler <- execState mkCompiler $ compileProgram topLevel
  return compiler.program
