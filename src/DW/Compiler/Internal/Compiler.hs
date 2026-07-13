{-# LANGUAGE MultiParamTypeClasses #-}

module DW.Compiler.Internal.Compiler where

import DW.Common hiding (scribe)
import DW.Compiler.Internal.Program
import DW.Compiler.Internal.Users
import DW.IR
import DW.LexicalScopes
import DW.Logging qualified as Logging
import DW.TypedAST
import DW.Util

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable
import Data.List (nub)
import Data.Text.Lazy qualified

newtype VarId = VarId Int
  deriving (Show, Eq)

instance Hashable VarId where
  hash (VarId id) = hash id
  hashWithSalt salt (VarId id) = hashWithSalt salt id

data PhiReference = PhiReference {name :: Name, forVariable :: VarId, inBlock :: BlockId}
  deriving (Eq)

instance Show PhiReference where
  show (PhiReference {name, forVariable, inBlock}) = Data.Text.Lazy.unpack $ format "φ({} for {} in {})" (Shown name, Shown forVariable, Shown inBlock)

-- | Refers to a unique variable in the original source code
data AbstractVariable = AbstractVariable {varId :: VarId, ty :: TypeExpr, span :: Span}
  deriving (Show, Eq)

data Compiler = Compiler
  { nameCounter :: Int,
    blockCounter :: Int,
    varCounter :: Int,
    fnCounter :: Int,
    program :: Program,
    activeFn :: FnId,
    activeBlock :: BlockId,
    scopes :: LexicalScopes AbstractVariable,
    -- | Each entry in this map represents the SSA name associated with a particular AST variable
    -- | in a particular block. these get used to fill out phi functions.
    -- | Equivalent of `currentDef` in the Braun construction.
    variablesPerBlock :: HashMap (VarId, BlockId) Name,
    -- | When generating code, sometimes we reach a point where we can't be sure what `Name` refers
    -- | to a given variable. In those instances, we generate an empty phi instruction, and mark it
    -- | in this map so that we can come back to it later when that block is sealed.
    incompletePhis :: HashMap BlockId [PhiReference],
    -- | Each entry in this map represents an instance in the IR where a `Name` gets used, whether
    -- | that's as an operand to an instruction, as an operand to a phi instruction, or as the condition
    -- | in a `JumpIf` control.
    userMap :: UserMap,
    -- | This just keeps track of which blocks are sealed (meaning their predecessors are all known)
    sealed :: HashSet BlockId,
    -- | If we're inside a loop, this points to the basic block that follows the loop
    -- | (in other words, where we jump when we hit a break statement)
    currentBreakBlocks :: [BlockId]
  }
  deriving (Show, Eq)

instance HasUserMap Compiler where
  getUserMap = userMap
  setUserMap userMap c = c {userMap}

instance HasProgram Compiler where
  getProgram = program
  setProgram program c = c {program}

instance HasLexicalScopes AbstractVariable Compiler where
  getScopes = scopes
  setScopes scopes c = c {scopes}

mkMainFnType :: TypeExpr
mkMainFnType = TypeExpr {reference = False, valueExpr = Function [] (TST mkVoid (Span 0 1))}

mkCompiler :: Compiler
mkCompiler =
  Compiler
    { nameCounter = 0,
      blockCounter = 1,
      varCounter = 0,
      fnCounter = 1,
      program = Program $ HashMap.fromList [(FnId 0, FnDef mkMainFnType [(BlockId 0, mkBlock)])],
      activeFn = FnId 0,
      activeBlock = BlockId 0,
      scopes = mkScopes,
      variablesPerBlock = HashMap.empty,
      incompletePhis = HashMap.empty,
      userMap = mkUserMap,
      sealed = HashSet.fromList [BlockId 0],
      currentBreakBlocks = []
    }

--
-- Counter manipulation
--

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

mkFnId :: (State Compiler :> es) => Eff es FnId
mkFnId = do
  compiler <- get
  let fnId = FnId compiler.fnCounter
      compiler' = compiler {fnCounter = compiler.fnCounter + 1}
  put compiler'
  return fnId

--
-- Scope handling
--

lookupVariableById ::
  (HasCallStack, State s :> es, HasLexicalScopes AbstractVariable s)
  => VarId -> Eff es AbstractVariable
lookupVariableById id = do
  scopes <- gets getScopes
  lookupByValue (\v -> v.varId == id) scopes <&> unwrapICE

spanForVariable :: (HasCallStack, State Compiler :> es) => VarId -> Eff es Span
spanForVariable varId = do
  var <- lookupVariableById varId
  return var.span

--
-- Function manipulation
--

getCurrentFunction :: (HasCallStack, State Compiler :> es) => Eff es FnDef
getCurrentFunction = do
  compiler <- get
  return $ HashMap.lookup compiler.activeFn (fnMap compiler.program) & unwrapICE

modifyCurrentFunction :: (HasCallStack, State Compiler :> es) => (FnDef -> Eff es FnDef) -> Eff es ()
modifyCurrentFunction f = do
  compiler <- get
  currentFn <- getCurrentFunction
  currentFn' <- f currentFn
  modify (\c -> c {program = Program (HashMap.insert compiler.activeFn currentFn' (fnMap compiler.program))})

saveCompilerAndResetForNewFn :: (HasCallStack, State Compiler :> es) => TypeExpr -> Eff es (FnId, Compiler)
saveCompilerAndResetForNewFn ty = do
  -- Make the new function and insert it into the program state
  fnId <- mkFnId
  let fnDef = FnDef ty [(BlockId 0, mkBlock)]
  modify
    ( \c@Compiler {program = Program fns} ->
        c {program = Program $ HashMap.insert fnId fnDef fns}
    )

  -- Save the compiler state at this point
  savedCompiler <- get

  -- Reset function-specific state to baseline
  modify
    ( \c ->
        c
          { nameCounter = 0,
            blockCounter = 1,
            activeFn = fnId,
            activeBlock = BlockId 0,
            scopes = mkScopes,
            variablesPerBlock = HashMap.empty,
            incompletePhis = HashMap.empty,
            userMap = HashMap.empty,
            sealed = HashSet.fromList [BlockId 0],
            currentBreakBlocks = []
          }
    )

  return (fnId, savedCompiler)

restoreSavedCompilerAfterFnCompile :: (HasCallStack, State Compiler :> es) => Compiler -> Eff es ()
restoreSavedCompilerAfterFnCompile saved = do
  -- Restore function-specific state from saved compiler
  modify
    ( \c ->
        c
          { nameCounter = saved.nameCounter,
            blockCounter = saved.blockCounter,
            activeFn = saved.activeFn,
            activeBlock = saved.activeBlock,
            scopes = saved.scopes,
            variablesPerBlock = saved.variablesPerBlock,
            incompletePhis = saved.incompletePhis,
            userMap = saved.userMap,
            sealed = saved.sealed,
            currentBreakBlocks = saved.currentBreakBlocks
          }
    )

--
-- Block manipulation
--

allocateBlock :: (State Compiler :> es) => Eff es BlockId
allocateBlock = do
  id <- mkBlockId
  modifyCurrentFunction $ \(FnDef ty blocks) -> do
    return $ FnDef ty (blocks ++ [(id, mkBlock)])
  return id

addPredecessor :: (HasCallStack, State Compiler :> es) => BlockId -> BlockId -> Eff es ()
addPredecessor from to = do
  seal <- isSealed to
  when seal throwICE
  modifyBlock to $ \block@Block {predecessors} -> do
    return block {predecessors = predecessors ++ [from]}

getBlock ::
  (HasCallStack, State Compiler :> es)
  => BlockId -> Eff es Block
getBlock id = do
  FnDef _ blocks <- getCurrentFunction
  return $ lookup id blocks & unwrapICE

modifyBlock ::
  (HasCallStack, State Compiler :> es)
  => BlockId -> (Block -> Eff es Block) -> Eff es ()
modifyBlock id f = do
  block <- getBlock id
  modifyCurrentFunction $ \(FnDef ty blocks) -> do
    block' <- f block
    return $ FnDef ty $ insertAssoc id block' blocks

--
-- Modifying the active block
--

setControl ::
  (HasCallStack, State Compiler :> es, Log :> es)
  => Control -> Eff es ()
setControl control = do
  activeBlock <- gets activeBlock

  -- when we make a new jump, that creates a new edge in the CFG
  -- so wherever we're jumping to, we should add ourself to its predecessors
  case control of
    Halt -> return ()
    Ret _ -> return ()
    Jump target -> do
      scribe $ format "Jump {} -> {}" (Shown activeBlock, Shown target)
      addPredecessor activeBlock target
    JumpIf name target1 target2 -> do
      scribe $ format "JumpIf {} -> ({}, {})" (Shown activeBlock, Shown target1, Shown target2)
      addUser name (ControlUser activeBlock)
      addPredecessor activeBlock target1
      addPredecessor activeBlock target2

  modifyBlock activeBlock $ \block -> do
    return block {control}

switchToBlock :: (HasCallStack, State Compiler :> es, Log :> es) => BlockId -> Eff es ()
switchToBlock id = do
  scribe $ format "Switching context to block {}" (Only (Shown id))
  compiler <- get
  put compiler {activeBlock = id}

emit :: (HasCallStack, State Compiler :> es, Log :> es) => TypeExpr -> RHS -> Span -> Eff es Name
emit ty rhs span = do
  name <- mkName
  let ssa = SSA {ty, name, rhs, span}
  activeBlock <- gets activeBlock
  modifyBlock activeBlock $ \block@Block {instructions} -> do
    return block {instructions = instructions ++ [ssa]}

  -- update the users map (if we reference any names on our RHS)
  let usesNames = nub $ case rhs of
        RBinOp _ left right -> [left, right]
        RUnaryOp _ name -> [name]
        RCall fn args -> fn : args
        _ -> []
  unless (null usesNames) $ do
    forM_ usesNames $ \usesName -> addUser usesName (SSAUser name activeBlock)

  return name

--
-- Sealing
--

isSealed :: (State Compiler :> es) => BlockId -> Eff es Bool
isSealed blockId = do
  sealed <- gets sealed
  return $ blockId `elem` sealed

markSealed :: (HasCallStack, State Compiler :> es, Log :> es) => BlockId -> Eff es ()
markSealed blockId = withRegion (format "Marking {} as sealed" (Only (Shown blockId))) do
  -- go back and fill in any incomplete phi instructions
  incompletePhis <- gets incompletePhis
  forM_ (HashMap.lookup blockId incompletePhis `orElse` []) $ \phiRef -> do
    scribe $ format "Filling out incomplete phi {}" (Only (Shown phiRef))
    recursivelySetPhiOperands phiRef

  -- then mark this block as sealed
  already <- isSealed blockId
  when already throwICE
  modify (\c -> c {sealed = HashSet.insert blockId c.sealed})

--
-- Variable determination
--

-- | Provide the `Name` that will contain the value of the given variable in the given block.
-- | Equivalent to `currentDef` in the Braun construction.
setVariableNameInBlock :: (HasCallStack, State Compiler :> es) => VarId -> BlockId -> Name -> Eff es ()
setVariableNameInBlock varId blockId name = do
  vars <- gets variablesPerBlock
  -- We allow overwrites, in case a phi reduction happens
  -- when (((/= name) <$> HashMap.lookup (varId, blockId) vars) `orElse` False) $ throwSpan span InternalCompilerError
  modify (\c -> c {variablesPerBlock = HashMap.insert (varId, blockId) name vars})

-- | For the given variable and block, determine the `Name` that will contain the
-- | value of that variable.
-- | This will generate phi instructions as necessary.
-- | Equivalent to `readVariable` in the Braun construction.
determineNameInBlock :: (HasCallStack, State Compiler :> es, Log :> es) => VarId -> BlockId -> Eff es Name
determineNameInBlock varId blockId = do
  variables <- gets variablesPerBlock
  case HashMap.lookup (varId, blockId) variables of
    -- local value numbering
    -- (this variable is already bound to an SSA value in this block)
    Just name -> return name
    -- global value numbering
    Nothing -> determineNameInBlockRec varId blockId

determineNameInBlockRec :: (HasCallStack, State Compiler :> es, Log :> es) => VarId -> BlockId -> Eff es Name
determineNameInBlockRec varId blockId = do
  blockIsSealed <- isSealed blockId
  Block {predecessors} <- getBlock blockId
  varSpan <- spanForVariable varId
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
          determineNameInBlock varId (head predecessors)
      | otherwise -> do
          -- Multiple predecessors means we need a phi instruction.

          -- We emit an empty phi instruction so that if addPhiOperands
          -- loops back around to this block, the recursion will terminate.
          phiRef <- addEmptyPhi blockId varId varSpan
          scribe $ format "Generating phi {}" (Only (Shown phiRef))
          setVariableNameInBlock varId blockId phiRef.name

          -- Then we fill out the phi's operands by recursing through
          -- each predecessor (basically the same as in the last branch,
          -- except multiple times).
          name <- recursivelySetPhiOperands phiRef

          return name
  setVariableNameInBlock varId blockId name
  return name

--
-- Phi handling
--

addEmptyPhi ::
  (HasCallStack, State Compiler :> es)
  => BlockId
  -> VarId
  -> Span
  -> Eff es PhiReference
addEmptyPhi blockId varId span = do
  AbstractVariable {ty} <- lookupVariableById varId
  name <- mkName
  let phi = Phi {ty, name, operands = [], span}
  modifyBlock blockId $ \block -> do
    return block {phis = block.phis ++ [phi]}
  let phiRef = PhiReference name varId blockId
  return phiRef

addCompletePhi ::
  (HasCallStack, State Compiler :> es)
  => BlockId
  -> TypeExpr
  -> [(BlockId, Name)]
  -> Span
  -> Eff es Name
addCompletePhi blockId ty operands span = do
  name <- mkName
  let phi = Phi {ty, name, operands, span}
  modifyBlock blockId $ \block -> do
    return block {phis = block.phis ++ [phi]}
  return name

getPhi :: (HasCallStack, State Compiler :> es) => PhiReference -> Eff es (Maybe Phi)
getPhi phiRef = do
  -- find the block that this phi reference points to
  block <- getBlock phiRef.inBlock
  -- find the phi in the block that the phi reference points to
  case [phi | phi@Phi {name} <- block.phis, name == phiRef.name] of
    [phi] -> return $ Just phi
    _ -> return Nothing

setPhiOperands :: (HasCallStack, State Compiler :> es) => PhiReference -> [(BlockId, Name)] -> Eff es ()
setPhiOperands phiRef operands = do
  -- find the block that this phi reference points to
  block <- getBlock phiRef.inBlock
  -- find the phi in the block that the phi reference points to
  phi <- getPhi phiRef <&> unwrapICE
  -- update the phi to contains the new operands
  let phi' = phi {operands}
      phis' = [if p.name == phi.name then phi' else p | p <- block.phis]
  -- modify the block
  modifyBlock phiRef.inBlock $ \block ->
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

recursivelySetPhiOperands ::
  (HasCallStack, State Compiler :> es, Log :> es)
  => PhiReference -> Eff es Name
recursivelySetPhiOperands phiRef = do
  Block {predecessors} <- getBlock phiRef.inBlock
  operands <- forM predecessors $ \pred -> do
    name <- determineNameInBlock phiRef.forVariable pred
    return (pred, name)
  setPhiOperands phiRef operands
  tryRemoveTrivialPhi phiRef

data PhiReduction = PRUnreachable | PRNoReduce | PRReduceTo Name
  deriving (Show)

replaceNameInRHS :: Name -> Name -> RHS -> RHS
replaceNameInRHS from to (RBinOp op left right) = RBinOp op left' right'
  where
    left' = if from == left then to else left
    right' = if from == right then to else right
replaceNameInRHS from to (RUnaryOp op name) = RUnaryOp op (if from == name then to else name)
replaceNameInRHS from to (RCall fn args) = RCall fn' args'
  where
    fn' = if from == fn then to else fn
    args' = map (\a -> if from == a then to else a) args
replaceNameInRHS _ _ rhs = rhs

tryRemoveTrivialPhi ::
  (HasCallStack, State Compiler :> es, Log :> es)
  => PhiReference -> Eff es Name
tryRemoveTrivialPhi phiRef = do
  phi <- getPhi phiRef <&> unwrapICE
  let operands' = nub $ filter (/= phi.name) $ snd <$> phi.operands
      reduceTo = case operands' of
        [] -> PRUnreachable
        [name'] -> PRReduceTo name'
        _ -> PRNoReduce

  scribe $ format "phiRef {} reduction status: {}" (Shown phiRef, Shown reduceTo)

  case reduceTo of
    PRUnreachable -> return phiRef.name
    PRNoReduce -> return phiRef.name
    PRReduceTo reduceTo -> do
      -- find all the places that use the value produced by this phi
      users <- getUsers phi.name
      let users' = filter (\case PhiUser n _ -> n /= phi.name; _ -> True) users
      scribe $ format "found {} users" (Only (length users'))
      -- for each place that uses this phi's value, rewrite them to use the original value instead
      forM_ users' $ \user -> do
        scribe $ format "rewriting user: {}" (Only (Shown user))
        case user of
          PhiUser userName blockId -> replacePhiUser blockId userName phi.name reduceTo
          SSAUser userName blockId -> replaceSSAUser blockId userName phi.name reduceTo
          ControlUser blockId -> replaceControlUser blockId phi.name reduceTo
        -- update the user map too
        addUser reduceTo user

      -- this phi has become completely useless (it literally has no users)
      -- so we delete it from the block
      modifyBlock phiRef.inBlock $ \block -> do
        return block {phis = filter (\p -> p.name /= phi.name) block.phis}
      -- and any place where it's marked as a user, we remove
      forM_ phi.operands $ \(_, operandName) ->
        removeUser operandName (PhiUser phi.name phiRef.inBlock)
      -- and we remove its own entries from userMap as well
      removeAllUsers phi.name

      -- then, for each place that used this phi's value, if that place was _itself_ a phi, it could potentially
      -- have become trivial itself. so, we recurse onto it
      forM_ users' $ \user -> do
        case user of
          PhiUser userName blockId -> do
            let recursePhiRef = PhiReference userName undefined blockId
            -- a previous recursion might have removed this phi reference, so make sure it still exists
            stillExists <- isJust <$> getPhi recursePhiRef
            when stillExists $ void $ tryRemoveTrivialPhi recursePhiRef
          _ -> return ()

      return reduceTo
  where
    replacePhiUser :: (State Compiler :> es) => BlockId -> Name -> Name -> Name -> Eff es ()
    replacePhiUser blockId userName replaceName withName = do
      block <- getBlock blockId
      let userPhi = block.phis `getSingleElement` (\p -> p.name == userName) & unwrapICE
      let userPhi' = userPhi {operands = map (\(id, n) -> (id, if n == replaceName then withName else n)) userPhi.operands}
      let phis' = modifyElementBy block.phis (\p -> p.name == userName) (const userPhi') & unwrapICE
      modifyBlock blockId $ \block -> do
        return block {phis = phis'}
    replaceSSAUser :: (State Compiler :> es) => BlockId -> Name -> Name -> Name -> Eff es ()
    replaceSSAUser blockId userName replaceName withName = do
      block <- getBlock blockId
      let userSSA = block.instructions `getSingleElement` (\s -> s.name == userName) & unwrapICE
      let userSSA' = userSSA {rhs = replaceNameInRHS replaceName withName userSSA.rhs}
      let instructions' = modifyElementBy block.instructions (\s -> s.name == userName) (const userSSA') & unwrapICE
      modifyBlock blockId $ \block -> do
        return block {instructions = instructions'}
    replaceControlUser :: (State Compiler :> es) => BlockId -> Name -> Name -> Eff es ()
    replaceControlUser blockId replaceName withName = do
      block <- getBlock blockId
      control' <- case block.control of
        Halt -> throwICE
        Ret name -> return $ Ret $ if name == replaceName then withName else name
        Jump _ -> throwICE
        JumpIf name t1 t2 -> return $ JumpIf (if name == replaceName then withName else name) t1 t2
      modifyBlock blockId $ \block -> do
        return block {control = control'}

--
-- Scribe override
--

scribe :: (HasCallStack, State Compiler :> es, Log :> es) => Data.Text.Lazy.Text -> Eff es ()
scribe msg = do
  activeBlock <- gets activeBlock
  Logging.scribe $ format "[{}] {}" (Shown activeBlock, msg)
