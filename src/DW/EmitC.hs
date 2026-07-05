{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module DW.EmitC where

import DW.Common
import DW.EmitC.Internal.EmitEffect
import DW.IR
import DW.TypedAST
import DW.Util (getSingleElement, orElse)

import Data.List (intersperse)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import NeatInterpolation

emitValueType :: forall es. (Emit :> es) => Eff es () -> ValueTypeExpr -> Eff es ()
emitValueType name Any = do emit "Box"; name
emitValueType name Undefined = do emit "uint8_t"; name -- for now
emitValueType name Void = do emit "uint8_t"; name -- for now
emitValueType name Bool = do emit "bool"; name
emitValueType name Int = do emit "int64_t"; name
emitValueType _ (NamespacedIdentifier _) = undefined
-- this is tremendously nasty, and that's just because C is evil and decided on the worst
-- possible syntax for function pointers. blame Dennis (RIP)
emitValueType name fn@(Function _ _) = do
  (left, right) <- functionPointerType fn
  left
  name
  right
  where
    emitArguments :: [TST TypeExpr] -> Eff es ()
    emitArguments args = do
      forM_ (intersperse Nothing (Just <$> args)) $ \case
        Just (TST arg _) -> emitType (return ()) arg
        Nothing -> emit ", "
    functionPointerType :: ValueTypeExpr -> Eff es (Eff es (), Eff es ())
    functionPointerType (Function args (TST (TypeExpr {valueExpr = innerFn@(Function _ _)}) _)) = do
      -- our return type is another function pointer
      (innerLeft, innerRight) <- functionPointerType innerFn
      let left = do innerLeft; emit "(*"
          right = do emit ")("; emitArguments args; emit ")"; innerRight
      return (left, right)
    functionPointerType (Function args (TST ret _)) = do
      -- our return type is a normal type
      let left = do emitType (return ()) ret; emit "(*"
          right = do emit ")("; emitArguments args; emit ")"
      return (left, right)
    functionPointerType _ = error "unreachable"

emitType :: (Emit :> es) => Eff es () -> TypeExpr -> Eff es ()
emitType name (TypeExpr {reference, valueExpr}) = do
  -- confusingly in C, the pointer syntax is attached to the *name*, not the type.
  -- this is why when you declare multiple pointer variables with a comma, you have
  -- to keep repeating the asterisk:
  --
  -- > int *a, *b;
  --
  -- similarly, that's why the asterisk(s) for a function pointer appear before the name:
  --
  -- > void(**foo)(int);
  let name' =
        if reference
          then do emit "*"; name
          else name
  emitValueType name' valueExpr

emitZeroValue :: (Emit :> es) => TypeExpr -> Eff es ()
emitZeroValue (TypeExpr {reference = True}) = emit "NULL"
emitZeroValue (TypeExpr {valueExpr}) = emitZeroValue' valueExpr
  where
    emitZeroValue' :: (Emit :> es) => ValueTypeExpr -> Eff es ()
    emitZeroValue' Any = emit "{0}"
    emitZeroValue' Undefined = emit "0"
    emitZeroValue' Void = emit "0"
    emitZeroValue' Bool = emit "false"
    emitZeroValue' Int = emit "0"
    emitZeroValue' (NamespacedIdentifier _) = undefined
    emitZeroValue' (Function _ _) = emit "NULL"

emitOperator :: Operator -> Text
emitOperator Or = "||"
emitOperator And = "&&"
emitOperator Equal = "=="
emitOperator NotEqual = "!="
emitOperator LessThan = "<"
emitOperator LessThanOrEqual = "<="
emitOperator GreaterThan = ">"
emitOperator GreaterThanOrEqual = ">="
emitOperator Plus = "+"
emitOperator Minus = "-"
emitOperator Multiply = "*"
emitOperator Divide = "/"
emitOperator Not = "!"
emitOperator Modulo = "%"

emitRuntimeTypeInfo :: (Emit :> es) => ValueTypeExpr -> Eff es ()
emitRuntimeTypeInfo Any = emit "make_type_any()"
emitRuntimeTypeInfo Undefined = emit "make_type_undefined()"
emitRuntimeTypeInfo Void = emit "make_type_void()"
emitRuntimeTypeInfo Bool = emit "make_type_bool()"
emitRuntimeTypeInfo Int = emit "make_type_int()"
emitRuntimeTypeInfo (NamespacedIdentifier _) = undefined
emitRuntimeTypeInfo (Function args ty@(TST TypeExpr {valueExpr = ret} _)) = do
  preamble do
    emit "  Type _function_type;\n"
    emit "  {\n"
    emit "    Type _return_type = "
    emitRuntimeTypeInfo ret
    emit ";\n"

    forM_ (args `zip` [0 :: Int ..]) $ uncurry \(TST TypeExpr {valueExpr = arg} _) i -> do
      emit "    Type _arg"
      emit $ Text.show i
      emit " = "
      emitRuntimeTypeInfo arg
      emit ";\n"

    emit "    Type _args["
    emit $ Text.show $ length args
    emit "] = {"
    forM_ (intersperse Nothing $ Just <$> [0 .. length args - 1]) $ \case
      Just i -> do
        emit "_arg"
        emit $ Text.show i
      Nothing -> emit ", "
    emit "};\n"

    emit "    _function_type = make_type_fn(_return_type, "
    emit $ Text.show $ length args
    emit ", _args);\n"

    emit "  }\n"
  emit "_function_type"

emitRHS :: (Emit :> es) => RHS -> Eff es ()
emitRHS RUndefined = do
  abort
  emit "  fprintf(stderr, \"ERROR: evaluated 'undefined'\\n\");\n"
  emit "  exit(1)"
emitRHS RVoid = emit "0"
emitRHS (RInt n) = emit $ Text.show n
emitRHS (RBool b) = emit if b then "true" else "false"
emitRHS (RBinOp op l r) = do
  emit $ Text.show l
  emit " "
  emit $ emitOperator op
  emit " "
  emit $ Text.show r
emitRHS (RUnaryOp op name) = do
  emit $ emitOperator op
  emit $ Text.show name
emitRHS (RCall fn args) = do
  emit $ Text.show fn
  emit "("
  forM_ (intersperse Nothing $ Just <$> args) $ \case
    Just arg -> emit $ Text.show arg
    Nothing -> emit ", "
  emit ")"
emitRHS (RBuiltin name) = do
  emit name
emitRHS (RBox TypeExpr {valueExpr = ty} name) = do
  emit "box_value(&"
  emit $ Text.show name
  emit ", "
  emitRuntimeTypeInfo ty
  emit ")"

emitC :: (Emit :> es) => Program -> Eff es ()
emitC (Program blocks) = do
  emit
    [text|
      #include <stdlib.h>
      #include <stddef.h>
      #include <stdio.h>
      #include <stdint.h>
      #include <stdbool.h>
      #include <runtime.h>

      int main() {
    |]
  emit "\n"

  -- declare all local variables at the top
  forM_ blocks $ \(_, Block {phis, instructions}) -> do
    forM_ instructions $ \(SSA {ty, name}) -> do
      emit "  "
      emitType (do emit " "; emit $ Text.show name) ty
      emit " = "
      emitZeroValue ty
      emit ";\n"
    forM_ phis $ \(Phi {ty, name}) -> do
      emit "  "
      emitType (do emit " "; emit $ Text.show name) ty
      emit " = "
      emitZeroValue ty
      emit ";\t/* phi */\n"
  emit "\n"

  -- then generate the blocks
  forM_ blocks $ \(blockId, Block {instructions, control}) -> do
    -- the block begins with a label
    emit $ Text.show blockId
    emit ":\n"

    -- then we generate the instructions
    forM_ instructions $ \(SSA {name, rhs}) -> do
      -- the RHS might need to generate some preamble
      -- so we flush the stream to mark that the preamble will get placed here, right before the instruction
      flush
      emit "  "
      emit $ Text.show name
      emit " = "
      emitRHS rhs
      emit ";\n"

    -- then we fill out any phis that we are reponsible for
    let nextBlocks = case control of
          Halt -> []
          Jump b -> [b]
          JumpIf _ b1 b2 -> [b1, b2]
    forM_ nextBlocks $ \nextBlockId -> do
      let (_, nextBlock) = blocks `getSingleElement` (\(id, _) -> id == nextBlockId) `orElse` error "unreachable"
      forM_ nextBlock.phis $ \Phi {name = toRemote, operands} -> do
        let operands' = filter (\(b, _) -> b == blockId) operands
        forM_ operands' $ \(_, fromLocal) -> do
          emit "  "
          emit $ Text.show toRemote
          emit " = "
          emit $ Text.show fromLocal
          emit ";\t/* phi */\n"

    -- lastly, we generate the control instruction
    emit "  "
    case control of
      Halt -> emit "goto __ret;\n"
      Jump toBlock -> do
        emit "goto "
        emit $ Text.show toBlock
        emit ";\n"
      JumpIf name target1 target2 -> do
        emit "if ("
        emit $ Text.show name
        emit ") { goto "
        emit $ Text.show target1
        emit "; } else { goto "
        emit $ Text.show target2
        emit "; }\n"
    emit "\n"

  -- generate the return block
  emit "__ret:\n  return 0;\n"

  emit
    [text|
      }
    |]
  emit "\n"

runEmitC :: (HasCallStack) => Program -> Eff es Text
runEmitC = runEmit . emitC
