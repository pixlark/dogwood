{-# LANGUAGE QuasiQuotes #-}

module EmitC where

import Common
import Compiler (UserReference)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.List (intersperse)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import IR
import NeatInterpolation
import TypedAST
import Util (getSingleElement, orElse)

emitValueType :: forall es. (Writer Text :> es) => Eff es () -> ValueTypeExpr -> Eff es ()
emitValueType name Any = do tell "Box"; name
emitValueType name Undefined = do tell "uint8_t"; name -- for now
emitValueType name Void = do tell "uint8_t"; name -- for now
emitValueType name Bool = do tell "bool"; name
emitValueType name Int = do tell "int64_t"; name
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
        Nothing -> tell ", "
    functionPointerType :: ValueTypeExpr -> Eff es (Eff es (), Eff es ())
    functionPointerType (Function args (TST (TypeExpr {valueExpr = innerFn@(Function _ _)}) _)) = do
      -- our return type is another function pointer
      (innerLeft, innerRight) <- functionPointerType innerFn
      let left = do innerLeft; tell "(*"
          right = do tell ")("; emitArguments args; tell ")"; innerRight
      return (left, right)
    functionPointerType (Function args (TST ret _)) = do
      -- our return type is a normal type
      let left = do emitType (return ()) ret; tell "(*"
          right = do tell ")("; emitArguments args; tell ")"
      return (left, right)
    functionPointerType _ = error "unreachable"

emitType :: (Writer Text :> es) => Eff es () -> TypeExpr -> Eff es ()
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
          then do tell "*"; name
          else name
  emitValueType name' valueExpr

emitZeroValue :: (Writer Text :> es) => TypeExpr -> Eff es ()
emitZeroValue (TypeExpr {reference = True}) = tell "NULL"
emitZeroValue (TypeExpr {valueExpr}) = emitZeroValue' valueExpr
  where
    emitZeroValue' :: (Writer Text :> es) => ValueTypeExpr -> Eff es ()
    emitZeroValue' Any = tell "{0}"
    emitZeroValue' Undefined = tell "0"
    emitZeroValue' Void = tell "0"
    emitZeroValue' Bool = tell "false"
    emitZeroValue' Int = tell "0"
    emitZeroValue' (NamespacedIdentifier _) = undefined
    emitZeroValue' (Function _ _) = tell "NULL"

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

emitRHS :: (Writer Text :> es) => RHS -> Eff es ()
emitRHS RUndefined = undefined
emitRHS RVoid = tell "0"
emitRHS (RInt n) = tell $ Text.show n
emitRHS (RBool b) = tell if b then "true" else "false"
emitRHS (RBinOp op l r) = do
  tell $ Text.show l
  tell " "
  tell $ emitOperator op
  tell " "
  tell $ Text.show r
emitRHS (RUnaryOp op name) = do
  tell $ emitOperator op
  tell $ Text.show name
emitRHS (RCall fn args) = do
  tell $ Text.show fn
  tell "("
  forM_ (intersperse Nothing $ Just <$> args) $ \case
    Just arg -> tell $ Text.show arg
    Nothing -> tell ", "
  tell ")"
emitRHS (RBuiltin name) = do
  tell name
emitRHS (RBox ty name) = do
  tell "box_value(&"
  tell $ Text.show name
  tell ", "
  case ty.valueExpr of
    Any -> undefined
    Undefined -> tell "make_type_void()"
    Void -> tell "make_type_void()"
    Bool -> tell "make_type_bool()"
    Int -> tell "make_type_int()"
    (NamespacedIdentifier _) -> undefined
    (Function args ret) -> tell "make_type_fn(make_type_void(), 0, NULL)"
  tell ")"

emitC :: (Writer Text :> es) => Program -> HashMap Name [UserReference] -> Eff es ()
emitC (Program blocks) userMap = do
  tell
    [text|
      #include <stdlib.h>
      #include <stddef.h>
      #include <stdio.h>
      #include <stdint.h>
      #include <stdbool.h>
      #include <runtime.h>

      int main() {
    |]
  tell "\n"

  -- declare all local variables at the top
  forM_ blocks $ \(_, Block {phis, instructions}) -> do
    forM_ instructions $ \(SSA {ty, name}) -> do
      tell "  "
      emitType (do tell " "; tell $ Text.show name) ty
      tell " = "
      emitZeroValue ty
      tell ";\n"
    forM_ phis $ \(Phi {ty, name}) -> do
      tell "  "
      emitType (do tell " "; tell $ Text.show name) ty
      tell " = "
      emitZeroValue ty
      tell ";\t/* phi */\n"
  tell "\n"

  -- then generate the blocks
  forM_ blocks $ \(blockId, Block {instructions, control}) -> do
    -- the block begins with a label
    tell $ Text.show blockId
    tell ":\n"

    -- then we generate the instructions
    forM_ instructions $ \(SSA {name, rhs}) -> do
      tell "  "
      tell $ Text.show name
      tell " = "
      emitRHS rhs
      tell ";\n"

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
          tell "  "
          tell $ Text.show toRemote
          tell " = "
          tell $ Text.show fromLocal
          tell ";\t/* phi */\n"

    -- lastly, we generate the control instruction
    tell "  "
    case control of
      Halt -> tell "goto __ret;\n"
      Jump toBlock -> do
        tell "goto "
        tell $ Text.show toBlock
        tell ";\n"
      JumpIf name target1 target2 -> do
        tell "if ("
        tell $ Text.show name
        tell ") { goto "
        tell $ Text.show target1
        tell "; } else { goto "
        tell $ Text.show target2
        tell "; }\n"
    tell "\n"

  -- generate the return block
  tell "__ret:\n  return 0;\n"

  tell
    [text|
      }
    |]
  tell "\n"

runEmitC :: Program -> HashMap Name [UserReference] -> Eff es Text
runEmitC program userMap = do
  (_, emitted) <- runWriter (emitC program userMap)
  return emitted
