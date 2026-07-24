module DW.ConstExprPass (runConstExprPass) where

import DW.AST
import DW.AST.Visit
import DW.Common
import DW.Error (markSpan)

visitor :: (Errors Err :> es, Log :> es) => Visitor (Eff es)
visitor = defaultVisitor {onTopLevelStmt}
  where
    onTopLevelStmt (AST (TLet {ty, value}) _) recurse = do
      case ty of
        Just ty@(AST TypeExpr {valueExpr = Any} _) -> markSpan (spanOf ty) TopLevelBoxedType
        _ -> return ()
      case node value of
        VoidLit -> return ()
        IntLit _ -> return ()
        BoolLit _ -> return ()
        Lambda {} -> return ()
        _ -> markSpan (spanOf value) NonConstTopLevelExpression
      recurse

constExprPass :: (HasCallStack, Errors Err :> es, Log :> es) => AST TopLevel -> Eff es ()
constExprPass = runTopLevelVisitor visitor

runConstExprPass :: (HasCallStack, Errors Err :> es, Log :> es) => AST TopLevel -> Eff es ()
runConstExprPass = constExprPass
