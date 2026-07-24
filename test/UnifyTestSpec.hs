module UnifyTestSpec where

import DW.Common (isLeft)
import DW.UnifyTest.Internal

import Test.Hspec

spec = describe "the unification tests" do
  describe "can unify correctly" do
    it "can unify basic types" do
      (mkTyVar 1 `unify` mkTyVar 2) `shouldBe` Right (substFromList [(TyVarId 1, mkTyVar 2)])
      ((CTy $ Constraint {typeclass = "Foo", params = [mkTyVar 1, mkTyVar 1]}) `unify` (CTy $ Constraint {typeclass = "Foo", params = [Int, Bool]}))
        `shouldBe` Left "type mismatch: Int and Bool"
      ((CTy $ Constraint {typeclass = "Foo", params = [mkTyVar 1, Bool]}) `unify` (CTy $ Constraint {typeclass = "Bar", params = [Int, mkTyVar 1]}))
        `shouldBe` Left "incompatible typeclasses: Foo and Bar"
      ((CTy $ Constraint {typeclass = "Foo", params = [mkTyVar 1, mkTyVar 1]}) `unify` (CTy $ Constraint {typeclass = "Foo", params = [mkTyVar 2, Int]}))
        `shouldBe` Right (substFromList [(TyVarId 1, Int), (TyVarId 2, Int)])
    it "rejects recursive types via occurs check" do
      let constraint ty = CTy $ Constraint {typeclass = "Foo", params = [ty]}
      (mkTyVar 1 `unify` constraint (mkTyVar 1)) `shouldBe` Left "recursive type"
    it "rejects mismatched param counts in constraints" do
      let foo1 = CTy $ Constraint {typeclass = "Foo", params = [Int]}
      let foo2 = CTy $ Constraint {typeclass = "Foo", params = [Int, Bool]}
      (foo1 `unify` foo2) `shouldBe` Left "incompatible param counts: 1 and 2"
    it "unifies matching constraints with concrete types" do
      let foo1 = CTy $ Constraint {typeclass = "Foo", params = [Int, Bool]}
      let foo2 = CTy $ Constraint {typeclass = "Foo", params = [Int, Bool]}
      (foo1 `unify` foo2) `shouldBe` Right mkSubst
    it "rejects mismatched primitive types" do
      isLeft (Int `unify` Bool) `shouldBe` True
      isLeft (Void `unify` Int) `shouldBe` True
    it "allows re-unification of a type variable with the same type" do
      let foo1 = CTy $ Constraint {typeclass = "Foo", params = [mkTyVar 1, mkTyVar 1]}
      let foo2 = CTy $ Constraint {typeclass = "Foo", params = [Int, Int]}
      (foo1 `unify` foo2) `shouldBe` Right (substFromList [(TyVarId 1, Int)])

  describe "can perform instance resolution correctly" do
    it "can resolve concrete typeclass instances" do
      -- typeclass Foo(T) {
      --     fn foo();
      -- }
      -- instance Foo(int) { .. }
      -- Foo::foo[int]();
      let program = Program [Typeclass "Foo" [TyVarId 1] [Method "foo"]] [Instance "Foo" [Int]]
      let call = Call "Foo" "foo" [Int] []
      resolveInstance call program `shouldBe` Right (ResolveConcrete (Instance "Foo" [Int]))
    it "fails on ambigous instances" do
      -- typeclass Foo(T) {
      --     fn foo();
      -- }
      -- instance Foo(int) { .. }
      -- instance[T] Foo(T) { .. }
      -- Foo::foo[int]();
      let program = Program [Typeclass "Foo" [TyVarId 1] [Method "foo"]] [Instance "Foo" [Int], Instance "Foo" [mkTyVar 2]]
      let call = Call "Foo" "foo" [Int] []
      resolveInstance call program `shouldBe` Left "ambiguous - multiple matching instances"
    it "can resolve polymorphic typeclass instances from the generic context" do
      -- typeclass Foo(T) {
      --     fn foo();
      -- }
      -- fn bar[T]() where Foo(T) {
      --     Foo::foo[int]();
      -- }
      let program = Program [Typeclass "Foo" [TyVarId 1] [Method "foo"]] []
      let call = Call "Foo" "foo" [mkTyVar 2] [Constraint "Foo" [mkTyVar 2]]
      resolveInstance call program `shouldBe` Right ResolvePolymorphic
    it "fails instance resolution for wrong type argument count" do
      -- typeclass Foo(T, K) {
      --     fn foo();
      -- }
      -- Foo::foo[int]();
      let program = Program [Typeclass "Foo" [TyVarId 1, TyVarId 2] [Method "foo"]] []
      let call = Call "Foo" "foo" [Int] []
      resolveInstance call program `shouldBe` Left "wrong number of type arguments"
    it "fails instance resolution for missing method" do
      -- typeclass Foo(T) {
      --     fn foo();
      -- }
      -- Foo::bar[int]();
      let program = Program [Typeclass "Foo" [TyVarId 1] [Method "foo"]] []
      let call = Call "Foo" "bar" [Int] []
      resolveInstance call program `shouldBe` Left "no such method bar"
    it "fails polymorphic resolution when no constraint matches" do
      -- typeclass Foo(T) {
      --     fn foo();
      -- }
      -- typeclass Bar(T) {
      --     fn bar();
      -- }
      -- fn baz[T]() where Bar(T) {
      --     Foo::foo[T]();
      -- }
      let program = Program [Typeclass "Foo" [TyVarId 1] [Method "foo"], Typeclass "Bar" [TyVarId 2] [Method "bar"]] []
      let call = Call "Foo" "foo" [mkTyVar 3] [Constraint "Bar" [mkTyVar 3]]
      resolveInstance call program `shouldBe` Left "no matching constraint in context"
