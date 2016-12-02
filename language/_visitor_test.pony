use "ponytest"

actor _VisitorTest is TestList
  new make() =>
    None

  fun tag tests(test: PonyTest) =>
    describe_visitor(test)

  fun tag describe_visitor(test: PonyTest) =>
    test(object iso is UnitTest
      fun name(): String => "---- Describe: Visitor ----"
      fun apply(h: TestHelper) => None
    end)
    test(_TestAllowsEditingANodeBothOnEnterAndOnLeave)
    test(_TestAllowsEditingTheRootNodeOnEnterAndOnLeave)
    test(_TestAllowsForEditingOnEnter)
    test(_TestAllowsForEditingOnLeave)
    test(_TestVisitsEditedNode)
    test(_TestAllowsSkippingASubTree)
    test(_TestAllowsExitWhileVisiting)
    test(_TestAllowsEarlyExitWhileLeaving)
    test(_TestAllowsANamedFunctionsVisitorApi)
    test(_TestVisitsKitchenSink)

class iso _TestAllowsEditingANodeBothOnEnterAndOnLeave is UnitTest
  fun name(): String => "allows editing a node both on enter and on leave"
  fun apply(h: TestHelper) ? =>
    let ast = GraphQLParser(h.env).parse("{ a, b, c { a, b, c } }")
    let visit = Visit(h.env)
    try
      let editedAst = visit(ast, object is Visitor
        let env: Env = h.env
        fun enter(node: ASTNode): VisitorResponse =>
          env.out.print("enter: " + node.string())
          None
        fun leave(node: ASTNode): VisitorResponse =>
          env.out.print("leave: " + node.string())
          None
      end)
    else
      h.assert_true(false, "visit.err", visit.loc)
    end

class iso _TestAllowsEditingTheRootNodeOnEnterAndOnLeave is UnitTest
  fun name(): String => "allows editing the root node on enter and on leave"
  fun apply(h: TestHelper) =>
    None

class iso _TestAllowsForEditingOnEnter is UnitTest
  fun name(): String => "allows for editing on enter"
  fun apply(h: TestHelper) =>
    None

class iso _TestAllowsForEditingOnLeave is UnitTest
  fun name(): String => "allows for editing on leave"
  fun apply(h: TestHelper) =>
    None
class iso _TestVisitsEditedNode is UnitTest
  fun name(): String => "visits edited node"
  fun apply(h: TestHelper) =>
    None
class iso _TestAllowsSkippingASubTree is UnitTest
  fun name(): String => "allows skipping a sub-tree"
  fun apply(h: TestHelper) =>
    None
class iso _TestAllowsExitWhileVisiting is UnitTest
  fun name(): String => "allows early exit while visiting"
  fun apply(h: TestHelper) =>
    None
class iso _TestAllowsEarlyExitWhileLeaving is UnitTest
  fun name(): String => "allows early exit while leaving"
  fun apply(h: TestHelper) =>
    None
class iso _TestAllowsANamedFunctionsVisitorApi is UnitTest
  fun name(): String => "allows a named functions visitor API"
  fun apply(h: TestHelper) =>
    None
class iso _TestVisitsKitchenSink is UnitTest
  fun name(): String => "visits kitchen sink"
  fun apply(h: TestHelper) =>
    None
