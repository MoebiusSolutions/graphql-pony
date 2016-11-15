use "ponytest"

actor Main is TestList
  new create(env: Env) =>
    PonyTest(env, this)

  new make() =>
    None

  fun tag tests(test: PonyTest) =>
    test(_TestAst)

class iso _TestAst is UnitTest
  fun name():String => "ast"

  fun apply(h: TestHelper) =>
    h.assert_eq[String]("123", "123")
