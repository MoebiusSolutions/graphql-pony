use "ponytest"

actor Main is TestList
  new create(env: Env) =>
    PonyTest(env, this)

  new make() =>
    None

  fun tag tests(test: PonyTest) =>
    test(_TestLexerAdvance)
    test(_TestLexer)
    test(_TestParser)

class iso _TestLexerAdvance is UnitTest
  fun name(): String => "lexer advance"
  fun apply(h: TestHelper) =>
    let env = h.env
    let lexer = GraphQLLexer(h.env, """
      query{
         name,
         age
      }
      """)
    while (lexer.token().kind is EOF) == false do
      printToken(env, lexer.token())
      lexer.advance()
    end
    printToken(env, lexer.token())

  fun printToken(env: Env, token: Token) =>
    env.out.print("token: "
      + token.kind.string() + " "
      + token.value + " "
      + token.line.string())

class iso _TestLexer is UnitTest
  fun name():String => "lexer"
  fun apply(h: TestHelper) =>
    h.assert_eq[String]("123", "123")
    let lexer = GraphQLLexer(h.env,
      """
      this is
      a test! with$ pleft( pright)
      :=[]{|}
      aName_with_123_numbers
      { ...abc }
      test # This is a comment
      "Try a quoted string"
      number: 0
      number: 12
      number: 12.1
      number: 12.2e1
      done
      """
    )
    let env = h.env
    let tokens = lexer.values()
    try
      while tokens.has_next() do
        let token = tokens.next()
        env.out.print("token: "
          + token.kind.string() + " "
          + token.value + " "
          + token.line.string())
      end
    end

class iso _TestParser is UnitTest
  fun name(): String => "parser"
  fun apply(h: TestHelper) =>
    let query = """
      query FetchLukeAliased {
        luke: human(id: "1000") {
          name
        }
      }
      """
    let parser = GraphQLParser(h.env)
    try
      let document = parser.parse(query)
      h.assert_eq[String]("Document", document.kind)
    else
      h.env.out.print(parser.err.string())
    end
