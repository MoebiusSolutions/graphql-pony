use "ponytest"

actor Main is TestList
  new create(env: Env) =>
    PonyTest(env, this)

  new make() =>
    None

  fun tag tests(test: PonyTest) =>
    test(_TestProvidesUsefulError)
    test(_TestProvidesUsefulErrorWithSource)
    test(_TestVariableInlineValues)
    test(_TestConstantDefaultValues)
    test(_TestDoesNotAcceptFragmentsNamedOn)
    test(_TestDoesNotAcceptFragmentsSpreadOfOn)
    test(_TestParsesMultibyteCharacters)
    test(_TestParsesKitchenSink)
    test(_TestLexerAdvance)
    test(_TestLexer)
    test(_TestParser)

class iso _TestLexerAdvance is UnitTest
  fun name(): String => "lexer advance"
  fun apply(h: TestHelper) =>
    let env = h.env
    let lexer = GraphQLLexer(h.env, """
      query{
         method(id: [1, 2])
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

class iso _TestProvidesUsefulError is UnitTest
  fun name(): String => "parse provides useful errors"
  fun apply(h: TestHelper) =>
    let parser = GraphQLParser(h.env)
    try
      let document = parser.parse("{")
      h.fail("Should have raised an error")
    else
      h.assert_eq[String](
        "Syntax Error GraphQL (1:2) Expected NAME, found EOF",
        parser.err.string()
      )
      h.assert_eq[USize](1, parser.err.locations.size())
      try
        h.assert_eq[U32](1, parser.err.locations(0)._1)
        h.assert_eq[U32](2, parser.err.locations(0)._2)
      else
        h.fail("Failed to access item 0")
      end
    end

    expect_error(h,
      """{ ...MissingOn }
fragment MissingOn Type
      """,
      """Syntax Error GraphQL (2:20) Expected "on", found NAME "Type""""
    )
    expect_error(h,
      "{ field: {} }",
      """Syntax Error GraphQL (1:10) Expected NAME, found {"""
    )
    expect_error(h,
      "notanoperation Foo { field }",
      """Syntax Error GraphQL (1:1) Unexpected NAME "notanoperation""""
    )
    expect_error(h, "...",
      """Syntax Error GraphQL (1:1) Unexpected SPREAD "..."""")

  fun expect_error(h: TestHelper, given: String, expect: String) =>
    let parser = GraphQLParser(h.env)
    try
      let document = parser.parse(given)
      h.fail("Should have raised an error")
    else
      h.assert_eq[String](
        expect,
        parser.err.string()
      )
    end

class iso _TestProvidesUsefulErrorWithSource is UnitTest
  fun name(): String => "parse provides useful error when using source"
  fun apply(h: TestHelper) =>
    // TODO
    None

class iso _TestVariableInlineValues is UnitTest
  fun name(): String => "parses variable inline values"
  fun apply(h: TestHelper) ? =>
    GraphQLParser(h.env).parse("{ field(complex: { a: { b: [ $var ] } }) }")

class iso _TestConstantDefaultValues is UnitTest
  fun name(): String => "parses constant default values"
  fun apply(h: TestHelper) =>
    let parser = GraphQLParser(h.env)
    try
      parser.parse(
        "query Foo($x: Complex = { a: { b: [ $var ] } }) { field }")
      h.fail("Should have raised an error")
    else
      h.assert_eq[String](
        """Syntax Error GraphQL (1:37) Unexpected DOLLAR "$"""",
        parser.err.string()
      )
    end

class iso _TestDoesNotAcceptFragmentsNamedOn is UnitTest
  fun name(): String => """does not accept fragments named "on""""
  fun apply(h: TestHelper) =>
    let parser = GraphQLParser(h.env)
    try
      parser.parse("fragment on on on { on }")
      h.fail("Should have raised an error")
    else
      h.assert_eq[String](
        """Syntax Error GraphQL (1:10) Unexpected NAME "on"""",
        parser.err.string()
      )
    end

class iso _TestDoesNotAcceptFragmentsSpreadOfOn is UnitTest
  fun name(): String => """does not accept fragments spread of "on""""
  fun apply(h: TestHelper) =>
    let parser = GraphQLParser(h.env)
    try
      parser.parse("{ ...on }")
      h.fail("Should have raised an error")
    else
      h.assert_eq[String](
        """Syntax Error GraphQL (1:9) Expected NAME, found }""",
        parser.err.string()
      )
    end

class iso _TestParsesMultibyteCharacters is UnitTest
  fun name(): String => "parses multi-byte characters"
  fun apply(h: TestHelper) ? =>
    let doc = GraphQLParser(h.env).parse("""
      # This comment has a \u0A0A multi-byte character.
      { field(arg: "Has a \u0A0A multi-byte character.") }
      """)
    h.assert_eq[USize](1, doc.definitions.size())
    let ops = doc.definitions(0) as OperationDefinitionNode
    h.assert_eq[USize](1, ops.selectionSet.selections.size())
    let f = ops.selectionSet.selections(0) as FieldNode
    let args = f.arguments as Array[ArgumentNode]
    h.assert_eq[USize](1, args.size())
    let v = args(0).value as StringValueNode
    h.assert_eq[String]("StringValue", v.kind.string())
    // TODO - below fails
    // h.assert_eq[String]("Has a \u0A0A multi-byte character.", v.value)

class iso _TestParsesKitchenSink is UnitTest
  fun name(): String => "parses kitchen sink"
  fun apply(h: TestHelper) =>
    let parser = GraphQLParser(h.env)
    try
      parser.parse("""
# Copyright (c) 2015, Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

query queryName($foo: ComplexType, $site: Site = MOBILE) {
  whoever123is: node(id: [123, 456]) {
    id ,
    ... on User @defer {
      field2 {
        id ,
        alias: field1(first:10, after:$foo,) @include(if: $foo) {
          id,
          ...frag
        }
      }
    }
    ... @skip(unless: $foo) {
      id
    }
    ... {
      id
    }
  }
}

mutation likeStory {
  like(story: 123) @defer {
    story {
      id
    }
  }
}

subscription StoryLikeSubscription($input: StoryLikeSubscribeInput) {
  storyLikeSubscribe(input: $input) {
    story {
      likers {
        count
      }
      likeSentence {
        text
      }
    }
  }
}

fragment frag on Friend {
  foo(size: $size, bar: $b, obj: {key: "value"})
}

{
  unnamed(truthy: true, falsey: false, nullish: null),
  query
}
      """)
    else
      h.fail("Failed: " + parser.err.string())
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
