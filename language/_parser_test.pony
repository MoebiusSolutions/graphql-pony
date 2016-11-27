use "ponytest"

actor _ParserTest is TestList
  new make() =>
    None

  fun tag tests(test: PonyTest) =>
    describe_parser(test)
    describe_parse_value(test)
    describe_parse_type(test)

  fun tag describe_parser(test: PonyTest) =>
    test(object iso is UnitTest
      fun name(): String => "---- Describe: GraphQLParser ----"
      fun apply(h: TestHelper) => None
    end)
    test(_TestProvidesUsefulError)
    test(_TestProvidesUsefulErrorWithSource)
    test(_TestVariableInlineValues)
    test(_TestConstantDefaultValues)
    test(_TestDoesNotAcceptFragmentsNamedOn)
    test(_TestDoesNotAcceptFragmentsSpreadOfOn)
    test(_TestParsesMultibyteCharacters)
    test(_TestParsesKitchenSink)
    test(_TestAllowsNonKeywordsAnywhereANameIsAllowed)
    test(_TestParsesAnonymousMutationOperations)
    test(_TestParsesAnonymousSubscriptionOperations)
    test(_TestParsesNamedMutationOperations)
    test(_TestParsesNamedSubscriptionOperations)
    test(_TestItCreatesAst)
    test(_TestAllowsParsingWithoutSourceLocationInformation)

  // Example of inline tests
  fun tag describe_parse_value(test: PonyTest) =>
    test(object iso is UnitTest
      fun name(): String => "---- Describe: parse_value ----"
      fun apply(h: TestHelper) => None
    end)
    test(object iso is UnitTest
      fun name(): String => "parses null value"
      fun apply(h: TestHelper) ? =>
        let parser = GraphQLParser(h.env)
        try
          let v = parser.parse_value("null")
          h.assert_eq[ValueNode](NullValueNode(Location(0,4)), v)
        else
          h.env.out.print(parser.err.string())
          error
        end
    end)
    test(object iso is UnitTest
      fun name(): String => "parses list value"
      fun apply(h: TestHelper) ? =>
        let parser = GraphQLParser(h.env)
        try
          let v = parser.parse_value("""[123 "abc"]""")
          let e = ListValueNode(where
            loc' = Location(0,11),
            values' = Array[ValueNode].push(
              IntValueNode(where
                loc' = Location(1,4),
                value' = "123")
            ).push(
              StringValueNode(where
                loc' = Location(5,10),
                value' = "abc"
              )
            )
          )
          let e': String = e.string()
          let v': String = v.string()
          h.assert_eq[String](e', v')
          // TODO: h.assert_eq[ListValueNode](e, v)
        else
          h.env.out.print(parser.err.string())
          error
        end
    end)

  fun tag describe_parse_type(test: PonyTest) =>
    test(object iso is UnitTest
      fun name(): String => "---- Describe: parse_type ----"
      fun apply(h: TestHelper) => None
    end)
    test(_TestParsesWellKnownTypes)
    test(_TestParsesCustomTypes)
    test(_TestParsesListTypes)
    test(_TestParsesNonNullTypes)
    test(_TestParsesNestedTypes)

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
    let v = args(0).valueNode as StringValueNode
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

class iso _TestAllowsNonKeywordsAnywhereANameIsAllowed is UnitTest
  fun name(): String => "allows non-keywords anywhere a Name is allowed"
  fun apply(h: TestHelper) ? =>
    let nonKeywords = [
      "on",
      "fragment",
      "query",
      "mutation",
      "subscription",
      "true",
      "false"
    ]
    for keyword in nonKeywords.values() do
      var fragmentName = keyword
      // You can't define or reference a fragment named `on`.
      if keyword == "on" then
        fragmentName = "a"
      end
      let query = String().append(
"""query ${keyword} {
  ... ${fragmentName}
  ... on ${keyword} { field }
}
fragment ${fragmentName} on Type {
  ${keyword}(${keyword}: $${keyword}) @${keyword}(${keyword}: ${keyword})
}""")
        .replace("${keyword}", keyword)
        .replace("${fragmentName}", fragmentName)
      GraphQLParser(h.env).parse(query.clone())
    end

class iso _TestParsesAnonymousMutationOperations is UnitTest
  fun name(): String => "parses anonymous mutation operations"
  fun apply(h: TestHelper) ? =>
    GraphQLParser(h.env).parse("""
      mutation {
        mutationField
      }
      """)

class iso _TestParsesAnonymousSubscriptionOperations is UnitTest
  fun name(): String => "parses anonymous subscription operations"
  fun apply(h: TestHelper) ? =>
    GraphQLParser(h.env).parse("""
      subscription {
        subscriptionField
      }
      """)

class iso _TestParsesNamedMutationOperations is UnitTest
  fun name(): String => "parses named mutation operations"
  fun apply(h: TestHelper) ? =>
    GraphQLParser(h.env).parse("""
      mutation Foo {
        mutationField
      }
      """)

class iso _TestParsesNamedSubscriptionOperations is UnitTest
  fun name(): String => "parses named subscription operations"
  fun apply(h: TestHelper) ? =>
    GraphQLParser(h.env).parse("""
      subscription Foo {
        subscriptionField
      }
      """)

class iso _TestItCreatesAst is UnitTest
  fun name(): String => "it creates ast"
  fun apply(h: TestHelper) ? =>
    let source = """
    {
      node(id: 4) {
        id,
        name
      }
    }
    """

    let result = GraphQLParser(h.env).parse(source)
    let expect = DocumentNode(where
      loc' = Location(0, 41),
      definitions' = Array[DefinitionNode].push(
        OperationDefinitionNode(where
          loc' = Location(0, 40),
          operation' = TnQuery,
          name' = None,
          variableDefinitions' = None,
          directives' = None,
          selectionSet' = SelectionSetNode(
            Location(0, 40),
            Array[SelectionNode].push(
              FieldNode(where
                loc' = Location(4,38),
                alias' = None,
                name' = NameNode(Location(4,8), "node"),
                arguments' = Array[ArgumentNode].push(
                  ArgumentNode(where
                    loc' = Location(9,14),
                    name' = NameNode(Location(9,11), "id"),
                    value' = IntValueNode(Location(13,14), "4")
                  )
                ),
                directives' = Array[DirectiveNode],
                selectionSet' = SelectionSetNode(where
                  loc' = Location(16,38),
                  selections' = Array[SelectionNode].push(
                    FieldNode(where
                      loc' = Location(22,24),
                      alias' = None,
                      name' = NameNode(Location(22,24), "id"),
                      arguments' = Array[ArgumentNode],
                      directives' = Array[DirectiveNode],
                      selectionSet' = None
                    )
                  ).push(
                    FieldNode(where
                      loc' = Location(30,34),
                      alias' = None,
                      name' = NameNode(Location(30,34), "name"),
                      arguments' = Array[ArgumentNode],
                      directives' = Array[DirectiveNode],
                      selectionSet' = None
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    let expect': String = expect.string()
    let result': String = result.string()
    h.assert_eq[String](expect', result')

class iso _TestAllowsParsingWithoutSourceLocationInformation is UnitTest
  fun name(): String => "allows parsing without source location information"
  fun apply(h: TestHelper) ? =>
    // let source = new Source("{ id }");
    // let result = parse(source, { noLocation: true });
    GraphQLParser(h.env).parse("{ id }")
    // TODO

class iso _TestParsesWellKnownTypes is UnitTest
  fun name(): String => "parses well known types"
  fun apply(h: TestHelper) ? =>
    let parser = GraphQLParser(h.env)
    try
      let v = parser.parse_type("String")
      // h.assert_eq[ValueNode](NullValueNode(Location(1,1)), v)
    else
      h.env.out.print(parser.err.string())
      error
    end

class iso _TestParsesCustomTypes is UnitTest
  fun name(): String => "parses custom types"
  fun apply(h: TestHelper) ? =>
    let parser = GraphQLParser(h.env)
    try
      let v = parser.parse_type("MyType")
      // h.assert_eq[ValueNode](NullValueNode(Location(1,1)), v)
    else
      h.env.out.print(parser.err.string())
      error
    end

class iso _TestParsesListTypes is UnitTest
  fun name(): String => "parses list types"
  fun apply(h: TestHelper) ? =>
    let parser = GraphQLParser(h.env)
    try
      let v = parser.parse_type("[MyType]")
      // h.assert_eq[ValueNode](NullValueNode(Location(1,1)), v)
    else
      h.env.out.print(parser.err.string())
      error
    end

class iso _TestParsesNonNullTypes is UnitTest
  fun name(): String => "parses non-null types"
  fun apply(h: TestHelper) ? =>
    let parser = GraphQLParser(h.env)
    try
      let v = parser.parse_type("MyType!")
      // h.assert_eq[ValueNode](NullValueNode(Location(1,1)), v)
    else
      h.env.out.print(parser.err.string())
      error
    end

class iso _TestParsesNestedTypes is UnitTest
  fun name(): String => "parses nested types"
  fun apply(h: TestHelper) ? =>
    let parser = GraphQLParser(h.env)
    try
      let v = parser.parse_type("[MyType!]")
      // h.assert_eq[ValueNode](NullValueNode(Location(1,1)), v)
    else
      h.env.out.print(parser.err.string())
      error
    end
