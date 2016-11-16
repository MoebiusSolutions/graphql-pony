class GraphQLParser
  let env: Env

  new create(env': Env) =>
    env = env'

  fun parse(source: String): DocumentNode =>
    """
    Given a GraphQL source, parses it into a Document.
    Throws GraphQLError if a syntax error is encountered.
    """
    let lexer = GraphQLLexer(env, source)
    parseDocument(lexer)

  // Implements the parsing rules in the Document section.
  /**
  * Document : Definition+
  */
  fun parseDocument(lexer: GraphQLLexer): DocumentNode =>
    let start = lexer.token()
    expect(lexer, SOF)
    let definitions = Array[DefinitionNode]
    repeat
      true
      // definitions.push(parseDefinition(lexer))
    until skip(lexer, EOF) == false end
    DocumentNode(loc(lexer, start), definitions)

  fun expect(lexer: GraphQLLexer, kind: TokenKind) =>
    None

  // fun parseDefinition(lexer: GraphQLLexer): DefinitionNode =>
  //   DefinitionNode(loc(lexer, EOF), NameNode(), Array[NameNode])

  fun skip(lexer: GraphQLLexer, kind: TokenKind): Bool =>
    false

  fun loc(lexer: GraphQLLexer, kind: Token): Location =>
    Location(0, 0, kind, kind, Source("TODO", "TODO"))
