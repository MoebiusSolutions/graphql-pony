class GraphQLParser
  let env: Env
  var err: GraphQLError = GraphQLError("", 0, "Unknown error")

  new create(env': Env) =>
    env = env'

  fun ref parse(source': String): DocumentNode ? =>
    """
    Given a GraphQL source, parses it into a Document.
    Throws GraphQLError if a syntax error is encountered.
    """
    let lexer = GraphQLLexer(env, source')
    parseDocument(lexer)

  // Implements the parsing rules in the Document section.
  /**
  * Document : Definition+
  */
  fun ref parseDocument(lexer: GraphQLLexer): DocumentNode ? =>
    let start = lexer.token()
    expect(lexer, SOF)
    let definitions = Array[DefinitionNode]
    repeat
      definitions.push(parseDefinition(lexer))
    until skip(lexer, EOF) end
    DocumentNode(loc(lexer, start), definitions)

  /**
   * If the next token is of the given kind, return that token after advancing
   * the lexer. Otherwise, do not change the parser state and throw an error.
   */
  fun ref expect(lexer: GraphQLLexer, kind: TokenKind): Token ? =>
    let token' = lexer.token()
    if token'.kind is kind then
      lexer.advance()
      return token'
    end
    syntax_error(
      "TODO",
      token'.line,
      "Expected "+ kind.string() +", found "+ kind.string()
    )
    error

    /**
     * Helper function for creating an error when an unexpected lexed token
     * is encountered.
     */
  fun ref unexpected(lexer: GraphQLLexer, atToken: (Token|None) = None) =>
    let token' = match atToken
    | let t: Token => t
    else lexer.token() end
    syntax_error(
      "lexer.source",
      token'.line,
      "Unexpected " + token'.kind.string() + "=" + token'.value
    )

  fun ref syntax_error(source': String, line: U32, message: String) =>
    err = GraphQLError(source', line, message)
    None

  /**
   * Definition :
   *   - OperationDefinition
   *   - FragmentDefinition
   *   - TypeSystemDefinition
   */
  fun ref parseDefinition(lexer: GraphQLLexer): DefinitionNode ? =>
    if (peek(lexer, BraceL)) then
      parseOperationDefinition(lexer)
    elseif (peek(lexer, NAME)) then
      match lexer.token().value
        // Note: subscription is an experimental non-spec addition.
      | "query" =>
        parseOperationDefinition(lexer)
      | "mutation" =>
        parseOperationDefinition(lexer)
      | "subscription" =>
        parseOperationDefinition(lexer)

      | "fragment" =>
        parseFragmentDefinition(lexer)

        // Note: the Type System IDL is an experimental non-spec addition.
      | "schema" =>
        parseTypeSystemDefinition(lexer)
      | "scalar" =>
        parseTypeSystemDefinition(lexer)
      | "type" =>
        parseTypeSystemDefinition(lexer)
      | "interface" =>
        parseTypeSystemDefinition(lexer)
      | "union" =>
        parseTypeSystemDefinition(lexer)
      | "enum" =>
        parseTypeSystemDefinition(lexer)
      | "input" =>
        parseTypeSystemDefinition(lexer)
      | "extend" =>
        parseTypeSystemDefinition(lexer)
      | "directive" =>
        parseTypeSystemDefinition(lexer)
      else
        unexpected(lexer)
        error
      end
    else
      unexpected(lexer)
      error
    end

  fun parseOperationDefinition(lexer: GraphQLLexer): DefinitionNode =>
    // TODO
    lexer.advance()

    let loc' : Location = loc(lexer, lexer.token())
    let operation': OperationTypeNode = TnQuery
    let name': NameNode = NameNode("zzz")
    let variableDefinitions': (Array[VariableDefinitionNode]|None) = None
    let directives': (Array[DirectiveNode]|None) = None
    let selectionSet': SelectionSetNode = SelectionSetNode(loc', Array[SelectionNode])
    OperationDefinitionNode(loc', TnQuery, name', variableDefinitions', directives', selectionSet')

  fun parseFragmentDefinition(lexer: GraphQLLexer): DefinitionNode =>
    // TODO
    lexer.advance()

    let loc' = loc(lexer, lexer.token())
    let name' = NameNode("fragment")
    let typeCondition' = NamedTypeNode(loc', NameNode("condition"))
    let selectionSet' = SelectionSetNode(loc', Array[SelectionNode])
    FragmentDefinitionNode(loc', name', typeCondition', selectionSet')

  fun parseTypeSystemDefinition(lexer: GraphQLLexer): DefinitionNode =>
    // TODO
    lexer.advance()

    let directives' = Array[DirectiveNode]
    let operationTypes' = Array[OperationTypeDefinitionNode]
    SchemaDefinitionNode(loc(lexer, lexer.token()), directives', operationTypes')

  fun peek(lexer: GraphQLLexer, kind: TokenKind): Bool =>
    """
    Determines if the next token is of a given kind
    """
    lexer.token().kind is kind

  fun skip(lexer: GraphQLLexer, kind: TokenKind): Bool =>
    """
    If the next token is of the given kind, return true after advancing
    the lexer. Otherwise, do not change the parser state and return false.
    """
    let match': Bool = lexer.token().kind is kind
    if match' then
      lexer.advance()
    end
    match'

  fun loc(lexer: GraphQLLexer, kind: Token): Location =>
    Location(0, 0, kind, kind, Source("TODO", "TODO"))
