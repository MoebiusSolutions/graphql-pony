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
    parse_document(lexer)

  // Implements the parsing rules in the Document section.
  /**
  * Document : Definition+
  */
  fun ref parse_document(lexer: GraphQLLexer): DocumentNode ? =>
    let start = lexer.token()
    expect(lexer, SOF)
    let definitions = Array[DefinitionNode]
    repeat
      definitions.push(parse_definition(lexer))
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
  fun ref parse_definition(lexer: GraphQLLexer): DefinitionNode ? =>
    if (peek(lexer, BraceL)) then
      parse_operation_definition(lexer)
    elseif (peek(lexer, NAME)) then
      match lexer.token().value
        // Note: subscription is an experimental non-spec addition.
      | "query" =>
        parse_operation_definition(lexer)
      | "mutation" =>
        parse_operation_definition(lexer)
      | "subscription" =>
        parse_operation_definition(lexer)

      | "fragment" =>
        parse_fragment_definition(lexer)

        // Note: the Type System IDL is an experimental non-spec addition.
      | "schema" =>
        parse_type_system_definition(lexer)
      | "scalar" =>
        parse_type_system_definition(lexer)
      | "type" =>
        parse_type_system_definition(lexer)
      | "interface" =>
        parse_type_system_definition(lexer)
      | "union" =>
        parse_type_system_definition(lexer)
      | "enum" =>
        parse_type_system_definition(lexer)
      | "input" =>
        parse_type_system_definition(lexer)
      | "extend" =>
        parse_type_system_definition(lexer)
      | "directive" =>
        parse_type_system_definition(lexer)
      else
        unexpected(lexer)
        error
      end
    else
      unexpected(lexer)
      error
    end

  fun ref parse_operation_definition(lexer: GraphQLLexer): DefinitionNode =>
    let start = lexer.token()
    if peek(lexer, BraceL) then
      let loc' = loc(lexer, start)
      let operation' = TnQuery
      let name' = None
      let variableDefinitions' = None
      let directives' = None
      let selectionSet' = parse_selection_set(lexer)
      OperationDefinitionNode(
        loc', operation', name', variableDefinitions', directives', selectionSet')
    else
      let operation: OperationTypeNode = parse_operation_type(lexer)
      let name = if peek(lexer, NAME) then
        parse_name(lexer)
      else
        None
      end
      let loc' = loc(lexer, start)
      let operation' = operation
      let name' = name
      let variableDefinitions' = parse_variable_definitions(lexer)
      let directives' = parse_directives(lexer)
      let selectionSet' = parse_selection_set(lexer)
      OperationDefinitionNode(
        loc', operation', name', variableDefinitions', directives', selectionSet')
    end

  fun ref parse_selection_set(lexer: GraphQLLexer): SelectionSetNode =>
    let start = lexer.token()
    SelectionSetNode(loc(lexer, start), many(lexer, BraceL, parse_selection, BraceR))

  fun ref parse_selection(lexer: GraphQLLexer) =>
    if peek(lexer, SPREAD) then
      parse_fragment(lexer)
    else
      parse_field(lexer)
    end

  // Implements the parsing rules in the Fragments section.

  fun ref parse_fragment(lexer: GraphQLLexer): (FragmentSpreadNode|InlineFragmentNode) =>
    """
    Corresponds to both FragmentSpread and InlineFragment in the spec.

    FragmentSpread : ... FragmentName Directives?

    InlineFragment : ... TypeCondition? Directives? SelectionSet
    """
    let start = lexer.token()
    expect(lexer, SPREAD)
    if peek(lexer, NAME) and (lexer.token().value != "on") then
      let name = parse_fragment_name(lexer)
      let directives = parse_directives(lexer)
      FragmentSpreadNode(loc(lexer, start), name, directives)
    else
      let typeCondition = if (lexer.token().value == "on") then
        lexer.advance()
        parse_named_type(lexer)
      else
        None
      end
      let directives = parse_directives(lexer)
      let selectionSet = parse_selection_set(lexer)
      FragmentSpreadNode(loc(lexer, start), typeCondition, directives, selectionSet)
    end

  fun ref parse_field(lexer: GraphQLLexer) =>
    None

  fun ref parse_operation_type(lexer: GraphQLLexer): OperationTypeNode =>
    None

  fun ref parse_name(lexer: GraphQLLexer) =>
    None

  fun ref parse_named_type(lexer: GraphQLLexer) =>
    None

  fun ref parse_variable_definitions(lexer: GraphQLLexer) =>
    None

  fun ref parse_directives(lexer: GraphQLLexer): Array[DirectiveNode] =>
    """
    Directives : Directive+
    """
    let directives = Array[DirectiveNode]
    while peek(lexer, AT) do
      directives.push(parse_directive(lexer))
    end
    directives

  fun ref parse_fragment_definition(lexer: GraphQLLexer): DefinitionNode =>
    // TODO
    lexer.advance()

    let loc' = loc(lexer, lexer.token())
    let name' = NameNode("fragment")
    let typeCondition' = NamedTypeNode(loc', NameNode("condition"))
    let selectionSet' = SelectionSetNode(loc', Array[SelectionNode])
    FragmentDefinitionNode(loc', name', typeCondition', selectionSet')

  fun parse_type_system_definition(lexer: GraphQLLexer): DefinitionNode =>
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
