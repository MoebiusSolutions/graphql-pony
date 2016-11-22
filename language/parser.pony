class GraphQLParser
  let env: Env
  var err: GraphQLError = GraphQLError("", 0, 0, "Unknown error")

  new create(env': Env) =>
    env = env'

  fun ref parse(source': String): DocumentNode ? =>
    """
    Given a GraphQL source, parses it into a Document.
    Throws GraphQLError if a syntax error is encountered.
    """
    let lexer = GraphQLLexer(env, source')
    parse_document(lexer)

  fun ref parse_value(source': String): ValueNode ? =>
    """
    Given a string containing a GraphQL value (ex. `[42]`), parse the AST for
    that value.
    Throws GraphQLError if a syntax error is encountered.

    This is useful within tools that operate upon GraphQL Values directly and
    in isolation of complete GraphQL documents.

    Consider providing the results to the utility function: valueFromAST().
    """
    let lexer = GraphQLLexer(env, source')
    expect(lexer, SOF)
    let value = parse_value_literal(lexer, false)
    expect(lexer, EOF)
    value

  fun ref parse_type(source': String): TypeNode ? =>
    """
    Given a string containing a GraphQL Type (ex. `[Int!]`), parse the AST for
    that type.
    Throws GraphQLError if a syntax error is encountered.

    This is useful within tools that operate upon GraphQL Types directly and
    in isolation of complete GraphQL documents.

    Consider providing the results to the utility function: typeFromAST().
    """
    let lexer = GraphQLLexer(env, source')
    expect(lexer, SOF)
    let type' = parse_type_reference(lexer)
    expect(lexer, EOF)
    type'

  // Implements the parsing rules in the Document section.

  fun ref parse_document(lexer: GraphQLLexer): DocumentNode ? =>
    """
    Document : Definition+
    """
    let start = lexer.token()
    expect(lexer, SOF)
    let definitions = Array[DefinitionNode]
    repeat
      definitions.push(parse_definition(lexer))
    until skip(lexer, EOF) end
    DocumentNode(loc(lexer, start), definitions)

  fun ref parse_definition(lexer: GraphQLLexer): DefinitionNode ? =>
    """
    Definition :
      - OperationDefinition
      - FragmentDefinition
      - TypeSystemDefinition
    """
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

  fun ref parse_operation_definition(lexer: GraphQLLexer): DefinitionNode ? =>
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

  fun ref parse_selection_set(lexer: GraphQLLexer): SelectionSetNode ? =>
    let start = lexer.token()
    let parseFn = {(self: GraphQLParser, lexer': GraphQLLexer): SelectionNode ? =>
      self.parse_selection(lexer')} ref
    SelectionSetNode(
      loc(lexer, start), many[SelectionNode](lexer, BraceL, parseFn, BraceR))

  fun ref parse_selection(lexer: GraphQLLexer): SelectionNode ? =>
    if peek(lexer, SPREAD) then
      parse_fragment(lexer)
    else
      parse_field(lexer)
    end

  // Implements the parsing rules in the Fragments section.

  fun ref parse_fragment(lexer: GraphQLLexer): (FragmentSpreadNode|InlineFragmentNode) ? =>
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
      InlineFragmentNode(loc(lexer, start),
        typeCondition, directives, selectionSet)
    end

  fun ref parse_field(lexer: GraphQLLexer): FieldNode ? =>
    """
    Field : Alias? Name Arguments? Directives? SelectionSet?

    Alias : Name :
    """
    let start = lexer.token()

    let nameOrAlias = parse_name(lexer)
    let isSkip = skip(lexer, COLON)
    let alias = if isSkip then nameOrAlias else None end
    let name = if isSkip then parse_name(lexer) else nameOrAlias end
    let arguments' = parse_arguments(lexer)
    let directives' = parse_directives(lexer)
    let selectionSet' = if peek(lexer, BraceL) then
      parse_selection_set(lexer)
    else
      None
    end
    FieldNode(loc(lexer, start), alias, name,
      arguments', directives', selectionSet')

  fun ref parse_operation_type(lexer: GraphQLLexer): OperationTypeNode ? =>
    """
    OperationType : one of query mutation subscription
    """
    let operationToken = expect(lexer, NAME)
    match operationToken.value
    | "query" => return TnQuery
    | "mutation" => return TnMutation
    // Note: subscription is an experimental non-spec addition.
    | "subscription" => return TnSubscription
    end
    unexpected(lexer, operationToken)
    error

  fun ref parse_name(lexer: GraphQLLexer): NameNode ? =>
    let token = expect(lexer, NAME)
    NameNode(loc(lexer, token), token.value)

  fun ref parse_named_type(lexer: GraphQLLexer): NamedTypeNode ? =>
    """
    NamedType : Name
    """
    let start = lexer.token()
    NamedTypeNode(loc(lexer, start), parse_name(lexer))

  fun ref parse_variable_definitions(lexer: GraphQLLexer): Array[VariableDefinitionNode] ? =>
    """
    VariableDefinitions : ( VariableDefinition+ )
    """
    if peek(lexer, ParenL) then
      let parseFn = {(self: GraphQLParser, lexer: GraphQLLexer): VariableDefinitionNode ? =>
        self.parse_variable_definition(lexer)} ref
      many[VariableDefinitionNode](lexer, ParenL, parseFn, ParenR)
    else
      Array[VariableDefinitionNode]
    end

  fun ref parse_variable_definition(lexer: GraphQLLexer): VariableDefinitionNode ? =>
    """
    VariableDefinition : Variable : Type DefaultValue?
    """
    let start = lexer.token()
    let variable' = parse_variable(lexer)
    expect(lexer, COLON)
    let type' = parse_type_reference(lexer)
    let defaultValue' = if skip(lexer, EQUALS) then
      parse_value_literal(lexer, true)
    else
      None
    end
    VariableDefinitionNode(
      loc(lexer, start),
      variable',
      type',
      defaultValue'
    )

  fun ref parse_directives(lexer: GraphQLLexer): Array[DirectiveNode] ? =>
    """
    Directives : Directive+
    """
    let directives = Array[DirectiveNode]
    while peek(lexer, AT) do
      directives.push(parse_directive(lexer))
    end
    directives

  fun ref parse_directive(lexer: GraphQLLexer): DirectiveNode ? =>
    """
    Directive : @ Name Arguments?
    """
    let start = lexer.token()
    expect(lexer, AT)
    let name' = parse_name(lexer)
    let arguments' = parse_arguments(lexer)
    DirectiveNode(loc(lexer, start), name', arguments')

  // Implements the parsing rules in the Types section.

  fun ref parse_type_reference(lexer: GraphQLLexer): TypeNode ? =>
    """
    Type :
      - NamedType
      - ListType
      - NonNullType
    """
    let start = lexer.token()
    let type' = if skip(lexer, BracketL) then
      let type'' = parse_type_reference(lexer)
      expect(lexer, BracketR)
      ListTypeNode(loc(lexer, start), type'')
    else
      parse_named_type(lexer)
    end

    if skip(lexer, BANG) then
      NonNullTypeNode(loc(lexer, start), type')
    else
      type'
    end

  fun ref parse_arguments(lexer: GraphQLLexer): Array[ArgumentNode] ? =>
    """
    Arguments : ( Argument+ )
    """
    if peek(lexer, ParenL) then
      let parseFn = {(self: GraphQLParser, lexer': GraphQLLexer): ArgumentNode ? =>
        self.parse_argument(lexer')} ref
      many[ArgumentNode](lexer, ParenL, parseFn, ParenR)
    else
      Array[ArgumentNode]
    end

  fun ref parse_argument(lexer: GraphQLLexer): ArgumentNode ? =>
    """
    Argument : Name : Value
    """
    let start = lexer.token()
    let name': NameNode = parse_name(lexer)
    expect(lexer, COLON)
    let value' = parse_value_literal(lexer, false)
    ArgumentNode(loc(lexer, start), name', value')

  // Implements the parsing rules in the Values section.

  fun ref parse_value_literal(lexer: GraphQLLexer, isConst: Bool): ValueNode ? =>
    """
    Value[Const] :
      - [~Const] Variable
      - IntValue
      - FloatValue
      - StringValue
      - BooleanValue
      - NullValue
      - EnumValue
      - ListValue[?Const]
      - ObjectValue[?Const]

    BooleanValue : one of `true` `false`

    NullValue : `null`

    EnumValue : Name but not `true`, `false` or `null`
    """
    let token = lexer.token()
    match token.kind
    | BracketL =>
      return parse_list(lexer, isConst)
    | BraceL =>
      return parse_object(lexer, isConst)
    | GraphQLInt =>
      lexer.advance()
      return IntValueNode(loc(lexer, token), token.value)
    | GraphQLFloat =>
      lexer.advance()
      return FloatValueNode(loc(lexer, token), token.value)
    | GraphQLString =>
      lexer.advance()
      return StringValueNode(loc(lexer, token), token.value)
    | NAME =>
      if (token.value == "true") or (token.value == "false") then
        lexer.advance()
        let v = if token.value == "true" then true else false end
        return BooleanValueNode(loc(lexer, token), v)
      elseif (token.value == "null") then
        lexer.advance()
        return NullValueNode(loc(lexer, token))
      end
      lexer.advance()
      return EnumValueNode(loc(lexer, token), token.value)
    | DOLLAR =>
      if (not isConst) then
        return parse_variable(lexer)
      end
    end
    unexpected(lexer)
    error

  fun ref parse_variable(lexer: GraphQLLexer): VariableNode ? =>
    """
    Variable: $ Name
    """
    let start = lexer.token()
    expect(lexer, DOLLAR)
    VariableNode(loc(lexer, start), parse_name(lexer))

  fun ref parse_const_value(lexer: GraphQLLexer): ValueNode ? =>
    parse_value_literal(lexer, true)

  fun ref parse_value_value(lexer: GraphQLLexer): ValueNode ? =>
    parse_value_literal(lexer, false)

  fun ref parse_list(lexer: GraphQLLexer, isConst: Bool): ListValueNode ? =>
    """
    ListValue[Const] :
      - [ ]
      - [ Value[?Const]+ ]
    """
    let start = lexer.token()
    let item = if isConst then
      {(self: GraphQLParser, lexer': GraphQLLexer): ValueNode ? =>
        self.parse_const_value(lexer')} ref
    else
      {(self: GraphQLParser, lexer': GraphQLLexer): ValueNode ? =>
        self.parse_value_value(lexer')} ref
    end
    let values': Array[ValueNode] = any[ValueNode](lexer, BracketL, item, BracketR)
    ListValueNode(loc(lexer, start), values')

  fun ref parse_object(lexer: GraphQLLexer, isConst: Bool): ObjectValueNode ? =>
    """
    ObjectValue[Const] :
      - { }
      - { ObjectField[?Const]+ }
    """
    let start = lexer.token()
    expect(lexer, BraceL)
    let fields = Array[ObjectFieldNode]
    while (not skip(lexer, BraceR)) do
      fields.push(parse_object_field(lexer, isConst))
    end
    ObjectValueNode(loc(lexer, start), fields)

  fun ref parse_object_field(lexer: GraphQLLexer, isConst: Bool): ObjectFieldNode ? =>
    """
    ObjectField[Const] : Name : Value[?Const]
    """
    let start = lexer.token()
    let name' = parse_name(lexer)
    expect(lexer, COLON)
    let value' = parse_value_literal(lexer, isConst)
    ObjectFieldNode(loc(lexer, start), name', value')

  fun ref parse_fragment_definition(lexer: GraphQLLexer): DefinitionNode ? =>
    """
    FragmentDefinition :
      - fragment FragmentName on TypeCondition Directives? SelectionSet

    TypeCondition : NamedType
    """
    let start = lexer.token()
    expect_keyword(lexer, "fragment")
    let name' = parse_fragment_name(lexer)
    expect_keyword(lexer, "on")
    let typeCondition' = parse_named_type(lexer)
    let directives' = parse_directives(lexer)
    let selectionSet' = parse_selection_set(lexer)
    FragmentDefinitionNode(loc(lexer, start),
      name', typeCondition', directives', selectionSet')

  fun ref parse_fragment_name(lexer: GraphQLLexer): NameNode ? =>
    """
    FragmentName : Name but not `on`
    """
    if lexer.token().value == "on" then
      unexpected(lexer)
      error
    end
    parse_name(lexer)

// Implements the parsing rules in the Type Definition section.

  fun ref parse_type_system_definition(
    lexer: GraphQLLexer
  ): TypeSystemDefinitionNode ? =>
    """
    TypeSystemDefinition :
      - SchemaDefinition
      - TypeDefinition
      - TypeExtensionDefinition
      - DirectiveDefinition

    TypeDefinition :
      - ScalarTypeDefinition
      - ObjectTypeDefinition
      - InterfaceTypeDefinition
      - UnionTypeDefinition
      - EnumTypeDefinition
      - InputObjectTypeDefinition
    """
    if peek(lexer, NAME) then
      match lexer.token().value
      | "schema" => return parse_schema_definition(lexer)
      | "scalar" => return parse_scalar_type_definition(lexer)
      | "type" => return parse_object_type_definition(lexer)
      | "interface" => return parse_interface_type_definition(lexer)
      | "union" => return parse_union_type_definition(lexer)
      | "enum" => return parse_enum_type_definition(lexer)
      | "input" => return parse_input_object_type_definition(lexer)
      | "extend" => return parse_type_extension_definition(lexer)
      | "directive" => return parse_directive_definition(lexer)
      end
    end
    unexpected(lexer)
    error

  fun ref parse_schema_definition(lexer: GraphQLLexer): SchemaDefinitionNode ? =>
    """
    SchemaDefinition : schema Directives? { OperationTypeDefinition+ }

    OperationTypeDefinition : OperationType : NamedType
    """
    let start = lexer.token()
    expect_keyword(lexer, "schema")
    let directives = parse_directives(lexer)
    let operationTypes = many[OperationTypeDefinitionNode](
      lexer,
      BraceL,
      {(s: GraphQLParser, l: GraphQLLexer):OperationTypeDefinitionNode ? =>
        s.parse_operation_type_definition(l)} ref,
      BraceR
    )
    SchemaDefinitionNode(loc(lexer, start),
      directives, operationTypes)

  fun ref parse_operation_type_definition(
    lexer: GraphQLLexer
  ): OperationTypeDefinitionNode ? =>
    let start = lexer.token()
    let operation = parse_operation_type(lexer)
    expect(lexer, COLON)
    let type' = parse_named_type(lexer)
    OperationTypeDefinitionNode(
      loc(lexer, start), operation, type')

  fun ref parse_scalar_type_definition(
    lexer: GraphQLLexer
  ): ScalarTypeDefinitionNode ? =>
    """
    ScalarTypeDefinition : scalar Name Directives?
    """
    let start = lexer.token()
    expect_keyword(lexer, "scalar")
    let name = parse_name(lexer)
    let directives = parse_directives(lexer)
    ScalarTypeDefinitionNode(loc(lexer, start), name, directives)

  fun ref parse_object_type_definition(lexer: GraphQLLexer): ObjectTypeDefinitionNode ? =>
    """
    ObjectTypeDefinition :
      - type Name ImplementsInterfaces? Directives? { FieldDefinition+ }
    """
    let start = lexer.token()
    expect_keyword(lexer, "type")
    let name = parse_name(lexer)
    let interfaces = parse_implements_interfaces(lexer)
    let directives = parse_directives(lexer)
    let fields = any[FieldDefinitionNode](
      lexer,
      BraceL,
      {(s: GraphQLParser, l: GraphQLLexer): FieldDefinitionNode ? =>
        s.parse_field_definition(l)} ref,
      BracketR
    )
    ObjectTypeDefinitionNode(loc(lexer, start), name, interfaces, directives, fields)

  fun ref parse_implements_interfaces(lexer: GraphQLLexer): Array[NamedTypeNode] ? =>
    """
    ImplementsInterfaces : implements NamedType+
    """
    let types = Array[NamedTypeNode]
    if lexer.token().value == "implements" then
      lexer.advance()
      repeat
        types.push(parse_named_type(lexer))
      until not peek(lexer, NAME) end
    end
    types

  fun ref parse_field_definition(lexer: GraphQLLexer): FieldDefinitionNode ? =>
    """
    FieldDefinition : Name ArgumentsDefinition? : Type Directives?
    """
    let start = lexer.token()
    let name = parse_name(lexer)
    let args = parse_argument_defs(lexer)
    expect(lexer, COLON)
    let type' = parse_type_reference(lexer)
    let directives = parse_directives(lexer)
    FieldDefinitionNode(loc(lexer, start), name, args, type', directives)

  fun ref parse_argument_defs(lexer: GraphQLLexer): Array[InputValueDefinitionNode] ? =>
    """
    ArgumentsDefinition : ( InputValueDefinition+ )
    """
    if not peek(lexer, ParenL) then
      return Array[InputValueDefinitionNode]
    end
    many[InputValueDefinitionNode](
      lexer,
      ParenL,
      {(s: GraphQLParser, l: GraphQLLexer): InputValueDefinitionNode ? =>
        s.parse_input_value_def(l)} ref,
      ParenR
    )

  fun ref parse_input_value_def(lexer: GraphQLLexer): InputValueDefinitionNode ? =>
    """
    InputValueDefinition : Name : Type DefaultValue? Directives?
    """
    let start = lexer.token()
    let name = parse_name(lexer)
    expect(lexer, COLON)
    let type' = parse_type_reference(lexer)
    let defaultValue = if skip(lexer, EQUALS) then
      parse_const_value(lexer)
    else
      None
    end
    let directives = parse_directives(lexer)
    InputValueDefinitionNode(loc(lexer, start), name, type', defaultValue, directives)

  fun ref parse_interface_type_definition(
    lexer: GraphQLLexer
  ): InterfaceTypeDefinitionNode ? =>
    """
    InterfaceTypeDefinition : interface Name Directives? { FieldDefinition+ }
    """
    let start = lexer.token()
    expect_keyword(lexer, "interface")
    let name = parse_name(lexer)
    let directives = parse_directives(lexer)
    let fields = any[FieldDefinitionNode](
      lexer,
      BraceL,
      {(s: GraphQLParser, l: GraphQLLexer): FieldDefinitionNode ? =>
        s.parse_field_definition(l)} ref,
      BraceR
    )
    InterfaceTypeDefinitionNode(loc(lexer, start), name, directives, fields)

  fun ref parse_union_type_definition(lexer: GraphQLLexer): UnionTypeDefinitionNode ? =>
    """
    UnionTypeDefinition : union Name Directives? = UnionMembers
    """
    let start = lexer.token()
    expect_keyword(lexer, "union")
    let name = parse_name(lexer)
    let directives = parse_directives(lexer)
    expect(lexer, EQUALS)
    let types = parse_union_members(lexer)
    UnionTypeDefinitionNode(loc(lexer, start), name, directives, types)

  fun ref parse_union_members(lexer: GraphQLLexer): Array[NamedTypeNode] ? =>
    """
    UnionMembers :
     - NamedType
     - UnionMembers | NamedType
    """
    let members = Array[NamedTypeNode]
    repeat
      members.push(parse_named_type(lexer))
    until not skip(lexer, PIPE) end
    members

  fun ref parse_enum_type_definition(lexer: GraphQLLexer): EnumTypeDefinitionNode ? =>
    """
    EnumTypeDefinition : enum Name Directives? { EnumValueDefinition+ }
    """
    let start = lexer.token()
    expect_keyword(lexer, "enum")
    let name = parse_name(lexer)
    let directives = parse_directives(lexer)
    let values = many[EnumValueDefinitionNode](
      lexer,
      BraceL,
      {(s: GraphQLParser, l: GraphQLLexer): EnumValueDefinitionNode ? =>
        s.parse_enum_value_definition(l)} ref,
      BraceR
    )
    EnumTypeDefinitionNode(loc(lexer, start), name, directives, values)

  fun ref parse_enum_value_definition(lexer: GraphQLLexer): EnumValueDefinitionNode ? =>
    """
    EnumValueDefinition : EnumValue Directives?

    EnumValue : Name
    """
    let start = lexer.token()
    let name = parse_name(lexer)
    let directives = parse_directives(lexer)
    EnumValueDefinitionNode(loc(lexer, start), name, directives)

  fun ref parse_input_object_type_definition(
    lexer: GraphQLLexer
  ): InputObjectTypeDefinitionNode ? =>
    """
    InputObjectTypeDefinition : input Name Directives? { InputValueDefinition+ }
    """
    let start = lexer.token()
    expect_keyword(lexer, "input")
    let name = parse_name(lexer)
    let directives = parse_directives(lexer)
    let fields = any[InputValueDefinitionNode](
      lexer,
      BraceL,
      {(s: GraphQLParser, l: GraphQLLexer): InputValueDefinitionNode ? =>
        s.parse_input_value_def(l)} ref,
      BraceR
    )
    InputObjectTypeDefinitionNode(loc(lexer, start), name, directives, fields)

  fun ref parse_type_extension_definition(
    lexer: GraphQLLexer
  ): TypeExtensionDefinitionNode ? =>
    """
    TypeExtensionDefinition : extend ObjectTypeDefinition
    """
    let start = lexer.token()
    expect_keyword(lexer, "extend")
    let definition = parse_object_type_definition(lexer)
    TypeExtensionDefinitionNode(loc(lexer, start), definition)

  fun ref parse_directive_definition(lexer: GraphQLLexer): DirectiveDefinitionNode ? =>
    """
    DirectiveDefinition :
      - directive @ Name ArgumentsDefinition? on DirectiveLocations
    """
    let start = lexer.token()
    expect_keyword(lexer, "directive")
    expect(lexer, AT)
    let name = parse_name(lexer)
    let args = parse_argument_defs(lexer)
    expect_keyword(lexer, "on")
    let locations = parse_directive_locations(lexer)
    DirectiveDefinitionNode(loc(lexer, start), name, args, locations)

  fun ref parse_directive_locations(lexer: GraphQLLexer): Array[NameNode] ? =>
    """
    DirectiveLocations :
      - Name
      - DirectiveLocations | Name
    """
    let locations = Array[NameNode]
    repeat
      locations.push(parse_name(lexer))
    until not skip(lexer, PIPE) end
    locations

  // Core parsing utility functions

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

  fun loc(lexer: GraphQLLexer, token: Token): Location =>
    Location(token.line, token.column,
      token, token, Source("TODO", "TODO"))

  fun ref expect(lexer: GraphQLLexer, kind: TokenKind): Token ? =>
    """
    If the next token is of the given kind, return that token after advancing
    the lexer. Otherwise, do not change the parser state and throw an error.
    """
    let token' = lexer.token()
    if token'.kind is kind then
      lexer.advance()
      return token'
    end
    syntax_error(
      "TODO",
      token'.line,
      token'.column,
      "Expected "+ kind.string() +", found "+ token'.kind.string()
    )
    error

  fun ref expect_keyword(lexer: GraphQLLexer, value: String): Token ? =>
    """
    If the next token is a keyword with the given value, return that token after
    advancing the lexer. Otherwise, do not change the parser state and return
    false.
    """
    let token = lexer.token()
    if (token.kind is NAME) and (token.value == value) then
      lexer.advance()
      return token
    end
    let msg = String().append("Expected ")
        .append("\"").append(value).append("\"")
        .append(", found ").append(token.kind.string()).append(" ")
        .append("\"").append(token.value).append("\"")
    syntax_error("TODO", token.line, token.column, msg.clone())
    error

  fun ref unexpected(lexer: GraphQLLexer, atToken: (Token|None) = None) =>
    """
    Helper function for creating an error when an unexpected lexed token
    is encountered.
    """
    let token' = match atToken
    | let t: Token => t
    else lexer.token() end
    let msg = String().append("Unexpected ")
      .append(token'.kind.string()).append(" ")
      .append("\"").append(token'.value).append("\"")
    syntax_error("TODO", token'.line, token'.column, msg.clone())

  fun ref syntax_error(source': String, line: U32, column: U32, message: String) =>
    err = GraphQLError(source', line, column, message)
    None

  fun ref any[T: ASTNode](
    lexer: GraphQLLexer,
    openKind: TokenKind,
    parseFn: {ref(GraphQLParser,GraphQLLexer): T ?},
    closeKind: TokenKind
  ): Array[T] ? =>
    """
    Returns a possibly empty list of parse nodes, determined by
    the parseFn. This list begins with a lex token of openKind
    and ends with a lex token of closeKind. Advances the parser
    to the next lex token after the closing token.
    """
    expect(lexer, openKind)
    let nodes = Array[T]
    while not skip(lexer, closeKind) do
      nodes.push(parseFn(this, lexer))
    end
    nodes

  fun ref many[T: ASTNode](
    lexer: GraphQLLexer,
    openKind: TokenKind,
    parseFn: {ref(GraphQLParser,GraphQLLexer): T ?},
    closeKind: TokenKind
  ): Array[T] ? =>
    """
    Returns a non-empty list of parse nodes, determined by
    the parseFn. This list begins with a lex token of openKind
    and ends with a lex token of closeKind. Advances the parser
    to the next lex token after the closing token.
    """
    expect(lexer, openKind)
    let nodes = Array[T]
    while not skip(lexer, closeKind) do
      nodes.push(parseFn(this, lexer))
    end
    nodes
