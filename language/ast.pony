// Each kind of token.
primitive SOF
  fun string(): String => "SOF"
primitive EOF
  fun string(): String => "EOF"
primitive BANG fun rune(): U32 => '!'
  fun string(): String => "BANG"
primitive DOLLAR fun rune(): U32 => '$'
  fun string(): String => "DOLLAR"
primitive ParenL fun rune(): U32 => '('
  fun string(): String => "ParenL"
primitive ParenR fun rune(): U32 => ')'
  fun string(): String => "ParenR"
primitive SPREAD fun rune(): U32 => '...'
  fun string(): String => "SPREAD"
primitive COLON fun rune(): U32 => ':'
  fun string(): String => "COLON"
primitive EQUALS fun rune(): U32 => '='
  fun string(): String => "EQUALS"
primitive AT fun rune(): U32 => '@'
  fun string(): String => "AT"
primitive BracketL fun rune(): U32 => '['
  fun string(): String => "["
primitive BracketR fun rune(): U32 => ']'
  fun string(): String => "]"
primitive BraceL fun rune(): U32 => '{'
  fun string(): String => "{"
primitive PIPE fun rune(): U32 => '|'
  fun string(): String => "|"
primitive BraceR fun rune(): U32 => '}'
  fun string(): String => "}"
primitive NAME
  fun string(): String => "NAME"
primitive GraphQLInt
  fun string(): String => "GraphQLInt"
primitive GraphQLFloat
  fun string(): String => "GraphQLFloat"
primitive GraphQLString
  fun string(): String => "GraphQLString"
primitive COMMENT
  fun string(): String => "COMMENT"

type TokenKind is (SOF | EOF | BANG | DOLLAR | ParenL | ParenR
  | SPREAD | COLON | EQUALS | AT | BracketL | BracketR
  | BraceL | PIPE | BraceR | NAME | GraphQLInt | GraphQLFloat | GraphQLString
  | COMMENT )

class ArraysHelper
  fun string[T: Stringable #read](a': (Array[T] box|None)): String iso^ =>
    match a'
    | let a: Array[T] box =>
      var sep = ""
      let s = String().append("[")
      for e in a.values() do
        s.append(sep)
        s.append(e.string())
        sep = ","
      end
      s.append("]")
      s.clone()
    else
      "None".clone()
    end
  fun directive_nodes(
    directives: (Array[DirectiveNode]|None)
  ): (Array[ASTNode]|None) =>
    match directives
    | let a': Array[DirectiveNode] =>
      Array[ASTNode].concat(a'.values())
    end
  fun argument_nodes(
    arguments: (Array[ArgumentNode]|None)
  ): (Array[ASTNode]|None) =>
    match arguments
    | let a': Array[ArgumentNode] =>
      Array[ASTNode].concat(a'.values())
    end
  fun named_type_nodes(
    a: (Array[NamedTypeNode]|None)
  ): (Array[ASTNode]|None) =>
    match a
    | let a': Array[NamedTypeNode] =>
      Array[ASTNode].concat(a'.values())
    end

class val Token
  """
  Immutable token lexed from input
  """
  // The kind of Token.
  let kind : TokenKind val
  // For non-punctuation tokens, represents the interpreted value of the token.
  let value : String val
  // The character offset at which this Node begins.
  let startOffset: U32
  // The character offset at which this Node ends.
  let endOffset: U32
  // The 1-indexed line number on which this Token appears.
  let line : U32 val
  // The 1-indexed column number at which this Token begins.
  let column: U32 val

  new val create(
    kind': TokenKind,
    value': String,
    startOffset': U32,
    endOffset': U32,
    line': U32,
    column': U32
  ) =>
    kind = kind'
    value = value'
    startOffset = startOffset'
    endOffset = endOffset'
    line = line'
    column = column'
  fun eq(that: box->Token): Bool =>
    (this.kind is that.kind) and
    (this.value == that.value) and
    (this.startOffset == that.startOffset) and
    (this.endOffset == that.endOffset) and
    (this.line == that.line) and
    (this.column == that.column)

class Location is Equatable[Location]
  """
  Contains a range of UTF-8 character offsets and token references that
  identify the region of the source from which the AST derived.
  """
  /**
   * The character offset at which this Node begins.
   */
  let startOffset: U32
  /**
   * The character offset at which this Node ends.
   */
  let endOffset: U32
  /**
   * The Token at which this Node begins.
   */
  let startToken: (Token|None)
  /**
   * The Token at which this Node ends.
   */
  let endToken: (Token|None)
  /**
   * The Source document the AST represents.
   */
  let source: (Source|None)

  new create(
    startOffset': U32 = 0,
    endOffset': U32 = 0,
    startToken': (Token|None) = None,
    endToken': (Token|None) = None,
    source': (Source|None) = None
  ) =>
    startOffset = match startToken' | let t: Token => t.startOffset else startOffset' end
    endOffset = match endToken' | let t: Token => t.endOffset else endOffset' end
    startToken = startToken'
    endToken = endToken'
    source = source'
  fun string(): String iso^ =>
    recover
      String()
        .append("(").append(startOffset.string())
        .append(",").append(endOffset.string()).append(")")
    end
  fun eq(that: box->Location): Bool =>
    (this.startOffset == that.startOffset)
      and
    (this.endOffset == that.endOffset)
    // TODO eq on source

/**
 * The list of all possible AST node types.
 */
type ASTNode is
  ( NameNode
  | DocumentNode
  | OperationDefinitionNode
  | VariableDefinitionNode
  | VariableNode
  | SelectionSetNode
  | FieldNode
  | ArgumentNode
  | FragmentSpreadNode
  | InlineFragmentNode
  | FragmentDefinitionNode
  | IntValueNode
  | FloatValueNode
  | StringValueNode
  | BooleanValueNode
  | NullValueNode
  | EnumValueNode
  | ListValueNode
  | ObjectValueNode
  | ObjectFieldNode
  | DirectiveNode
  | NamedTypeNode
  | ListTypeNode
  | NonNullTypeNode
  | SchemaDefinitionNode
  | OperationTypeDefinitionNode
  | ScalarTypeDefinitionNode
  | ObjectTypeDefinitionNode
  | FieldDefinitionNode
  | InputValueDefinitionNode
  | InterfaceTypeDefinitionNode
  | UnionTypeDefinitionNode
  | EnumTypeDefinitionNode
  | EnumValueDefinitionNode
  | InputObjectTypeDefinitionNode
  | TypeExtensionDefinitionNode
  | DirectiveDefinitionNode
  )

// Name
class NameNode
  let kind : String = "Name"
  let loc: (Location|None)
  let valueString: String
  new create(loc': Location, value': String) =>
    loc = loc'
    valueString = value'
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    let s = String().append(kind).append("{")
      .append("loc:").append(loc.string()).append(",")
      .append("value:").append("\"").append(valueString)
      .append("}")
    s.clone()

// Document
class DocumentNode is (Equatable[DocumentNode] & Stringable)
  let atos: ArraysHelper = ArraysHelper
  let kind : String = "Document"
  let loc: (Location|None)
  let definitions: Array[DefinitionNode]
  new create(loc': Location, definitions': Array[DefinitionNode]) =>
    loc = loc'
    definitions = definitions'
  fun ref definitionNodes(): Array[ASTNode] =>
    Array[ASTNode].concat(definitions.values())
  // fun ref apply(key: String): Array[ASTNode] ? =>
  //   match key
  //   | "definitions" =>
  //     Array[ASTNode].concat(definitions.values())
  //   else
  //     error
  //   end
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    let s = String().append(kind).append("{\n")
    s.append(" loc:").append(loc.string()).append("\n")
    s.append(" definitions:").append(atos.string[DefinitionNode](definitions)).append("\n")
    s.append("\n}")
    s.clone()

type DefinitionNode is
  ( OperationDefinitionNode
  | FragmentDefinitionNode
  | TypeSystemDefinitionNode // experimental non-spec addition.
  )

class OperationDefinitionNode
  let atos: ArraysHelper = ArraysHelper
  let kind: String = "OperationDefinition"
  let loc: (Location|None)
  let operation: OperationTypeNode
  let name: (NameNode|None)
  let variableDefinitions: (Array[VariableDefinitionNode]|None)
  let directives: (Array[DirectiveNode]|None)
  let selectionSet: SelectionSetNode
  new create(
    loc' : Location,
    operation': OperationTypeNode,
    name': (NameNode|None),
    variableDefinitions': (Array[VariableDefinitionNode]|None),
    directives': (Array[DirectiveNode]|None),
    selectionSet': SelectionSetNode
  ) =>
    loc = loc'
    operation = operation'
    name = name'
    variableDefinitions = variableDefinitions'
    directives = directives'
    selectionSet = selectionSet'
  fun ref variableDefinitionNodes(): (Array[ASTNode]|None) =>
    match variableDefinitions
    | let a': Array[VariableDefinitionNode] =>
      Array[ASTNode].concat(a'.values())
    end
  fun ref directiveNodes(): (Array[ASTNode]|None) =>
    atos.directive_nodes(directives)
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    let s = String().append(kind).append("{\n")
      .append("loc:").append(loc.string()).append("\n")
      .append("operation:").append(operation.string()).append("\n")
      .append("name:").append(name.string()).append("\n")
      .append("variableDefinitions:").append(atos.string[VariableDefinitionNode](variableDefinitions)).append("\n")
      .append("directives:").append(atos.string[DirectiveNode](directives)).append("\n")
      .append("selectionSet:").append(selectionSet.string()).append("\n")
      .append("\n}")
    s.clone()

// Note: subscription is an experimental non-spec addition.
primitive TnQuery
  fun string(): String => "query"
primitive TnMutation
  fun string(): String => "mutation"
primitive TnSubscription
  fun string(): String => "subscript"
type OperationTypeNode is (TnQuery | TnMutation | TnSubscription)

class VariableDefinitionNode
  let kind: String = "VariableDefinition"
  let loc: (Location|None)
  let variable: VariableNode
  let typeNode: TypeNode
  let defaultValue: (ValueNode|None)
  new create(loc': Location, variable': VariableNode,
    typeNode': TypeNode,
    defaultValue': (ValueNode|None)
  ) =>
    loc = loc'
    variable = variable'
    typeNode = typeNode'
    defaultValue = defaultValue'
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()

class VariableNode is (Equatable[ValueNode] & Stringable)
  let kind: String = "Variable"
  let loc: (Location|None)
  let name: NameNode
  new create(loc': Location, name': NameNode) =>
    loc = loc'
    name = name'
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()

class SelectionSetNode is Stringable
  let kind: String = "SelectionSet"
  let loc: (Location|None)
  let selections: Array[SelectionNode]
  new create(loc': Location, selections': Array[SelectionNode]) =>
    loc = loc'
    selections = selections'
  fun ref selectionNodes(): Array[ASTNode] =>
      Array[ASTNode].concat(selections.values())
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    let s = String().append(kind).append(loc.string()).append("[\n")
    for n in selections.values() do
      s.append("  ").append(n.string()).append("\n")
    end
    s.append(" ]")
    s.clone()

type SelectionNode is
  ( FieldNode
  | FragmentSpreadNode
  | InlineFragmentNode
  )

class FieldNode
  let atos: ArraysHelper = ArraysHelper
  let kind: String = "Field"
  let loc: (Location|None)
  let alias: (NameNode|None)
  let name: NameNode
  let arguments: (Array[ArgumentNode]|None)
  let directives: (Array[DirectiveNode]|None)
  let selectionSet: (SelectionSetNode|None)
  new create(
    loc': Location,
    alias': (NameNode|None),
    name': NameNode,
    arguments': (Array[ArgumentNode]|None),
    directives': (Array[DirectiveNode]|None),
    selectionSet': (SelectionSetNode|None)
  ) =>
    loc = loc'
    alias = alias'
    name = name'
    arguments = arguments'
    directives = directives'
    selectionSet = selectionSet'
  fun ref argumentNodes(): (Array[ASTNode]|None) =>
    atos.argument_nodes(arguments)
  fun ref directiveNodes(): (Array[ASTNode]|None) =>
    atos.directive_nodes(directives)
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    let s = String()
    s.append(kind).append("{")
      .append(loc.string())
      .append("alias:").append(alias.string()).append(",")
      .append("name:").append(name.string()).append(",")
      .append("arguments:").append(atos.string[ArgumentNode](arguments)).append(",")
      .append("directives").append(atos.string[DirectiveNode](directives)).append(",")
      .append("selectionSet:").append(selectionSet.string())
      .append("}")
    s.clone()

class ArgumentNode is Stringable
  let kind: String = "Argument"
  let loc: (Location|None)
  let name: NameNode
  let valueNode: ValueNode
  new create(loc': Location, name': NameNode, value': ValueNode) =>
    loc = loc'
    name = name'
    valueNode = value'
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    let s = String().append(kind).append("{\n    ")
      .append("loc:").append(loc.string()).append(",")
      .append("name:").append(name.string()).append(",")
      .append("value:").append(valueNode.string())
      .append("}")
    s.clone()

// Fragments
class FragmentSpreadNode
  let atos: ArraysHelper = ArraysHelper
  let kind: String = "FragmentSpread"
  let loc: (Location|None)
  let name: NameNode
  let directives: (Array[DirectiveNode]|None)
  new create(
    loc': Location,
    name': NameNode,
    directives': (Array[DirectiveNode]|None)
  ) =>
    loc = loc'
    name = name'
    directives = directives'
  fun ref directiveNodes(): (Array[ASTNode]|None) =>
    atos.directive_nodes(directives)
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()

class InlineFragmentNode
  let atos: ArraysHelper = ArraysHelper
  let kind: String = "InlineFragment"
  let loc: (Location|None)
  let typeCondition: (NamedTypeNode|None)
  let directives: (Array[DirectiveNode]|None)
  let selectionSet: SelectionSetNode
  new create(
    loc': Location,
    typeCondition': (NamedTypeNode|None),
    directives': (Array[DirectiveNode]|None),
    selectionSet': SelectionSetNode
  ) =>
    loc = loc'
    typeCondition = typeCondition'
    directives = directives'
    selectionSet = selectionSet'
  fun ref directiveNodes(): (Array[ASTNode]|None) =>
    atos.directive_nodes(directives)
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()

class FragmentDefinitionNode
  let atos: ArraysHelper = ArraysHelper
  let kind: String = "FragmentDefinition"
  let loc: (Location|None)
  let name: NameNode
  let typeCondition: NamedTypeNode
  let directives: (Array[DirectiveNode]|None)
  let selectionSet: SelectionSetNode
  new create(
    loc': Location,
    name': NameNode,
    typeCondition': NamedTypeNode,
    directives': (Array[DirectiveNode]|None),
    selectionSet': SelectionSetNode
  ) =>
    loc = loc'
    name = name'
    typeCondition = typeCondition'
    directives = directives'
    selectionSet = selectionSet'
  fun ref directiveNodes(): (Array[ASTNode]|None) =>
    atos.directive_nodes(directives)
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()

// Values
type ValueNode is
  ( VariableNode
  | IntValueNode
  | FloatValueNode
  | StringValueNode
  | BooleanValueNode
  | NullValueNode
  | EnumValueNode
  | ListValueNode
  | ObjectValueNode
  )

class IntValueNode is (Equatable[ValueNode] & Stringable)
  let kind: String = "IntValue"
  let loc: (Location|None)
  let valueString: String
  new create(loc': Location, value': String) =>
    loc = loc'
    valueString = value'
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    String().append(kind).append("{")
      .append("loc:").append(loc.string()).append(",")
      .append("value:").append(valueString)
      .append("}").clone()

class FloatValueNode is (Equatable[ValueNode] & Stringable)
  let kind: String = "FloatValue"
  let loc: (Location|None)
  let valueString: String
  new create(loc': Location, value': String) =>
    loc = loc'
    valueString = value'
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    String().append(kind).append("{")
      .append("loc:").append(loc.string()).append(",")
      .append("value:").append(valueString)
      .append("}").clone()

class StringValueNode is (Equatable[ValueNode] & Stringable)
  let kind: String = "StringValue"
  let loc: (Location|None)
  let valueString: String
  new create(loc': Location, value': String) =>
    loc = loc'
    valueString = value'
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    String().append(kind).append("{")
      .append("loc:").append(loc.string()).append(",")
      .append("value:").append(valueString)
      .append("}").clone()

class BooleanValueNode is (Equatable[ValueNode] & Stringable)
  let kind: String = "BooleanValue"
  let loc: (Location|None)
  let valueBool: Bool
  new create(loc': Location, value': Bool) =>
    loc = loc'
    valueBool = value'
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    String().append(kind).append("{")
      .append("loc:").append(loc.string()).append(",")
      .append("value:").append(valueBool.string())
      .append("}").clone()

class NullValueNode is (Equatable[ValueNode] & Stringable)
  let kind: String = "NullValue"
  let loc: (Location|None)
  new create(loc': Location) =>
    loc = loc'
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    String().append(kind).append("{").append(loc.string()).append("}").clone()
  fun eq(that': box->ValueNode): Bool =>
    match that'
    | let that: NullValueNode box =>
      (this.kind == that.kind) and
      match (this.loc, that.loc)
      | (None, None) => true
      | (let l: Location box, let r: Location box) => l == r
      else
        false
      end
    else
      false
    end

class EnumValueNode is (Equatable[ValueNode] & Stringable)
  let kind: String = "EnumValue"
  let loc: (Location|None)
  let valueString: String
  new create(loc': Location, value': String) =>
    loc = loc'
    valueString = value'
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    String().append(kind).append("{")
      .append("loc:").append(loc.string()).append(",")
      .append("value:").append(valueString)
      .append("}").clone()

class ListValueNode is (Equatable[ValueNode] & Stringable)
  let atos: ArraysHelper = ArraysHelper
  let kind: String = "ListValue"
  let loc: (Location|None)
  let values: Array[ValueNode]
  new create(loc': Location, values': Array[ValueNode]) =>
    loc = loc'
    values = values'
  fun ref valueNodes(): Array[ASTNode] =>
    Array[ASTNode].concat(values.values())
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    String().append(kind).append("{")
      .append("loc:").append(loc.string()).append(",")
      .append("value:").append(atos.string[ValueNode](values))
      .append("}").clone()

class ObjectValueNode is (Equatable[ValueNode] & Stringable)
  let atos: ArraysHelper = ArraysHelper
  let kind: String = "ObjectValue"
  let loc: (Location|None)
  let fields: Array[ObjectFieldNode]
  new create(loc': Location, fields': Array[ObjectFieldNode]) =>
    loc = loc'
    fields = fields'
  fun ref fieldNodes(): Array[ASTNode] =>
    Array[ASTNode].concat(fields.values())
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    String()
      .append(kind).append("{")
      .append("loc:").append(loc.string()).append(",")
      .append("fields:").append(atos.string[ObjectFieldNode](fields))
      .append("}").clone()

class ObjectFieldNode
  let kind: String = "ObjectField"
  let loc: (Location|None)
  let name: NameNode
  let valueNode: ValueNode
  new create(loc': Location, name': NameNode, value': ValueNode) =>
    loc = loc'
    name = name'
    valueNode = value'
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()

// Directives
class DirectiveNode
  let atos: ArraysHelper = ArraysHelper
  let kind: String = "Directive"
  let loc: (Location|None)
  let name: NameNode
  let arguments: (Array[ArgumentNode]|None)
  new create(loc': Location, name': NameNode, arguments': Array[ArgumentNode]) =>
    loc = loc'
    name = name'
    arguments = arguments'
  fun ref argumentNodes(): (Array[ASTNode]|None) =>
    atos.argument_nodes(arguments)
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()

// Type Reference
type TypeNode is
  ( NamedTypeNode
  | ListTypeNode
  | NonNullTypeNode
  )

class NamedTypeNode
  let kind: String = "NamedType"
  let loc: (Location|None)
  let name: NameNode
  new create(loc': Location, name': NameNode) =>
    loc = loc'
    name = name'
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()

class ListTypeNode
  let kind: String = "ListType"
  let loc: (Location|None)
  let typeNode: TypeNode
  new create(loc': Location, typeNode': TypeNode) =>
    loc = loc'
    typeNode = typeNode'
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()

class NonNullTypeNode
  let kind: String = "NonNullType"
  let loc: (Location|None)
  let typeNode: (NamedTypeNode | ListTypeNode)
  new create(loc': Location, typeNode': (NamedTypeNode | ListTypeNode)) =>
    loc = loc'
    typeNode = typeNode'
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()

// Type System Definition
type TypeSystemDefinitionNode is
  ( SchemaDefinitionNode
  | TypeDefinitionNode
  | TypeExtensionDefinitionNode
  | DirectiveDefinitionNode
  )

class SchemaDefinitionNode
  let atos: ArraysHelper = ArraysHelper
  let kind: String = "SchemaDefinition"
  let loc: (Location|None)
  let directives: Array[DirectiveNode]
  let operationTypes: Array[OperationTypeDefinitionNode]
  new create(loc': Location,
    directives': Array[DirectiveNode],
    operationTypes': Array[OperationTypeDefinitionNode]
  ) =>
    loc = loc'
    directives = directives'
    operationTypes = operationTypes'
  fun ref directiveNodes(): (Array[ASTNode]|None) =>
    atos.directive_nodes(directives)
  fun ref operationTypeNodes(): Array[ASTNode] =>
    Array[ASTNode].concat(operationTypes.values())
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()

class OperationTypeDefinitionNode
  let kind: String = "OperationTypeDefinition"
  let loc: (Location|None)
  let operation: OperationTypeNode
  let typeNode: NamedTypeNode
  new create(
    loc': Location,
    operation': OperationTypeNode,
    typeNode': NamedTypeNode
  ) =>
    loc = loc'
    operation = operation'
    typeNode = typeNode'
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()

type TypeDefinitionNode is
  ( ScalarTypeDefinitionNode
  | ObjectTypeDefinitionNode
  | InterfaceTypeDefinitionNode
  | UnionTypeDefinitionNode
  | EnumTypeDefinitionNode
  | InputObjectTypeDefinitionNode
  )

class ScalarTypeDefinitionNode
  let atos: ArraysHelper = ArraysHelper
  let kind: String = "ScalarTypeDefinition"
  let loc: (Location|None)
  let name: NameNode
  let directives: (Array[DirectiveNode]|None)
  new create(loc': Location, name': NameNode, directives': (Array[DirectiveNode]|None)) =>
    loc = loc'
    name = name'
    directives = directives'
  fun ref directiveNodes(): (Array[ASTNode]|None) =>
    atos.directive_nodes(directives)
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()

class ObjectTypeDefinitionNode
  let atos: ArraysHelper = ArraysHelper
  let kind: String = "ObjectTypeDefinition"
  let loc: (Location|None)
  let name: NameNode
  let interfaces: (Array[NamedTypeNode]|None)
  let directives: (Array[DirectiveNode]|None)
  let fields: Array[FieldDefinitionNode]
  new create(
    loc': Location,
    name': NameNode,
    interfaces': (Array[NamedTypeNode]|None),
    directives': (Array[DirectiveNode]|None),
    fields': Array[FieldDefinitionNode]
  ) =>
    loc = loc'
    name = name'
    interfaces = interfaces'
    directives = directives'
    fields = fields'
  fun ref interfaceNodes(): (Array[ASTNode]|None) =>
    atos.named_type_nodes(interfaces)
  fun ref directiveNodes(): (Array[ASTNode]|None) =>
    atos.directive_nodes(directives)
  fun ref fieldNodes(): (Array[ASTNode]) =>
    Array[ASTNode].concat(fields.values())
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()

class FieldDefinitionNode
  let atos: ArraysHelper = ArraysHelper
  let kind: String = "FieldDefinition"
  let loc: (Location|None)
  let name: NameNode
  let arguments: Array[InputValueDefinitionNode]
  let typeNode: TypeNode
  let directives: (Array[DirectiveNode]|None)
  new create(
    loc': Location,
    name': NameNode,
    arguments': Array[InputValueDefinitionNode],
    typeNode': TypeNode,
    directives': (Array[DirectiveNode]|None)
  ) =>
    loc = loc'
    name = name'
    arguments = arguments'
    typeNode = typeNode'
    directives = directives'
  fun ref argumentNodes(): (Array[ASTNode]|None) =>
    Array[ASTNode].concat(arguments.values())
  fun ref directiveNodes(): (Array[ASTNode]|None) =>
    atos.directive_nodes(directives)
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()

class InputValueDefinitionNode
  let atos: ArraysHelper = ArraysHelper
  let kind: String = "InputValueDefinition"
  let loc: (Location|None)
  let name: NameNode
  let typeNode: TypeNode
  let defaultValue: (ValueNode|None)
  let directives: (Array[DirectiveNode]|None)
  new create(
    loc': Location,
    name': NameNode,
    typeNode': TypeNode,
    defaultValue': (ValueNode|None),
    directives': (Array[DirectiveNode]|None)
  ) =>
    loc = loc'
    name = name'
    typeNode = typeNode'
    defaultValue = defaultValue'
    directives = directives'
  fun ref directiveNodes(): (Array[ASTNode]|None) =>
    atos.directive_nodes(directives)
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()

class InterfaceTypeDefinitionNode
  let atos: ArraysHelper = ArraysHelper
  let kind: String = "InterfaceTypeDefinition"
  let loc: (Location|None)
  let name: NameNode
  let directives: (Array[DirectiveNode]|None)
  let fields: Array[FieldDefinitionNode]
  new create(
    loc': Location,
    name': NameNode,
    directives': (Array[DirectiveNode]|None),
    fields': Array[FieldDefinitionNode]
  ) =>
    loc = loc'
    name = name'
    directives = directives'
    fields = fields'
  fun ref directiveNodes(): (Array[ASTNode]|None) =>
    atos.directive_nodes(directives)
  fun ref fieldNodes(): Array[ASTNode] =>
    Array[ASTNode].concat(fields.values())
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()

class UnionTypeDefinitionNode
  let atos: ArraysHelper = ArraysHelper
  let kind: String = "UnionTypeDefinition"
  let loc: (Location|None)
  let name: NameNode
  let directives: (Array[DirectiveNode]|None)
  let types: Array[NamedTypeNode]
  new create(
    loc': Location,
    name': NameNode,
    directives': (Array[DirectiveNode]|None),
    types': Array[NamedTypeNode]
  ) =>
    loc = loc'
    name = name'
    directives = directives'
    types = types'
  fun ref directiveNodes(): (Array[ASTNode]|None) =>
    atos.directive_nodes(directives)
  fun ref typeNodes(): (Array[ASTNode]|None) =>
    atos.named_type_nodes(types)
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()

class EnumTypeDefinitionNode
  let atos: ArraysHelper = ArraysHelper
  let kind: String = "EnumTypeDefinition"
  let loc: (Location|None)
  let name: NameNode
  let directives: (Array[DirectiveNode]|None)
  let values: Array[EnumValueDefinitionNode]
  new create(
    loc': Location,
    name': NameNode,
    directives': (Array[DirectiveNode]|None),
    values': Array[EnumValueDefinitionNode]
  ) =>
    loc = loc'
    name = name'
    directives = directives'
    values = values'
  fun ref directiveNodes(): (Array[ASTNode]|None) =>
    atos.directive_nodes(directives)
  fun ref valueNodes(): Array[ASTNode] =>
    Array[ASTNode].concat(values.values())
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()

class EnumValueDefinitionNode
  let atos: ArraysHelper = ArraysHelper
  let kind: String = "EnumValueDefinition"
  let loc: (Location|None)
  let name: NameNode
  let directives: (Array[DirectiveNode]|None)
  new create(
    loc': Location,
    name': NameNode,
    directives': (Array[DirectiveNode]|None)
  ) =>
    loc = loc'
    name = name'
    directives = directives'
  fun ref directiveNodes(): (Array[ASTNode]|None) =>
    atos.directive_nodes(directives)
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()

class InputObjectTypeDefinitionNode
  let atos: ArraysHelper = ArraysHelper
  let kind: String = "InputObjectTypeDefinition"
  let loc: (Location|None)
  let name: NameNode
  let directives: (Array[DirectiveNode]|None)
  let fields: Array[InputValueDefinitionNode]
  new create(
    loc': Location,
    name': NameNode,
    directives': (Array[DirectiveNode]|None),
    fields': Array[InputValueDefinitionNode]
  ) =>
    loc = loc'
    name = name'
    directives = directives'
    fields = fields'
  fun ref directiveNodes(): (Array[ASTNode]|None) =>
    atos.directive_nodes(directives)
  fun ref fieldNodes(): Array[ASTNode] =>
    Array[ASTNode].concat(fields.values())
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()

class TypeExtensionDefinitionNode
  let kind: String = "TypeExtensionDefinition"
  let loc: (Location|None)
  let definition: ObjectTypeDefinitionNode
  new create(
    loc': Location,
    definition': ObjectTypeDefinitionNode
  ) =>
    loc = loc'
    definition = definition'
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()

class DirectiveDefinitionNode
  let kind: String = "DirectiveDefinition"
  let loc: (Location|None)
  let name: NameNode
  let arguments: (Array[InputValueDefinitionNode]|None)
  let locations: Array[NameNode]
  new create(
    loc': Location,
    name': NameNode,
    arguments': (Array[InputValueDefinitionNode]|None),
    locations': Array[NameNode]
  ) =>
    loc = loc'
    name = name'
    arguments = arguments'
    locations = locations'
  fun ref argumentNodes(): (Array[ASTNode]|None) =>
    match arguments
    | let a': Array[InputValueDefinitionNode] =>
      Array[ASTNode].concat(a'.values())
    end
  fun ref locationNodes(): Array[ASTNode] =>
    Array[ASTNode].concat(locations.values())
  fun ref update(key: String, value: (ASTNode|Array[ASTNode]|None)) =>
    None
  fun string(): String iso^ =>
    kind.clone()
