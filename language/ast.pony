class Token

class Location
  """
  Contains a range of UTF-8 character offsets and token references that
  identify the region of the source from which the AST derived.
  """

  /**
   * The character offset at which this Node begins.
   */
  let startOff: U32

  /**
   * The character offset at which this Node ends.
   */
  let endOff: U32

  /**
   * The Token at which this Node begins.
   */
  let startToken: Token

  /**
   * The Token at which this Node ends.
   */
  let endToken: Token

  /**
   * The Source document the AST represents.
   */
  let source: Source

  new create(
    startOff': U32, endOff': U32,
    startToken': Token, endToken': Token,
    source': Source) =>
    startOff = startOff'
    endOff = endOff'
    startToken = startToken'
    endToken = endToken'
    source = source'

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
  let loc: (Location|None) = None
  let value: String
  new create(value' : String) =>
    value = value'

// Document
class DocumentNode
  let kind : String = "Document"
  let loc: (Location|None)
  let definitions: Array[DefinitionNode]
  new create(loc' : Location, definitions': Array[DefinitionNode]) =>
    loc = loc'
    definitions = definitions'

type DefinitionNode is
  ( OperationDefinitionNode
  | FragmentDefinitionNode
  | TypeSystemDefinitionNode // experimental non-spec addition.
  )

class OperationDefinitionNode
  let kind: String = "OperationDefinition"
  let loc: (Location|None)
  let operation: OperationTypeNode
  let name: (NameNode|None)
  let variableDefinitions: (Array[VariableDefinitionNode]|None)
  let directives: (Array[DirectiveNode]|None)
  let selectionSet: SelectionSetNode
  new create(loc' : Location,
    operation': OperationTypeNode,
    name': NameNode,
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

// Note: subscription is an experimental non-spec addition.
primitive TnQuery
primitive TnMutation
primitive TnSubscription
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

class VariableNode
  let kind: String = "Variable"
  let loc: (Location|None)
  let name: NameNode
  new create(loc': Location, name': NameNode) =>
    loc = loc'
    name = name'

class SelectionSetNode
  let kind: String = "SelectionSet"
  let loc: (Location|None)
  let selections: Array[SelectionNode]
  new create(loc': Location, selections': Array[SelectionNode]) =>
    loc = loc'
    selections = selections'

type SelectionNode is
  ( FieldNode
  | FragmentSpreadNode
  | InlineFragmentNode
  )

class FieldNode
  let kind: String = "Field"
  let loc: (Location|None)
  let alias: (NameNode|None)
  let name: NameNode
  let arguments: (Array[ArgumentNode]|None) = None
  let directives: (Array[DirectiveNode]|None) = None
  let selectionSet: (SelectionSetNode|None) = None
  new create(loc': Location, alias': NameNode, name': NameNode) =>
    loc = loc'
    alias = alias'
    name = name'

class ArgumentNode
  let kind: String = "Argument"
  let loc: (Location|None)
  let name: NameNode
  let value: ValueNode
  new create(loc': Location, name': NameNode, value': ValueNode) =>
    loc = loc'
    name = name'
    value = value'

// Fragments
class FragmentSpreadNode
  let kind: String = "FragmentSpread"
  let loc: (Location|None)
  let name: NameNode
  let directives: (Array[DirectiveNode]|None) = None
  new create(loc': Location, name': NameNode) =>
    loc = loc'
    name = name'

class InlineFragmentNode
  let kind: String = "InlineFragment"
  let loc: (Location|None)
  let typeCondition: (NamedTypeNode|None) = None
  let directives: (Array[DirectiveNode]|None) = None
  let selectionSet: SelectionSetNode
  new create(loc': Location, selectionSet': SelectionSetNode) =>
    loc = loc'
    selectionSet = selectionSet'

class FragmentDefinitionNode
  let kind: String = "FragmentDefinition"
  let loc: (Location|None)
  let name: NameNode
  let typeCondition: NamedTypeNode
  let directives: (Array[DirectiveNode]|None) = None
  let selectionSet: SelectionSetNode
  new create(loc': Location, name': NameNode,
    typeCondition': NamedTypeNode,
    selectionSet': SelectionSetNode
  ) =>
    loc = loc'
    name = name'
    typeCondition = typeCondition'
    selectionSet = selectionSet'

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

class IntValueNode
  let kind: String = "IntValue"
  let loc: (Location|None)
  let value: String
  new create(loc': Location, value': String) =>
    loc = loc'
    value = value'

class FloatValueNode
  let kind: String = "FloatValue"
  let loc: (Location|None)
  let value: String
  new create(loc': Location, value': String) =>
    loc = loc'
    value = value'

class StringValueNode
  let kind: String = "StringValue"
  let loc: (Location|None)
  let value: String
  new create(loc': Location, value': String) =>
    loc = loc'
    value = value'

class BooleanValueNode
  let kind: String = "BooleanValue"
  let loc: (Location|None)
  let value: Bool
  new create(loc': Location, value': Bool) =>
    loc = loc'
    value = value'

class NullValueNode
  let kind: String = "NullValue"
  let loc: (Location|None)
  new create(loc': Location) =>
    loc = loc'

class EnumValueNode
  let kind: String = "EnumValue"
  let loc: (Location|None)
  let value: String
  new create(loc': Location, value': String) =>
    loc = loc'
    value = value'

class ListValueNode
  let kind: String = "ListValue"
  let loc: (Location|None)
  // let values: Array[ValueNode]
  new create(loc': Location) =>
    loc = loc'

class ObjectValueNode
  let kind: String = "ObjectValue"
  let loc: (Location|None)
  let fields: Array[ObjectFieldNode]
  new create(loc': Location, fields': Array[ObjectFieldNode]) =>
    loc = loc'
    fields = fields'

class ObjectFieldNode
  let kind: String = "ObjectField"
  let loc: (Location|None)
  let name: NameNode
  let value: ValueNode
  new create(loc': Location, name': NameNode, value': ValueNode) =>
    loc = loc'
    name = name'
    value = value'

// Directives
class DirectiveNode
  let kind: String = "Directive"
  let loc: (Location|None)
  let name: NameNode
  let arguments: (Array[ArgumentNode]|None)
  new create(loc': Location, name': NameNode, arguments': Array[ArgumentNode]) =>
    loc = loc'
    name = name'
    arguments = arguments'

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

class ListTypeNode
  let kind: String = "ListType"
  let loc: (Location|None)
  let typeNode: TypeNode
  new create(loc': Location, typeNode': TypeNode) =>
    loc = loc'
    typeNode = typeNode'

class NonNullTypeNode
  let kind: String = "NonNullType"
  let loc: (Location|None)
  let typeNode: (NamedTypeNode | ListTypeNode)
  new create(loc': Location, typeNode': (NamedTypeNode | ListTypeNode)) =>
    loc = loc'
    typeNode = typeNode'

// Type System Definition
type TypeSystemDefinitionNode is
  ( SchemaDefinitionNode
  | TypeDefinitionNode
  | TypeExtensionDefinitionNode
  | DirectiveDefinitionNode
  )

class SchemaDefinitionNode
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

class OperationTypeDefinitionNode
  let kind: String = "OperationTypeDefinition"
  let loc: (Location|None)
  let operation: OperationTypeNode
  let typeNode: NamedTypeNode
  new create(loc': Location,
    operation': OperationTypeNode,
    typeNode': NamedTypeNode
  ) =>
    loc = loc'
    operation = operation'
    typeNode = typeNode'

type TypeDefinitionNode is
  ( ScalarTypeDefinitionNode
  | ObjectTypeDefinitionNode
  | InterfaceTypeDefinitionNode
  | UnionTypeDefinitionNode
  | EnumTypeDefinitionNode
  | InputObjectTypeDefinitionNode
  )

class ScalarTypeDefinitionNode
  let kind: String = "ScalarTypeDefinition"
  let loc: (Location|None)
  let name: NameNode
  let directives: (Array[DirectiveNode]|None) = None
  new create(loc': Location, name': NameNode) =>
    loc = loc'
    name = name'

class ObjectTypeDefinitionNode
  let kind: String = "ObjectTypeDefinition"
  let loc: (Location|None)
  let name: NameNode
  let interfaces: (Array[NamedTypeNode]|None) = None
  let directives: (Array[DirectiveNode]|None) = None
  let fields: Array[FieldDefinitionNode]
  new create(loc': Location, name': NameNode, fields': Array[FieldDefinitionNode]) =>
    loc = loc'
    name = name'
    fields = fields'

class FieldDefinitionNode
  let kind: String = "FieldDefinition"
  let loc: (Location|None)
  let name: NameNode
  let arguments: Array[InputValueDefinitionNode]
  let typeNode: TypeNode
  let directives: (Array[DirectiveNode]|None) = None
  new create(loc': Location,
    name': NameNode,
    arguments': Array[InputValueDefinitionNode],
    typeNode': TypeNode
  ) =>
    loc = loc'
    name = name'
    arguments = arguments'
    typeNode = typeNode'

class InputValueDefinitionNode
  let kind: String = "InputValueDefinition"
  let loc: (Location|None)
  let name: NameNode
  let typeNode: TypeNode
  let defaultValue: (ValueNode|None) = None
  let directives: (Array[DirectiveNode]|None) = None
  new create(loc': Location, name': NameNode, typeNode': TypeNode) =>
    loc = loc'
    name = name'
    typeNode = typeNode'

class InterfaceTypeDefinitionNode
  let kind: String = "InterfaceTypeDefinition"
  let loc: (Location|None)
  let name: NameNode
  let directives: (Array[DirectiveNode]|None) = None
  let fields: Array[FieldDefinitionNode]
  new create(loc': Location, name': NameNode, fields': Array[FieldDefinitionNode]) =>
    loc = loc'
    name = name'
    fields = fields'

class UnionTypeDefinitionNode
  let kind: String = "UnionTypeDefinition"
  let loc: (Location|None)
  let name: NameNode
  let directives: (Array[DirectiveNode]|None) = None
  let types: Array[NamedTypeNode]
  new create(loc': Location, name': NameNode, types': Array[NamedTypeNode]) =>
    loc = loc'
    name = name'
    types = types'

class EnumTypeDefinitionNode
  let kind: String = "EnumTypeDefinition"
  let loc: (Location|None)
  let name: NameNode
  let directives: (Array[DirectiveNode]|None) = None
  let values: Array[EnumValueDefinitionNode]
  new create(loc': Location, name': NameNode, values': Array[EnumValueDefinitionNode]) =>
    loc = loc'
    name = name'
    values = values'

class EnumValueDefinitionNode
  let kind: String = "EnumValueDefinition"
  let loc: (Location|None)
  let name: NameNode
  let directives: (Array[DirectiveNode]|None) = None
  new create(loc': Location, name': NameNode) =>
    loc = loc'
    name = name'

class InputObjectTypeDefinitionNode
  let kind: String = "InputObjectTypeDefinition"
  let loc: (Location|None)
  let name: NameNode
  let directives: (Array[DirectiveNode]|None) = None
  let fields: Array[InputValueDefinitionNode]
  new create(loc': Location, name': NameNode, fields': Array[InputValueDefinitionNode]) =>
    loc = loc'
    name = name'
    fields = fields'

class TypeExtensionDefinitionNode
  let kind: String = "TypeExtensionDefinition"
  let loc: (Location|None)
  let definition: ObjectTypeDefinitionNode
  new create(loc': Location, definition': ObjectTypeDefinitionNode) =>
    loc = loc'
    definition = definition'

class DirectiveDefinitionNode
  let kind: String = "DirectiveDefinition"
  let loc: (Location|None)
  let name: NameNode
  let arguments: (Array[InputValueDefinitionNode]|None) = None
  let locations: Array[NameNode]
  new create(loc': Location, name': NameNode, locations': Array[NameNode]) =>
    loc = loc'
    name = name'
    locations = locations'