primitive BREAK
primitive SKIP
primitive DELETE

type VisitorResponse is (BREAK|SKIP|ASTNode|DELETE|None)

interface Visitor
  fun enter(node: ASTNode): VisitorResponse
  fun leave(node: ASTNode): VisitorResponse

class VisitStack
  let inArray: Bool
  let index: U32
  let keys: Array[ASTNode]
  let edits: Array[(U32, ASTNode)]
  let prev: VisitStack

  new create(
    inArray': Bool,
    index': U32,
    keys': Array[ASTNode],
    edits': Array[(U32, ASTNode)],
    prev': VisitStack
  ) =>
    inArray = inArray'
    index = index'
    keys = keys'
    edits = edits'
    prev = prev'

class Visit
  fun ref query_document_keys(node: ASTNode, key: (String|None) = None):
    (ASTNode
    |Array[DefinitionNode]
    |Array[VariableDefinitionNode]
    |Array[DirectiveNode]
    |Array[SelectionNode]
    |Array[OperationTypeDefinitionNode]
    |Array[ArgumentNode]
    |Array[ValueNode]
    |Array[ObjectFieldNode]
    |Array[NamedTypeNode]
    |Array[FieldDefinitionNode]
    |Array[InputValueDefinitionNode]
    |Array[EnumValueDefinitionNode]
    |Array[NameNode]
    |Array[String]
    |None
    )
  =>
    match node
    | let n: DocumentNode =>
      match key
      | "definitions" => n.definitions
      | None => [ "definitions" ]
      end
    | let n: OperationDefinitionNode =>
      match key
      | "name" => n.name
      | "variableDefinitions" => n.variableDefinitions
      | "directives" => n.directives
      | "selectionSet" => n.selectionSet
      | None =>
        [ "name", "variableDefinitions", "directives", "selectionSet" ]
      end
    | let n: VariableDefinitionNode =>
      match key
      | "variable" => n.variable
      | "type" => n.typeNode
      | "defaultValue" => n.defaultValue
      | None => [ "variable", "type", "defaultValue" ]
      end
    | let n: VariableNode =>
      match key
      | "name" => n.name
      | None => [ "name" ]
      end
    | let n: SelectionSetNode =>
      match key
      | "selections" => n.selections
      | None => [ "selections" ]
      end
    | let n: FieldNode =>
      match key
      | "alias" => n.alias
      | "name" => n.name
      | "arguments" => n.arguments
      | "directives" => n.directives
      | "selectionSet" => n.selectionSet
      | None => [ "alias", "name", "arguments", "directives", "selectionSet" ]
      end
    | let n: ArgumentNode =>
      match key
      | "name" => n.name
      | "value" => n.value
      | None => [ "name", "value" ]
      end

    | let n: FragmentSpreadNode =>
      match key
      | "name" => n.name
      | "directives" => n.directives
      | None => [ "name", "directives" ]
      end
    | let n: InlineFragmentNode =>
      match key
      | "typeCondition" => n.typeCondition
      | "directives" => n.directives
      | "selectionSet" => n.selectionSet
      | None => [ "typeCondition", "directives", "selectionSet" ]
      end
    | let n: FragmentDefinitionNode =>
      match key
      | "name" => n.name
      | "typeCondition" => n.typeCondition
      | "directives" => n.directives
      | "selectionSet" => n.selectionSet
      | None => [ "name", "typeCondition", "directives", "selectionSet" ]
      end

    | let n: IntValueNode => Array[String]
    | let n: FloatValueNode => Array[String]
    | let n: StringValueNode => Array[String]
    | let n: BooleanValueNode => Array[String]
    | let n: NullValueNode => Array[String]
    | let n: EnumValueNode => Array[String]
    | let n: ListValueNode =>
      match key
      | "values" => n.values
      | None => [ "values" ]
      end
    | let n: ObjectValueNode =>
      match key
      | "fields" => n.fields
      | None => [ "fields" ]
      end
    | let n: ObjectFieldNode =>
      match key
      | "name" => n.name
      | "value" => n.value
      | None => [ "name", "value" ]
      end

    | let n: DirectiveNode =>
      match key
      | "name" => n.name
      | "arguments" => n.arguments
      | None => [ "name", "arguments" ]
      end

    | let n: NamedTypeNode =>
      match key
      | "name" => n.name
      | None => [ "name" ]
      end
    | let n: ListTypeNode =>
      match key
      | "type" => n.typeNode
      | None => [ "type" ]
      end
    | let n: NonNullTypeNode =>
      match key
      | "type" => n.typeNode
      | None => [ "type" ]
      end

    | let n: SchemaDefinitionNode =>
      match key
      | "directives" => n.directives
      | "operationTypes" => n.operationTypes
      | None => [ "directives", "operationTypes" ]
      end
    | let n: OperationTypeDefinitionNode =>
      match key
      | "type" => n.typeNode
      | None => [ "type" ]
      end

    | let n: ScalarTypeDefinitionNode =>
      match key
      | "name" => n.name
      | "directives" => n.directives
      | None => [ "name", "directives" ]
      end
    | let n: ObjectTypeDefinitionNode =>
      match key
      | "name" => n.name
      | "interfaces" => n.interfaces
      | "directives" => n.directives
      | "fields" => n.fields
      | None => [ "name", "interfaces", "directives", "fields" ]
      end
    | let n: FieldDefinitionNode =>
      match key
      | "name" => n.name
      | "arguments" => n.arguments
      | "type" => n.typeNode
      | "directives" => n.directives
      | None => [ "name", "arguments", "type", "directives" ]
      end
    | let n: InputValueDefinitionNode =>
      match key
      | "name" => n.name
      | "type" => n.typeNode
      | "defaultValue" => n.defaultValue
      | "directives" => n.directives
      | None => [ "name", "type", "defaultValue", "directives" ]
      end
    | let n: InterfaceTypeDefinitionNode =>
      match key
      | "name" => n.name
      | "directives" => n.directives
      | "fields" => n.fields
      | None => [ "name", "directives", "fields" ]
      end
    | let n: UnionTypeDefinitionNode =>
      match key
      | "name" => n.name
      | "directives" => n.directives
      | "types" => n.types
      | None => [ "name", "directives", "types" ]
      end
    | let n: EnumTypeDefinitionNode =>
      match key
      | "name" => n.name
      | "directives" => n.directives
      | "values" => n.values
      | None => [ "name", "directives", "values" ]
      end
    | let n: EnumValueDefinitionNode =>
      match key
      | "name" => n.name
      | "directives" => n.directives
      | None => [ "name", "directives" ]
      end
    | let n: InputObjectTypeDefinitionNode =>
      match key
      | "name" => n.name
      | "directives" => n.directives
      | "fields" => n.fields
      | None => [ "name", "directives", "fields" ]
      end

    | let n: TypeExtensionDefinitionNode =>
      match key
      | "definition" => n.definition
      | None => [ "definition" ]
      end

    | let n: DirectiveDefinitionNode =>
      match key
      | "name" => n.name
      | "arguments" => n.arguments
      | "locations" => n.locations
      | None => [ "name", "arguments", "locations" ]
      end
    end

  fun ref tt()=>
    None

/*
  fun ref visit(root: ASTNode, visitor: Visitor): ASTNode =>
    """
    visit() will walk through an AST using a depth first traversal, calling
    the visitor's enter function at each node in the traversal, and calling the
    leave function after visiting that node and all of its child nodes.

    By returning different values from the enter and leave functions, the
    behavior of the visitor can be altered, including skipping over a sub-tree of
    the AST (by returning SKIP), editing the AST by returning a value or DELETE
    to remove the value, or to stop the whole traversal by returning BREAK.

    When using visit() to edit an AST, the original AST will not be modified, and
    a new version of the AST with the changes applied will be returned from the
    visit function.

        const editedAST = visit(ast, {
          enter(node, key, parent, path, ancestors) {
            // @return
            //   None: no action
            //   SKIP: skip visiting this node
            //   BREAK: stop visiting altogether
            //   DELETE: delete this node
            //   any value: replace this node with the returned value
          },
          leave(node, key, parent, path, ancestors) {
            // @return
            //   None: no action
            //   BREAK: stop visiting altogether
            //   DELETE: delete this node
            //   any value: replace this node with the returned value
          }
        });

    """
    let stack: (VisitStack|None) = None
    let inArray = match root else false end
    let keys: Array[ASTNode] = [ root ]
    let index = -1
    var edits = Array[(U32, ASTNode)] = Array[(U32, ASTNode)]
    var parent: (ASTNode|None) = None
    let path = []
    let ancestors = []
    var newRoot = root

    repeat
      index = index + 1
      let isLeaving = index == keys.size()
      var key: (U32|None)
      var node: ASTNode
      let isEditing = isLeaving and edits.size() != 0
      if isLeaving then
        key = if ancestors.size() == 0 then None else path.pop()
        node = parent
        parent = ancestors.pop()
      else

      end
    until true end
    root
*/
