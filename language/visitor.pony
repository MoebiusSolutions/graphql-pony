use "collections"

primitive BREAK
primitive SKIP
primitive DELETE

type VisitorResponse is (BREAK|SKIP|ASTNode|DELETE|None)

interface Visitor
  fun detailed_enter(
    node: ASTNode,
    key: String,
    parent: ASTNode,
    path: Array[String],
    ancestors: Array[ASTNode box]
  ) =>
    enter(node)
  fun enter(node: ASTNode): VisitorResponse
  fun leave(node: ASTNode): VisitorResponse

class VisitStack
  let inArray: Bool
  let index: USize
  let keys: Array[String]
  let edits: Array[(U32, ASTNode|None)]
  let prev: VisitStack

  new create(
    inArray': Bool,
    index': USize,
    keys': Array[String],
    edits': Array[(U32, ASTNode|None)],
    prev': VisitStack
  ) =>
    inArray = inArray'
    index = index'
    keys = keys'
    edits = edits'
    prev = prev'

class Visit
  fun ref query_document_keys(node: ASTNode box): Array[String] =>
    match node
    | let n: DocumentNode box =>
      [ "definitions" ]
    | let n: OperationDefinitionNode box =>
      [ "name", "variableDefinitions", "directives", "selectionSet" ]
    | let n: VariableDefinitionNode box =>
      [ "variable", "type", "defaultValue" ]
    | let n: VariableNode box =>
      [ "name" ]
    | let n: SelectionSetNode box =>
      [ "selections" ]
    | let n: FieldNode box =>
      [ "alias", "name", "arguments", "directives", "selectionSet" ]
    | let n: ArgumentNode box =>
      [ "name", "value" ]

    | let n: FragmentSpreadNode box =>
      [ "name", "directives" ]
    | let n: InlineFragmentNode box =>
      [ "typeCondition", "directives", "selectionSet" ]
    | let n: FragmentDefinitionNode box =>
      [ "name", "typeCondition", "directives", "selectionSet" ]

    | let n: IntValueNode box => Array[String]
    | let n: FloatValueNode box => Array[String]
    | let n: StringValueNode box => Array[String]
    | let n: BooleanValueNode box => Array[String]
    | let n: NullValueNode box => Array[String]
    | let n: EnumValueNode box => Array[String]
    | let n: ListValueNode box =>
      [ "values" ]
    | let n: ObjectValueNode box =>
      [ "fields" ]
    | let n: ObjectFieldNode box =>
      [ "name", "value" ]

    | let n: DirectiveNode box =>
      [ "name", "arguments" ]

    | let n: NamedTypeNode box =>
      [ "name" ]
    | let n: ListTypeNode box =>
      [ "type" ]
    | let n: NonNullTypeNode box =>
      [ "type" ]

    | let n: SchemaDefinitionNode box =>
      [ "directives", "operationTypes" ]
    | let n: OperationTypeDefinitionNode box =>
      [ "type" ]

    | let n: ScalarTypeDefinitionNode box =>
      [ "name", "directives" ]
    | let n: ObjectTypeDefinitionNode box =>
      [ "name", "interfaces", "directives", "fields" ]
    | let n: FieldDefinitionNode box =>
      [ "name", "arguments", "type", "directives" ]
    | let n: InputValueDefinitionNode box =>
      [ "name", "type", "defaultValue", "directives" ]
    | let n: InterfaceTypeDefinitionNode box =>
      [ "name", "directives", "fields" ]
    | let n: UnionTypeDefinitionNode box =>
      [ "name", "directives", "types" ]
    | let n: EnumTypeDefinitionNode box =>
      [ "name", "directives", "values" ]
    | let n: EnumValueDefinitionNode box =>
      [ "name", "directives" ]
    | let n: InputObjectTypeDefinitionNode box =>
      [ "name", "directives", "fields" ]

    | let n: TypeExtensionDefinitionNode box =>
      [ "definition" ]

    | let n: DirectiveDefinitionNode box =>
      [ "name", "arguments", "locations" ]
    else
      Array[String]
    end

  fun ref query_document_key(
    node: ASTNode box, key: String
  ): (ASTNode box|Array[ASTNode box]|None) =>
    match node
    | let n: DocumentNode box =>
      match key
      | "definitions" => n.definitionNodes()
      end
    | let n: OperationDefinitionNode box =>
      match key
      | "name" => n.name
      | "variableDefinitions" => n.variableDefinitionNodes()
      | "directives" => n.directiveNodes()
      | "selectionSet" => n.selectionSet
      end
    | let n: VariableDefinitionNode box =>
      match key
      | "variable" => n.variable
      | "type" => n.typeNode
      | "defaultValue" => n.defaultValue
      end
    | let n: VariableNode box =>
      match key
      | "name" => n.name
      end
    | let n: SelectionSetNode box =>
      match key
      | "selections" => n.selectionNodes()
      end
    | let n: FieldNode box =>
      match key
      | "alias" => n.alias
      | "name" => n.name
      | "arguments" => n.argumentNodes()
      | "directives" => n.directiveNodes()
      | "selectionSet" => n.selectionSet
      end
    | let n: ArgumentNode box =>
      match key
      | "name" => n.name
      | "value" => n.value
      end

    | let n: FragmentSpreadNode box =>
      match key
      | "name" => n.name
      | "directives" => n.directiveNodes()
      end
    | let n: InlineFragmentNode box =>
      match key
      | "typeCondition" => n.typeCondition
      | "directives" => n.directiveNodes()
      | "selectionSet" => n.selectionSet
      end
    | let n: FragmentDefinitionNode box =>
      match key
      | "name" => n.name
      | "typeCondition" => n.typeCondition
      | "directives" => n.directiveNodes()
      | "selectionSet" => n.selectionSet
      end

    | let n: IntValueNode box => None
    | let n: FloatValueNode box => None
    | let n: StringValueNode box => None
    | let n: BooleanValueNode box => None
    | let n: NullValueNode box => None
    | let n: EnumValueNode box => None
    | let n: ListValueNode box =>
      match key
      | "values" => n.valueNodes()
      end
    | let n: ObjectValueNode box =>
      match key
      | "fields" => n.fieldNodes()
      end
    | let n: ObjectFieldNode box =>
      match key
      | "name" => n.name
      | "value" => n.value
      end

    | let n: DirectiveNode box =>
      match key
      | "name" => n.name
      | "arguments" => n.argumentNodes()
      end

    | let n: NamedTypeNode box =>
      match key
      | "name" => n.name
      end
    | let n: ListTypeNode box =>
      match key
      | "type" => n.typeNode
      end
    | let n: NonNullTypeNode box =>
      match key
      | "type" => n.typeNode
      end

    | let n: SchemaDefinitionNode box =>
      match key
      | "directives" => n.directiveNodes()
      | "operationTypes" => n.operationTypeNodes()
      end
    | let n: OperationTypeDefinitionNode box =>
      match key
      | "type" => n.typeNode
      end

    | let n: ScalarTypeDefinitionNode box =>
      match key
      | "name" => n.name
      | "directives" => n.directiveNodes()
      end
    | let n: ObjectTypeDefinitionNode box =>
      match key
      | "name" => n.name
      | "interfaces" => n.interfaceNodes()
      | "directives" => n.directiveNodes()
      | "fields" => n.fieldNodes()
      end
    | let n: FieldDefinitionNode box =>
      match key
      | "name" => n.name
      | "arguments" => n.argumentNodes()
      | "type" => n.typeNode
      | "directives" => n.directiveNodes()
      end
    | let n: InputValueDefinitionNode box =>
      match key
      | "name" => n.name
      | "type" => n.typeNode
      | "defaultValue" => n.defaultValue
      | "directives" => n.directiveNodes()
      end
    | let n: InterfaceTypeDefinitionNode box =>
      match key
      | "name" => n.name
      | "directives" => n.directiveNodes()
      | "fields" => n.fieldNodes()
      end
    | let n: UnionTypeDefinitionNode box =>
      match key
      | "name" => n.name
      | "directives" => n.directiveNodes()
      | "types" => n.typeNodes()
      end
    | let n: EnumTypeDefinitionNode box =>
      match key
      | "name" => n.name
      | "directives" => n.directiveNodes()
      | "values" => n.valueNodes()
      end
    | let n: EnumValueDefinitionNode box =>
      match key
      | "name" => n.name
      | "directives" => n.directiveNodes()
      end
    | let n: InputObjectTypeDefinitionNode box =>
      match key
      | "name" => n.name
      | "directives" => n.directiveNodes()
      | "fields" => n.fieldNodes()
      end

    | let n: TypeExtensionDefinitionNode box =>
      match key
      | "definition" => n.definition
      end

    | let n: DirectiveDefinitionNode box =>
      match key
      | "name" => n.name
      | "arguments" => n.argumentNodes()
      | "locations" => n.locationNodes()
      end
    end

  fun ref visit(root: ASTNode, visitor: Visitor): ASTNode ? =>
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
    var stack: (VisitStack|None) = None
    var inArray = false // match root else false end
    var keys: Array[String] = [ "root" ]
    var index: USize = -1
    var edits = Array[(U32, (ASTNode|None))]
    var parent: (ASTNode box|Array[ASTNode box]|None) = None
    let path = Array[String]
    let ancestors = Array[ASTNode box]
    var newRoot = root

    repeat
      index = index + 1
      let isLeaving: Bool = index == keys.size()
      var key: (String|None) = None
      var node: (ASTNode box|Array[ASTNode box]|None) = None
      let isEdited: Bool = isLeaving and (edits.size() != 0)
      if isLeaving then
        key = if ancestors.size() == 0 then None else path.pop() end
        node = parent
        parent = ancestors.pop()
        if isEdited then
          match node
          | let node': Array[ASTNode box] =>
            // node = node'.slice()
            node
          | let node': ASTNode box =>
            // TODO
            // node = node.clone()
            node
          end
          var editOffset: U32 = 0
          for ii in Range(0, edits.size()) do
            var editKey: U32 = edits(ii)._1
            let editValue = edits(ii)._2
            if inArray then
              editKey = editKey - editOffset
            end
            match (node, editValue)
            | (let node': Array[ASTNode box], None) =>
              // TODO
              // node'.splice(editKey, 1)
              editOffset = editOffset + 1
            | (let node': Array[ASTNode box], _) =>
              // TODO
              // node'(editKey) = editValue
              node
            end
          end
        end
        match stack
        | let stack': VisitStack =>
          index = stack'.index
          keys = stack'.keys
          edits = stack'.edits
          inArray = stack'.inArray
          stack = stack'.prev
        end
      else
        key = match (parent, inArray)
        | (None, _) => None
        | (_, not inArray) => keys(index)
        end
        node = match (parent, key)
        | (let parent': ASTNode box, let key': String) =>
          query_document_key(parent', key')
        | (let parent': Array[ASTNode box], _) =>
          parent'(index)
        else
          newRoot
        end
        if node is None then
          continue
        end
        match key
        | let key': String => path.push(key')
        end
      end

      // match node
      // | ASTNode =>
      //   None
      // | Array[ASTNode box] =>
      //   None
      // | None =>
      //   error
      // end
    until stack is None end

    if edits.size() != 0 then
      match edits(edits.size() - 1)._2
      | let e': ASTNode => newRoot = e'
      else
        error
      end
    end
    newRoot
