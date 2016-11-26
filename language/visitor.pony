use "collections"

primitive BREAK
primitive SKIP
primitive DELETE

type VisitorResponse is (BREAK|SKIP|ASTNode|DELETE|None)

interface Visitor
  fun detailed_enter(
    node: ASTNode,
    key: (String|None),
    parent: (ASTNode|Array[ASTNode]|None),
    path: Array[String],
    ancestors: Array[(ASTNode|Array[ASTNode]|None)]
  ): VisitorResponse =>
    enter(node)
  fun enter(node: ASTNode): VisitorResponse
  fun leave(node: ASTNode): VisitorResponse

type EditEntry is ((USize|String|None), ASTNode|Array[ASTNode]|None|DELETE)

class VisitStack
  let inArray: Bool
  let index: USize
  let keys: Array[String]
  let edits: Array[EditEntry]
  let prev: (VisitStack|None)

  new create(
    inArray': Bool,
    index': USize,
    keys': Array[String],
    edits': Array[EditEntry],
    prev': (VisitStack|None)
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
    node: ASTNode, key: String
  ): (ASTNode|Array[ASTNode]|None) =>
    match node
    | let n: DocumentNode =>
      match key
      | "definitions" => n.definitionNodes()
      end
    | let n: OperationDefinitionNode =>
      match key
      | "name" => n.name
      | "variableDefinitions" => n.variableDefinitionNodes()
      | "directives" => n.directiveNodes()
      | "selectionSet" => n.selectionSet
      end
    | let n: VariableDefinitionNode =>
      match key
      | "variable" => n.variable
      | "type" => n.typeNode
      | "defaultValue" => n.defaultValue
      end
    | let n: VariableNode =>
      match key
      | "name" => n.name
      end
    | let n: SelectionSetNode =>
      match key
      | "selections" => n.selectionNodes()
      end
    | let n: FieldNode =>
      match key
      | "alias" => n.alias
      | "name" => n.name
      | "arguments" => n.argumentNodes()
      | "directives" => n.directiveNodes()
      | "selectionSet" => n.selectionSet
      end
    | let n: ArgumentNode =>
      match key
      | "name" => n.name
      | "value" => n.value
      end

    | let n: FragmentSpreadNode =>
      match key
      | "name" => n.name
      | "directives" => n.directiveNodes()
      end
    | let n: InlineFragmentNode =>
      match key
      | "typeCondition" => n.typeCondition
      | "directives" => n.directiveNodes()
      | "selectionSet" => n.selectionSet
      end
    | let n: FragmentDefinitionNode =>
      match key
      | "name" => n.name
      | "typeCondition" => n.typeCondition
      | "directives" => n.directiveNodes()
      | "selectionSet" => n.selectionSet
      end

    | let n: IntValueNode => None
    | let n: FloatValueNode => None
    | let n: StringValueNode => None
    | let n: BooleanValueNode => None
    | let n: NullValueNode => None
    | let n: EnumValueNode => None
    | let n: ListValueNode =>
      match key
      | "values" => n.valueNodes()
      end
    | let n: ObjectValueNode =>
      match key
      | "fields" => n.fieldNodes()
      end
    | let n: ObjectFieldNode =>
      match key
      | "name" => n.name
      | "value" => n.value
      end

    | let n: DirectiveNode =>
      match key
      | "name" => n.name
      | "arguments" => n.argumentNodes()
      end

    | let n: NamedTypeNode =>
      match key
      | "name" => n.name
      end
    | let n: ListTypeNode =>
      match key
      | "type" => n.typeNode
      end
    | let n: NonNullTypeNode =>
      match key
      | "type" => n.typeNode
      end

    | let n: SchemaDefinitionNode =>
      match key
      | "directives" => n.directiveNodes()
      | "operationTypes" => n.operationTypeNodes()
      end
    | let n: OperationTypeDefinitionNode =>
      match key
      | "type" => n.typeNode
      end

    | let n: ScalarTypeDefinitionNode =>
      match key
      | "name" => n.name
      | "directives" => n.directiveNodes()
      end
    | let n: ObjectTypeDefinitionNode =>
      match key
      | "name" => n.name
      | "interfaces" => n.interfaceNodes()
      | "directives" => n.directiveNodes()
      | "fields" => n.fieldNodes()
      end
    | let n: FieldDefinitionNode =>
      match key
      | "name" => n.name
      | "arguments" => n.argumentNodes()
      | "type" => n.typeNode
      | "directives" => n.directiveNodes()
      end
    | let n: InputValueDefinitionNode =>
      match key
      | "name" => n.name
      | "type" => n.typeNode
      | "defaultValue" => n.defaultValue
      | "directives" => n.directiveNodes()
      end
    | let n: InterfaceTypeDefinitionNode =>
      match key
      | "name" => n.name
      | "directives" => n.directiveNodes()
      | "fields" => n.fieldNodes()
      end
    | let n: UnionTypeDefinitionNode =>
      match key
      | "name" => n.name
      | "directives" => n.directiveNodes()
      | "types" => n.typeNodes()
      end
    | let n: EnumTypeDefinitionNode =>
      match key
      | "name" => n.name
      | "directives" => n.directiveNodes()
      | "values" => n.valueNodes()
      end
    | let n: EnumValueDefinitionNode =>
      match key
      | "name" => n.name
      | "directives" => n.directiveNodes()
      end
    | let n: InputObjectTypeDefinitionNode =>
      match key
      | "name" => n.name
      | "directives" => n.directiveNodes()
      | "fields" => n.fieldNodes()
      end

    | let n: TypeExtensionDefinitionNode =>
      match key
      | "definition" => n.definition
      end

    | let n: DirectiveDefinitionNode =>
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
    var edits = Array[EditEntry]
    var parent: (ASTNode|Array[ASTNode]|None) = None
    let path = Array[String]
    let ancestors = Array[(ASTNode|Array[ASTNode]|None)]
    var newRoot = root

    repeat
      index = index + 1
      let isLeaving: Bool = index == keys.size()
      var key: (String|None) = None
      var node: (ASTNode|Array[ASTNode]|None) = None
      let isEdited: Bool = isLeaving and (edits.size() != 0)
      if isLeaving then
        key = if ancestors.size() == 0 then None else path.pop() end
        node = parent
        parent = ancestors.pop()
        if isEdited then
          match node
          | let node': Array[ASTNode] =>
            // node = node'.slice()
            node
          | let node': ASTNode =>
            // TODO
            // node = node.clone()
            node
          end
          var editOffset: USize = 0
          for ii in Range(0, edits.size()) do
            var editKey: (USize|String|None) = edits(ii)._1
            let editValue = edits(ii)._2
            match editKey
            | let editKey': USize =>
              editKey = editKey' - editOffset
            end
            match (node, editKey, editValue)
            | (let node': Array[ASTNode], let editKey': USize, DELETE) =>
              // node'.splice(editKey', 1)
              editOffset = editOffset + 1
            | (let node': Array[ASTNode], let editKey': USize, let value': ASTNode) =>
              // node'(editKey') = value'
              // TODO
              None
            | (let node': ASTNode, let editKey': String, let value': ASTNode) =>
              // node'(editKey') = value'
              // TODO
              None
            else
              error
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
        | (let parent': ASTNode, let key': String) =>
          query_document_key(parent', key')
        | (let parent': Array[ASTNode], _) =>
          parent'(index)
          None
        else
          newRoot
        end
        if node is None then
          continue
        end
        match key
        | let key': String =>
          path.push(key')
        end

        match node
        | let node': ASTNode =>
          let result = visitor.detailed_enter(node', key, parent, path, ancestors)
          match result
          | BREAK =>
            break
          | SKIP =>
            if not isLeaving then
              path.pop()
              continue
            end
          | DELETE =>
            edits.push((key, DELETE))
            if not isLeaving then
              path.pop()
              continue
            end
          | let value': ASTNode =>
            edits.push((key, value'))
            if not isLeaving then
              node = value'
            end
          | None =>
            if isEdited then
              edits.push((key, node))
            end
          end
        end
      end

      if not isLeaving then
        stack = VisitStack(inArray, index, keys, edits, stack)
        inArray = match node
        | let node': Array[ASTNode] =>
          true
        else
          false
        end
        keys = match node
        | let node': ASTNode => query_document_keys(node')
        | let node': Array[ASTNode] => keys
        else Array[String] end

        index = -1
        edits = Array[EditEntry]
        match parent
        | None => None
        else
          ancestors.push(parent)
        end
        parent = node
      end
    until stack is None end

    if edits.size() != 0 then
      match edits(edits.size() - 1)._2
      | let e': ASTNode => newRoot = e'
      else
        error
      end
    end
    newRoot
