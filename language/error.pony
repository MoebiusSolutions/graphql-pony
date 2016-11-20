/**
 * A GraphQLError describes an Error found during the parse, validate, or
 * execute phases of performing a GraphQL operation. In addition to a message
 * and stack trace, it also includes information about the locations in a
 * GraphQL document and/or execution result that correspond to the Error.
 */
class GraphQLError
  /**
   * A message describing the Error for debugging purposes.
   *
   * Enumerable, and appears in the result of JSON.stringify().
   */
  let message: String

  /**
   * An array of { line, column } locations within the source GraphQL document
   * which correspond to this error.
   *
   * Errors during validation often contain multiple locations, for example to
   * point out two things with the same name. Errors during execution include a
   * single location, the field which produced the error.
   *
   * Enumerable, and appears in the result of JSON.stringify().
   */
  let locations: Array[(U32, U32)] = Array[(U32, U32)]

  /**
   * An array describing the JSON-path into the execution response which
   * corresponds to this error. Only included for errors during execution.
   *
   * Enumerable, and appears in the result of JSON.stringify().
   */
  // let path: Array[(U32, U32)]

  /**
   * An array of GraphQL AST Nodes corresponding to this error.
   */
  // let nodes: Array[ASTNode]

  /**
   * The source GraphQL document corresponding to this error.
   */
  // let source: Source
  let source: String

  /**
   * An array of character offsets within the source GraphQL document
   * which correspond to this error.
   */
  // let positions: Array[U32]

  new create(source': String, line: U32, message': String) =>
    source = source'
    locations.push((line, 1))
    message = message'

  fun string(): String =>
    let loc = try
      locations(0)
    else
      (0,0)
    end
    "Syntax Error GraphQL ("
      + (1+loc._1).string() +":"+ (1+loc._2).string()
    +") " + message
