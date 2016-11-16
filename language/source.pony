class val Source
  let body: String
  let name: String

  new val create(body': String, name': (String|None)) =>
    body = body'
    name = match name'
    | let n : String => n
    else "GraphQL" end
