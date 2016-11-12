actor Main
  new create(env: Env) =>
    env.out.print("lexer")
    let lexer = GraphQLLexer("this is a test")
    let tokens = lexer.values()
    try
      while tokens.has_next() do
        let token = tokens.next()
        env.out.print("token: " + token.name)
      end
    end

class val GraphQLToken
  """
  Immutable token lexed from input
  """
  let name : String val
  new val create(name' : String) =>
    name = name'

class GraphQLTokens is Iterator[GraphQLToken val]
  let _lexer : GraphQLLexer ref
  var _next : (GraphQLToken|None)

  new create(lexer : GraphQLLexer ref) =>
    _lexer = lexer
    _next = _lexer.nextToken()

  fun has_next(): Bool =>
    if _next is None then
      false
    else
      true
    end

  fun ref next() : GraphQLToken val ? =>
    match _next = _lexer.nextToken()
    | let t : GraphQLToken =>
      t
    else
      error
    end

class GraphQLLexer
  let _input : String
  let _runes : StringRunes
  var _collect : String ref = String()

  new create(input : String) =>
    _input = input
    _runes = input.runes()

  fun ref nextToken() : (GraphQLToken|None) =>
    try
      let rune = _runes.next()
      match rune
      | 32 =>
        let res = GraphQLToken(_collect.clone())
        _collect = String()
        res
      else
        _collect.push_utf32(rune)
        GraphQLToken(_collect.clone())
      end
    else
      None
    end

  fun ref values() : GraphQLTokens => GraphQLTokens(this)
