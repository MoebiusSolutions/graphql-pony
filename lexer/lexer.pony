actor Main
  new create(env: Env) =>
    env.out.print("lexer")
    let lexer = GraphQLLexer(env,
      """
      this is
      a test! with$ pleft( pright)
      :=[]{|}
      aName_with_123_numbers
      { ...abc }
      test # This is a comment
      "Try a quoted string"
      number: 0
      number: 12
      number: 12.1
      number: 12.2e1
      done
      """
    )
    let tokens = lexer.values()
    try
      while tokens.has_next() do
        let token = tokens.next()
        env.out.print("token: "
          + token.token.string() + " "
          + token.name + " "
          + token.line.string())
      end
    end

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
  fun string(): String => "BracketL"
primitive BracketR fun rune(): U32 => ']'
  fun string(): String => "BracketR"
primitive BraceL fun rune(): U32 => '{'
  fun string(): String => "BraceL"
primitive PIPE fun rune(): U32 => '|'
  fun string(): String => "PIPE"
primitive BraceR fun rune(): U32 => '}'
  fun string(): String => "BraceR"
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

type TokenType is (SOF | EOF | BANG | DOLLAR | ParenL | ParenR
  | SPREAD | COLON | EQUALS | AT | BracketL | BracketR
  | BraceL | PIPE | BraceR | NAME | GraphQLInt | GraphQLFloat | GraphQLString
  | COMMENT )

class val GraphQLToken
  """
  Immutable token lexed from input
  """
  let name : String val
  let token : TokenType val
  let line : U32 val

  new val create(name' : String, token' : TokenType, line' : U32) =>
    name = name'
    token = token'
    line = line'

class GraphQLTokens is Iterator[GraphQLToken val]
  let _lexer : GraphQLLexer ref
  var _next : (GraphQLToken|None)

  new create(lexer : GraphQLLexer ref) =>
    _lexer = lexer
    _next = _lexer.next_token()

  fun has_next(): Bool =>
    if _next is None then
      false
    else
      true
    end

  fun ref next() : GraphQLToken val ? =>
    match _next = _lexer.next_token()
    | let t : GraphQLToken =>
      t
    else
      error
    end

class GraphQLLexer
  let env : Env
  let _input : String
  let _runes : StringRunes
  var _collect : String ref = String()
  var _line : U32 = 1
  var _nextRune : (U32|None) = None

  new create(env' : Env, input : String) =>
    env = env'
    _input = input
    _runes = input.runes()

  fun ref has_next_rune() : Bool =>
    match _nextRune
    | None => _runes.has_next()
    else
      true
    end

  fun ref push_back(rune : U32) =>
    """
    Push rune back into the input stream so that
    it is the next rune returned
    """
    _nextRune = rune
    if rune == '\n' then
      _line = _line - 1
    end

  fun ref next_rune() : U32 ? =>
    var rune : U32
    match _nextRune
    | let r : U32 =>
      _nextRune = None
      rune = r
    else
      rune = _runes.next()
    end
    if rune == '\n' then
      _line = _line + 1
    end
    rune

  fun ref _read_name(rune : U32) : GraphQLToken ? =>
    """
    Reads an alphanumeric + underscore name from the source.

    [_A-Za-z][_0-9A-Za-z]*
    """
    let name = String()
    name.push_utf32(rune)
    while has_next_rune() do
      let r = next_rune()
      if   ((r >= 'A') and (r <= 'Z'))
        or ((r >= 'a') and (r <= 'z'))
        or ((r >= '0') and (r <= '9'))
        or (r == '_')
      then
        name.push_utf32(r)
      else
        push_back(r)
        break
      end
    end
    GraphQLToken(name.clone(), NAME, _line)

  fun ref _read_comment() : GraphQLToken ? =>
    """
    Read comment - from # until the end of the line
    """
    let comment = String()
    while has_next_rune() do
      let r = next_rune()
      if r == '\n' then
        break
      else
        comment.push_utf32(r)
      end
    end
    GraphQLToken(comment.clone(), COMMENT, _line)

  fun ref _read_spread() : GraphQLToken ? =>
    let dot2 = next_rune()
    if dot2 != '.' then
      error
    end
    let dot3 = next_rune()
    if dot3 != '.' then
      error
    end
    GraphQLToken("...", SPREAD, _line)

  fun ref _read_digits(number : String ref) ? =>
    while has_next_rune() do
      let r = next_rune()
      if (r >= '0') and (r <= '9') then
        number.push_utf32(r)
      else
        push_back(r)
        break
      end
    end

  fun ref _read_number(rune : U32) : GraphQLToken ? =>
    """
    Reads a number token from the source file, either a float
    or an int depending on whether a decimal point appears.

    Int:   -?(0|[1-9][0-9]*)
    Float: -?(0|[1-9][0-9]*)(\.[0-9]+)?((E|e)(+|-)?[0-9]+)?
    """
    var is_float = false
    var rune' = rune
    let number = String()
    number.push_utf32(rune')
    if (rune' == '-') then
      rune' = next_rune()
      number.push_utf32(rune')
    end
    if (rune' == '0') then
      rune' = next_rune()
      if (rune' >= '0') and (rune' <= '9') then
        // 0[0123456789] is illegal
        error
      else
        push_back(rune')
      end
    else
      _read_digits(number)
    end
    rune' = next_rune()
    if (rune' == '.') then
      is_float = true
      number.push_utf32(rune')
      _read_digits(number)
    else
      push_back(rune')
    end
    rune' = next_rune()
    if (rune' == 'E') or (rune' == 'e') then
      number.push_utf32(rune')
      rune' = next_rune()
      if (rune' == '-') or (rune' == '+') then
        number.push_utf32(rune')
        _read_digits(number)
      end
    end
    if is_float then
      GraphQLToken(number.clone(), GraphQLFloat, _line)
    else
      GraphQLToken(number.clone(), GraphQLInt, _line)
    end

  fun ref _make_unicode(d1 : U32, d2 : U32, d3 : U32, d4 : U32)  : U32 =>
    0

  fun ref _read_string() : GraphQLToken ? =>
    """
    Read a quoted string. Lexes common escape sequences.
    "([^"\\\u000A\u000D]|(\\(u[0-9a-fA-F]{4}|["\\/bfnrt])))*"
    """
    let string = String()
    while has_next_rune() do
      let r = next_rune()
      match r
      | '"' => break
      | '\\' =>
        let e = next_rune()
        match e
        | '"' => string.push('"')
        | '/' => string.push('/')
        | '\\' => string.push('\\')
        | 'b' => string.push('\b')
        | 'f' => string.push('\f')
        | 'n' => string.push('\n')
        | 'r' => string.push('\r')
        | 't' => string.push('\t')
        | 'u' =>
          let d1 = next_rune()
          let d2 = next_rune()
          let d3 = next_rune()
          let d4 = next_rune()
          string.push_utf32(_make_unicode(d1, d2, d3, d4))
        else
          error
        end
      else
        string.push_utf32(r)
      end
    end
    GraphQLToken(string.clone(), GraphQLString, _line)

  fun ref _skip_whitespace() =>
    try
      while has_next_rune() do
        let rune = next_rune()
        match rune
        | ' ' => None
        | '\t' => None
        | ',' => None
        | '\n' => None
        | '\r' => None
        | 0xFEFF => None
        else
          push_back(rune)
          break
        end
      end
    end

  fun ref next_token() : (GraphQLToken|None) =>
    """
    Gets the next token from the source starting at the current position.

    This skips over whitespace and comments until it finds the next lexable
    token, then lexes punctuators immediately or calls the appropriate helper
    function for more complicated tokens.
    """
    try
      _skip_whitespace()
      let rune = next_rune()
      match rune
      | BANG.rune() => GraphQLToken("!", BANG, _line)
      | '#' => _read_comment()
      | DOLLAR.rune() => GraphQLToken("$", DOLLAR, _line)
      | ParenL.rune() => GraphQLToken("(", ParenL, _line)
      | ParenR.rune() => GraphQLToken(")", ParenR, _line)
      | '.' => _read_spread()
      | COLON.rune() => GraphQLToken(":", COLON, _line)
      | EQUALS.rune() => GraphQLToken("=", EQUALS, _line)
      | AT.rune() => GraphQLToken("@", AT, _line)
      | BracketL.rune() => GraphQLToken("[", BracketL, _line)
      | BracketR.rune() => GraphQLToken("]", BracketR, _line)
      | BraceL.rune() => GraphQLToken("{", BraceL, _line)
      | PIPE.rune() => GraphQLToken("|", PIPE, _line)
      | BraceR.rune() => GraphQLToken("}", BraceR, _line)
      | let c : U32 if ((c >= 'A') and (c <= 'Z'))
                    or ((c >= 'a') and (c <= 'z'))
                    or (c == '_')  =>
        _read_name(c)
      | let c : U32 if ((c >= '0') and (c <= '9')) or (c == '-') =>
        _read_number(c)
      | '"' =>
        _read_string()
      else
        // TODO return error...
        None
      end
    end

  fun ref values() : GraphQLTokens => GraphQLTokens(this)
