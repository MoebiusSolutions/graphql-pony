class Tokens is Iterator[Token val]
  let _lexer : GraphQLLexer ref
  var _next : (Token|None)

  new create(lexer : GraphQLLexer ref) =>
    _lexer = lexer
    _next = _lexer.next_token()

  fun has_next(): Bool =>
    if _next is None then
      false
    else
      true
    end

  fun ref next() : Token val ? =>
    match _next = _lexer.next_token()
    | let t : Token =>
      t
    else
      error
    end

class GraphQLLexer
  let env: Env
  let _input: String
  let _runes: StringRunes
  var _collect: String ref = String()
  var _position: U32 = 0
  var _line: U32 = 1
  var _column: U32 = 1
  var _nextRune: (U32|None) = None
  var _token: Token = Token(SOF, "", 0, 0, 0, 0)
  var _lastToken: Token = Token(SOF, "", 0, 0, 0, 0)

  new create(env' : Env, input : String) =>
    env = env'
    _input = input
    _runes = input.runes()

  fun ref token(): Token =>
    _token

  fun ref lastToken(): Token =>
    _lastToken

  fun ref advance() =>
    var token' = _token
    _lastToken = _token

    if (token'.kind is EOF) == false then
      repeat
        token' = match next_token()
        | let t: Token => t
        else Token(EOF, "", _position, _position, _line, _column) end
// //        token = token.next = readToken(this, token);
      until (token'.kind is COMMENT) == false end
      _token = token'
    end
    token'

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
    _position = _position - 1
    if rune == '\n' then
      _line = _line - 1
    else
      _column = _column - 1
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
    _position = _position + 1
    if rune == '\n' then
      _line = _line + 1
      _column = 1
    else
      _column = _column + 1
    end
    rune

  fun ref _read_name(rune : U32) : Token ? =>
    """
    Reads an alphanumeric + underscore name from the source.

    [_A-Za-z][_0-9A-Za-z]*
    """
    let start = _position - 1
    let col = _column - 1
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
    Token(NAME, name.clone(), start, _position, _line, col)

  fun ref _read_comment() : Token ? =>
    """
    Read comment - from # until the end of the line
    """
    let start = _position - 1
    let col = _column - 1
    let comment = String()
    while has_next_rune() do
      let r = next_rune()
      if r == '\n' then
        break
      else
        comment.push_utf32(r)
      end
    end
    Token(COMMENT, comment.clone(), start, _position, _line, col)

  fun ref _read_spread() : Token ? =>
    let start = _position - 1
    let col = _column - 1
    let dot2 = next_rune()
    if dot2 != '.' then
      error
    end
    let dot3 = next_rune()
    if dot3 != '.' then
      error
    end
    Token(SPREAD, "...", start, _position, _line, col)

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

  fun ref _read_number(rune : U32) : Token ? =>
    """
    Reads a number token from the source file, either a float
    or an int depending on whether a decimal point appears.

    Int:   -?(0|[1-9][0-9]*)
    Float: -?(0|[1-9][0-9]*)(\.[0-9]+)?((E|e)(+|-)?[0-9]+)?
    """
    let start = _position - 1
    let col = _column - 1
    var is_float = false
    let number = String()

    var rune' = rune
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
    else
      push_back(rune')
    end

    if is_float then
      Token(GraphQLFloat, number.clone(), start, _position, _line, col)
    else
      Token(GraphQLInt, number.clone(), start, _position, _line, col)
    end

  fun ref _make_unicode(d1 : U32, d2 : U32, d3 : U32, d4 : U32)  : U32 =>
    0

  fun ref _read_string() : Token ? =>
    """
    Read a quoted string. Lexes common escape sequences.
    "([^"\\\u000A\u000D]|(\\(u[0-9a-fA-F]{4}|["\\/bfnrt])))*"
    """
    let start = _position - 1
    let col = _column - 1
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
    Token(GraphQLString, string.clone(), start, _position, _line, col)

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

  fun ref next_token() : (Token|None) =>
    """
    Gets the next token from the source starting at the current position.

    This skips over whitespace and comments until it finds the next lexable
    token, then lexes punctuators immediately or calls the appropriate helper
    function for more complicated tokens.
    """
    try
      _skip_whitespace()
      let start = _position
      let col = _column
      let rune = next_rune()
      match rune
      | BANG.rune() => Token(BANG, "!", start, _position, _line, col)
      | '#' => _read_comment()
      | DOLLAR.rune() => Token(DOLLAR, "$",  start, _position,_line, col)
      | ParenL.rune() => Token(ParenL, "(",  start, _position,_line, col)
      | ParenR.rune() => Token(ParenR, ")",  start, _position,_line, col)
      | '.' => _read_spread()
      | COLON.rune() => Token(COLON, ":",  start, _position,_line, col)
      | EQUALS.rune() => Token(EQUALS, "=",  start, _position,_line, col)
      | AT.rune() => Token(AT, "@",  start, _position,_line, col)
      | BracketL.rune() => Token(BracketL, "[",  start, _position,_line, col)
      | BracketR.rune() => Token(BracketR, "]",  start, _position,_line, col)
      | BraceL.rune() => Token(BraceL, "{",  start, _position,_line, col)
      | PIPE.rune() => Token(PIPE, "|",  start, _position,_line, col)
      | BraceR.rune() => Token(BraceR, "}",  start, _position,_line, col)
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

  fun ref values() : Tokens => Tokens(this)
