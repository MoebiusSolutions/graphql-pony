use "net/http"
use "json"

actor Main
  new create(env: Env) =>
    let service = try env.args(1) else "50000" end
    let limit = try env.args(2).usize() else 100 end

    let logger = CommonLog(env.out)
    // let logger = ContentsLog(env.out)
    // let logger = DiscardLog

    let auth = try
      env.root as AmbientAuth
    else
      env.out.print("unable to use network")
      return
    end

    Server(auth, Info(env), Handle(env), logger
      where service=service, limit=limit, reversedns=auth
    )

class Info
  let _env: Env

  new iso create(env: Env) =>
    _env = env

  fun ref listening(server: Server ref) =>
    try
      (let host, let service) = server.local_address().name()
      _env.out.print("Listening on " + host + ":" + service)
    else
      _env.out.print("Couldn't get local address.")
      server.dispose()
    end

  fun ref not_listening(server: Server ref) =>
    _env.out.print("Failed to listen.")

  fun ref closed(server: Server ref) =>
    _env.out.print("Shutdown.")

class Handle
  let _env: Env

  new iso create(env: Env) =>
    _env = env

  fun parse_json(s: String, env: Env): JsonDoc ? =>
    let json_doc: JsonDoc = JsonDoc
    try
      json_doc.parse(s)
      json_doc
    else
      env.err.print("err" + json_doc.parse_report()._2)
      error
    end

  fun val apply(request: Payload) =>
    _env.out.print("Hello, world.")
    let response = Payload.response()
    response.add_chunk("Just a path here: ")
    response.add_chunk(request.method)
    response.add_chunk(request.url.path)
    response.add_chunk(request.body_size().string())
    response.add_chunk("\n")

    let req = recover
      let read: Payload ref = consume request
      var collect: String val = ""
      let chunks = read.body().values()
      for c in chunks do
        response.add_chunk(c)
        match c
        | let cs: String => collect = collect.add(cs)
        | let ca: Array[U8 val] val => collect = collect.add(String.from_array(ca))
        end
      end
      _env.out.print("My string is ")
      _env.out.print(collect)
      _env.out.print("end")

      try
        let json = parse_json(collect, _env)
        _env.out.print("parsed:" + json.string())
      end

      consume read
    end

    // if request.url.query.size() > 0 then
    //   response.add_chunk("?")
    //   response.add_chunk(request.url.query)
    // end

    // if request.url.fragment.size() > 0 then
    //   response.add_chunk("#")
    //   response.add_chunk(request.url.fragment)
    // end

    (consume req).respond(consume response)
