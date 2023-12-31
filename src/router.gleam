import wisp.{Request, Response}
import gleam/http.{Get, Post}
import gleam/pgo.{Connection, ConnectionUnavailable, PostgresqlError, Returned}
import gleam/dynamic.{DecodeError, Dynamic}
import gleam/string.{length}
import gleam/list
import gleam/json
import gleam/http/response.{set_header}
import gleam/http/request.{get_query}
import gleam/option.{Option}
import gleba_utils.{try_nil, try_string}
import gleam/bool.{guard}
import gleam/int
import ids/uuid.{generate_v4}
import gleam/erlang/atom.{Atom}
import gleam/erlang/process.{Pid}
import gleam/erlang/node.{Node}
import gleam/io
import gleam/result

@external(erlang, "kv_server", "start_link")
fn start_link(name: Atom) -> Result(Pid, String)

@external(erlang, "gen_server", "call")
fn call(
  kv: #(Atom, Node),
  msg: #(Atom, #(String, String)),
) -> Result(String, String)

@external(erlang, "calendar", "valid_date")
fn valid_date(year: Int, month: Int, day: Int) -> Bool

pub type Pessoa {
  Pessoa(
    apelido: String,
    nome: String,
    nascimento: String,
    stack: Option(List(String)),
  )
}

fn notify_kv(key: String, value: String) {
  let assert Ok(put) = atom.from_string("put")
  use <- guard(key == "", [Nil])
  [node.self(), ..node.visible()]
  |> list.map(fn(n) { node.send(n, kv_server_name(), #(put, #(key, value))) })
}

fn get_from_kv(key: String) {
  let assert Ok(get) = atom.from_string("get")
  use <- guard(key == "", Error(Nil))
  let data = call(#(kv_server_name(), node.self()), #(get, #(key, "")))
  case data {
    Ok("") -> Error(Nil)
    Ok(data) -> Ok(data)
    Error(_) -> Error(Nil)
  }
}

fn kv_server_name() {
  atom.create_from_string("kv_server")
}

fn node_list() {
  [atom.create_from_string("api1@api1"), atom.create_from_string("api2@api2")]
}

pub fn handle_request(db: Connection) -> fn(Request) -> Response {
  list.map(node_list(), fn(n) { node.connect(n) })
  |> io.debug
  let assert Ok(_) = start_link(kv_server_name())
  fn(req) {
    case wisp.path_segments(req) {
      ["contagem-pessoas"] -> count_pessoas(db)
      ["pessoas"] ->
        case req.method {
          Get -> list_pessoas(req, db)
          Post -> create_pessoa(req, db)
          _ -> wisp.method_not_allowed([Get, Post])
        }
      ["pessoas", id] -> {
        case req.method {
          Get -> get_pessoa(id)
          _ -> wisp.method_not_allowed([Get])
        }
      }
      _ -> wisp.not_found()
    }
  }
}

fn count_pessoas(db: Connection) -> Response {
  let query = "SELECT COUNT(id) FROM pessoas"
  case pgo.execute(query, db, [], dynamic.element(0, dynamic.int)) {
    Ok(response) -> {
      let [count] = response.rows
      wisp.ok()
      |> wisp.string_body(int.to_string(count))
    }
    Error(_) -> wisp.internal_server_error()
  }
}

fn pessoa_return_type() {
  dynamic.tuple5(
    dynamic.string,
    dynamic.string,
    dynamic.string,
    dynamic.string,
    dynamic.string,
  )
}

fn list_pessoas(req: Request, db: Connection) -> Response {
  let result = {
    use params <- try_string(get_query(req), "no params")
    use #(_, search_term) <- try_string(
      list.find(params, fn(x) { x.0 == "t" && x.1 != "" }),
      "No element found",
    )
    let query =
      "SELECT id,apelido,nome,cast(nascimento as text),stack FROM pessoas 
                WHERE search LIKE '%' || $1 || '%' LIMIT 50"
    let response =
      pgo.execute(
        query,
        db,
        [pgo.text(string.lowercase(search_term))],
        pessoa_return_type(),
      )
    case response {
      Ok(Returned(_, rows)) ->
        Ok(json.to_string_builder(json.array(
          rows,
          fn(x) {
            let #(id, apelido, nome, nascimento, stack) = x
            let assert Ok(stack) =
              json.decode(stack, dynamic.list(dynamic.string))
            json.object([
              #("id", json.string(id)),
              #("apelido", json.string(apelido)),
              #("nome", json.string(nome)),
              #("nascimento", json.string(nascimento)),
              #("stack", json.array(stack, json.string)),
            ])
          },
        )))
      Error(PostgresqlError(_, _, message)) -> Error(message)
      Error(ConnectionUnavailable) -> Error("Connection unavailable")
      Error(_) -> Error("Unknown error")
    }
  }
  case result {
    Ok(content) ->
      wisp.ok()
      |> wisp.json_body(content)
    Error(reason) -> {
      case reason {
        "No element found" -> ""
        _ -> io.debug(reason)
      }
      wisp.bad_request()
    }
  }
}

fn dyn_pessoa_decoder() -> fn(Dynamic) -> Result(Pessoa, List(DecodeError)) {
  dynamic.decode4(
    Pessoa,
    dynamic.field("apelido", dynamic.string),
    dynamic.field("nome", dynamic.string),
    dynamic.field("nascimento", dynamic.string),
    dynamic.optional_field("stack", dynamic.list(dynamic.string)),
  )
}

fn get_pessoa(id: String) -> Response {
  let resp = get_from_kv(id)
  case resp {
    Error(_) -> wisp.not_found()
    Ok(data) -> {
      let assert Ok(data) = json.decode(data, dyn_pessoa_decoder())
      let json_string =
        json.to_string_builder(json.object([
          #("id", json.string(id)),
          #("apelido", json.string(data.apelido)),
          #("nome", json.string(data.nome)),
          #("nascimento", json.string(data.nascimento)),
          #("stack", json.array(option.unwrap(data.stack, []), json.string)),
        ]))
      wisp.ok()
      |> wisp.json_body(json_string)
    }
  }
}

fn validate_pessoa(data: Pessoa) -> Bool {
  let Pessoa(apelido, nome, nascimento, stack) = data
  let iso_date = string.split(nascimento, "-")
  case iso_date {
    [year, month, day] -> {
      let year = int.parse(year)
      let month = int.parse(month)
      let day = int.parse(day)
      result.is_ok(year) && result.is_ok(month) && result.is_ok(day) && apelido != "" && nome != "" && valid_date(
        result.unwrap(year, 2023),
        result.unwrap(month, 1),
        result.unwrap(day, 1),
      ) && length(apelido) <= 32 && length(nome) <= 100 && list.any(
        option.unwrap(stack, []),
        fn(x) { x == "" || string.length(x) > 32 },
      ) == False
    }
    _ -> False
  }
}

fn create_pessoa(req: Request, db: Connection) -> Response {
  use json_data <- wisp.require_bit_string_body(req)
  let result = {
    use data <- try_nil(json.decode_bits(json_data, dyn_pessoa_decoder()))
    use <- guard(!validate_pessoa(data), Error(Nil))
    let resp = get_from_kv(data.apelido)
    case resp {
      Error(_) -> {
        // record doesn't exist, so create
        let _ = notify_kv(data.apelido, "1")
        let assert Ok(id) = generate_v4()
        let json_data_stringified =
          json.to_string(json.object([
            #("apelido", json.string(data.apelido)),
            #("nome", json.string(data.nome)),
            #("nascimento", json.string(data.nascimento)),
            #("stack", json.array(option.unwrap(data.stack, []), json.string)),
          ]))
        let _ = notify_kv(id, json_data_stringified)
        let query =
          "INSERT INTO pessoas (id, apelido,nome,nascimento,stack) VALUES ($1,$2,$3,TO_DATE($4, 'YYYY-MM-DD'),$5)"
        let _ =
          pgo.execute(
            query,
            db,
            [
              pgo.text(id),
              pgo.text(data.apelido),
              pgo.text(data.nome),
              pgo.text(data.nascimento),
              pgo.text(json.to_string(json.array(
                option.unwrap(data.stack, []),
                json.string,
              ))),
            ],
            dynamic.dynamic,
          )
        Ok(id)
      }
      // record exists, so return error
      _ -> Error(Nil)
    }
  }
  case result {
    Ok(id) ->
      wisp.created()
      |> set_header("Location", "/pessoas/" <> id)
    Error(_) -> wisp.unprocessable_entity()
  }
}
