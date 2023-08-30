import wisp.{Request, Response}
import gleam/http.{Get, Post}
import gleam/pgo.{Connection}
import gleam/dynamic.{DecodeError, Dynamic}
import gleam/string.{length}
import gleam/list
import gleam/json
import gleam/result.{try}
import gleam/http/response.{set_header}
import gleam/http/request.{get_query}
import gleam/option.{Option, Some}
import gleba_utils.{try_nil}
import gleam/bool.{guard}
import gleam/int
import ids/uuid.{generate_v4}
import gleam/erlang/atom.{Atom}

@external(erlang, "calendar", "valid_date")
fn valid_date(year: Int, month: Int, day: Int) -> Bool

@external(erlang, "erlang", "node")
fn node() -> Atom

@external(erlang, "erlang", "nodes")
fn nodes() -> List(Atom)

@external(erlang, "net_kernel", "connect_node")
fn connect(node: Atom) -> Bool

@external(erlang, "ram", "start_cluster")
fn start_cluster(nodes: List(Atom)) -> String

@external(erlang, "ram", "get")
fn ram_get(id: String, default: String) -> String

@external(erlang, "ram", "put")
fn ram_put(id: String, data: String) -> String

pub type Pessoa {
  Pessoa(
    apelido: String,
    nome: String,
    nascimento: String,
    stack: Option(List(String)),
  )
}

pub fn handle_request(db: Connection) -> fn(Request) -> Response {
  let node_name = atom.to_string(node())
  case node_name {
    "api1@api1" -> {
      wisp.log_info("initializing cluster...")
      nodes()
      |> list.map(fn(n) { connect(n) })
      start_cluster(nodes())
      Nil
    }
    node -> wisp.log_info("already initialized..." <> node)
  }
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

fn list_pessoas(req: Request, db: Connection) -> Response {
  let result = {
    use params <- try(get_query(req))
    use #(_, search_term) <- try(list.find(
      params,
      fn(x) { x.0 == "t" && x.1 != "" },
    ))
    let return_type =
      dynamic.tuple5(
        dynamic.string,
        dynamic.string,
        dynamic.string,
        dynamic.string,
        dynamic.string,
      )
    let query =
      "SELECT id,apelido,nome,cast(nascimento as text),stack FROM pessoas 
                WHERE search LIKE '%' || $1 || '%' LIMIT 50"
    use response <- try_nil(pgo.execute(
      query,
      db,
      [pgo.text(string.lowercase(search_term))],
      return_type,
    ))
    Ok(json.to_string_builder(json.array(
      response.rows,
      fn(x) {
        let assert Ok(stack) = json.decode(x.4, dynamic.list(dynamic.string))
        json.object([
          #("id", json.string(x.0)),
          #("apelido", json.string(x.1)),
          #("nome", json.string(x.2)),
          #("nascimento", json.string(x.3)),
          #("stack", json.array(stack, json.string)),
        ])
      },
    )))
  }
  case result {
    Ok(content) ->
      wisp.ok()
      |> wisp.json_body(content)
    Error(_) -> wisp.bad_request()
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
  let resp = ram_get(id, "")
  case resp {
    "" -> wisp.not_found()
    data -> {
      let assert Ok(decoded_data) = json.decode(data, dyn_pessoa_decoder())
      let Pessoa(apelido, nome, nascimento, Some(stack)) = decoded_data
      let json_string =
        json.to_string_builder(json.object([
          #("id", json.string(id)),
          #("apelido", json.string(apelido)),
          #("nome", json.string(nome)),
          #("nascimento", json.string(nascimento)),
          #("stack", json.array(stack, json.string)),
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
    let resp = ram_get(data.apelido, "")
    case resp {
      "" -> {
        // record doesn't exist, so create
        let _ = ram_put(data.apelido, "1")
        let assert Ok(id) = generate_v4()
        let json_data_stringified =
          json.to_string(json.object([
            #("apelido", json.string(data.apelido)),
            #("nome", json.string(data.nome)),
            #("nascimento", json.string(data.nascimento)),
            #("stack", json.array(option.unwrap(data.stack, []), json.string)),
          ]))
        let _ = ram_put(id, json_data_stringified)
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
      _ -> Error(Nil)
    }
  }
  // record exists, so return error
  case result {
    Ok(id) ->
      wisp.created()
      |> set_header("Location", "/pessoas/" <> id)
    Error(_) -> wisp.unprocessable_entity()
  }
}
