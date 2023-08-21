import wisp.{Request, Response, Text}
import gleam/http.{Get, Post}
import gleam/pgo.{Connection}
import gleam/dynamic.{DecodeError, Dynamic}
import gleam/string.{length}
import gleam/string_builder.{from_string}
import gleam/list
import gleam/json
import gleam/result.{try}
import gleam/http/response.{set_header}
import gleam/http/request.{get_query}
import gleam/option.{Option, Some}
import gleba_utils.{try_nil}
import gleam/bool.{guard}
import gleam/int
import gleam/regex
import gluon.{Socket}
import redis
import gleam/io

pub type Pessoa {
  Pessoa(
    apelido: String,
    nome: String,
    nascimento: String,
    stack: Option(List(String)),
  )
}

pub fn handle_request(db: Connection) -> fn(Request) -> Response {
  fn(req) {
    let socket = redis.init()
    let result = {
      case wisp.path_segments(req) {
        ["contagem-pessoas"] -> count_pessoas(db)
        ["pessoas"] ->
          case req.method {
            Get -> list_pessoas(req, db)
            Post -> create_pessoa(req, db, socket)
            _ -> wisp.method_not_allowed([Get, Post])
          }
        ["pessoas", id] -> {
          case req.method {
            Get -> get_pessoa(id, socket)
            _ -> wisp.method_not_allowed([Get])
          }
        }
        _ -> wisp.not_found()
      }
    }
    let _ = gluon.close(socket)
    result
  }
}

fn count_pessoas(db: Connection) -> Response {
  let query = "SELECT COUNT(id) FROM pessoas"
  case pgo.execute(query, db, [], dynamic.element(0, dynamic.int)) {
    Ok(response) -> {
      let [count] = response.rows
      wisp.ok()
      |> wisp.set_body(Text(from_string(int.to_string(count))))
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
      |> set_header("Content-Type", "application/json")
      |> wisp.set_body(Text(content))
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

fn get_pessoa(id: String, socket: Socket) -> Response {
  let resp = gluon.get(socket, id)
  case resp {
    Ok(data) -> {
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
      |> set_header("Content-Type", "application/json")
      |> wisp.set_body(Text(json_string))
    }
    Error(_) -> wisp.not_found()
  }
}

fn validate_pessoa(data: Pessoa) -> Bool {
  let assert Ok(re) =
    regex.from_string("\\d{4}\\-(0[1-9]|1[012])\\-(0[1-9]|[12][0-9]|3[01])")
  let Pessoa(apelido, nome, nascimento, stack) = data
  apelido != "" && nome != "" && regex.check(re, nascimento) && length(apelido) <= 32 && length(
    nome,
  ) <= 100 && length(nascimento) <= 10 && list.any(
    option.unwrap(stack, []),
    fn(x) { x == "" || string.length(x) > 32 },
  ) == False
}

fn create_pessoa(req: Request, db: Connection, socket: Socket) -> Response {
  use json_data <- wisp.require_bit_string_body(req)
  let result = {
    use data <- try_nil(json.decode_bits(json_data, dyn_pessoa_decoder()))
    use <- guard(!validate_pessoa(data), Error(Nil))
    let resp = gluon.get(socket, data.apelido)
    case resp {
      Error(_) -> {
        // record doesn't exist, so create
        let query =
          "INSERT INTO pessoas (apelido,nome,nascimento,stack) VALUES ($1,$2,TO_DATE($3, 'YYYY-MM-DD'),$4) RETURNING ID"
        use response <- try_nil(pgo.execute(
          query,
          db,
          [
            pgo.text(data.apelido),
            pgo.text(data.nome),
            pgo.text(data.nascimento),
            pgo.text(json.to_string(json.array(
              option.unwrap(data.stack, []),
              json.string,
            ))),
          ],
          dynamic.element(0, dynamic.string),
        ))
        use id <- try(list.at(response.rows, 0))
        let json_data_stringified =
          json.to_string(json.object([
            #("apelido", json.string(data.apelido)),
            #("nome", json.string(data.nome)),
            #("nascimento", json.string(data.nascimento)),
            #("stack", json.array(option.unwrap(data.stack, []), json.string)),
          ]))
        let command =
          "MSET '" <> id <> "' '" <> json_data_stringified <> "' '" <> data.apelido <> "' '1'"
        let _ = gluon.send_command(socket, command)
        Ok(id)
      }
      Ok(_) -> Error(Nil)
    }
  }
  // record exists, so return error
  case result {
    Ok(id) ->
      wisp.created()
      |> set_header("Location", "/pessoas/" <> id)
    Error(_) -> wisp.response(422)
  }
}
