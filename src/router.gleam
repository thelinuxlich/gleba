import wisp.{Request, Response, Text}
import gleam/http.{Get, Post}
import gleam/pgo.{Connection}
import gleam/dynamic
import gleam/string.{length}
import gleam/string_builder.{from_string}
import gleam/list
import gleam/json
import gleam/result.{try}
import gleam/http/response.{set_header}
import gleam/http/request.{get_query}
import helpers.{try_nil}

pub type Pessoa {
  Pessoa(apelido: String, nome: String, nascimento: String, stack: List(String))
}

pub fn handle_request(db: Connection) -> fn(Request) -> Response {
  fn(req) {
    case wisp.path_segments(req) {
      ["contagem-pessoas"] -> count_pessoas(db)
      ["pessoas"] ->
        case req.method {
          Get -> list_pessoas(req, db)
          Post -> create_pessoa(req, db)
          _ -> wisp.method_not_allowed([Get, Post])
        }
      ["pessoas", id] -> get_pessoa(id, db)
      _ -> wisp.not_found()
    }
  }
}

fn count_pessoas(db: Connection) {
  let query = "SELECT COUNT(*) FROM pessoas"
  case pgo.execute(query, db, [], dynamic.string) {
    Ok(response) -> {
      let [count] = response.rows
      wisp.ok()
      |> wisp.set_body(Text(from_string(count)))
    }
    Error(_) -> wisp.internal_server_error()
  }
}

fn list_pessoas(req: Request, db: Connection) {
  let result = {
    use params <- try(get_query(req))
    use #(_, search_term) <- try(list.find(params, fn(x) { x.0 == "t" }))
    let return_type =
      dynamic.tuple4(
        dynamic.string,
        dynamic.string,
        dynamic.string,
        dynamic.string,
      )
    let query =
      "SELECT apelido,nome,nascimento,stack FROM pessoas 
                WHERE apelido LIKE '%' || $1 || '%' OR nome LIKE '%' || $1 || '%'
                OR nascimento LIKE '%' || $1 || '%'
                OR stack LIKE '%' || $1 || '%' 
            "
    use response <- try_nil(pgo.execute(
      query,
      db,
      [pgo.text(search_term)],
      return_type,
    ))
    Ok(json.to_string_builder(json.array(
      response.rows,
      fn(x) {
        let assert Ok(stack) = json.decode(x.3, dynamic.list(dynamic.string))
        json.object([
          #("apelido", json.string(x.0)),
          #("nome", json.string(x.1)),
          #("nascimento", json.string(x.2)),
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

fn get_pessoa(id: String, db: Connection) {
  let query = "SELECT apelido,nome,nascimento,stack FROM pessoas WHERE id = $1"
  let return_type =
    dynamic.tuple4(
      dynamic.string,
      dynamic.string,
      dynamic.string,
      dynamic.string,
    )
  case pgo.execute(query, db, [pgo.text(id)], return_type) {
    Ok(response) -> {
      case response.rows {
        [] -> wisp.not_found()
        _ -> {
          let [#(apelido, nome, nascimento, stack)] = response.rows
          let assert Ok(stack) =
            json.decode(stack, dynamic.list(dynamic.string))
          let json_string =
            json.to_string_builder(json.object([
              #("apelido", json.string(apelido)),
              #("nome", json.string(nome)),
              #("nascimento", json.string(nascimento)),
              #("stack", json.array(stack, json.string)),
            ]))
          wisp.ok()
          |> set_header("Content-Type", "application/json")
          |> wisp.set_body(Text(json_string))
        }
      }
    }
    Error(_) -> wisp.internal_server_error()
  }
}

fn validate_pessoa(data: Pessoa) {
  let Pessoa(apelido, nome, nascimento, stack) = data
  apelido != "" && nome != "" && nascimento != "" && length(apelido) <= 32 && length(
    nome,
  ) <= 100 && length(nascimento) <= 10 && list.any(
    stack,
    fn(x) { x == "" || string.length(x) > 32 },
  ) == False
}

fn create_pessoa(req: Request, db: Connection) {
  use json_data <- wisp.require_bit_string_body(req)
  let json_decoded =
    json.decode_bits(
      json_data,
      dynamic.decode4(
        Pessoa,
        dynamic.field("apelido", dynamic.string),
        dynamic.field("nome", dynamic.string),
        dynamic.field("nascimento", dynamic.string),
        dynamic.field("stack", dynamic.list(dynamic.string)),
      ),
    )
  case json_decoded {
    Ok(data) -> {
      case validate_pessoa(data) {
        True -> {
          let query =
            "INSERT INTO pessoas (apelido,nome,nascimento,stack) VALUES ($1,$2,$3,$4) RETURNING ID"
          case
            pgo.execute(
              query,
              db,
              [
                pgo.text(data.apelido),
                pgo.text(data.nome),
                pgo.text(data.nascimento),
                pgo.text(json.to_string(json.array(data.stack, json.string))),
              ],
              dynamic.string,
            )
          {
            Ok(response) -> {
              let [id] = response.rows
              wisp.created()
              |> set_header("Location", "/pessoas/" <> id)
            }
            Error(_) -> wisp.response(422)
          }
        }
        False -> wisp.response(422)
      }
    }
    _ -> wisp.response(422)
  }
}
