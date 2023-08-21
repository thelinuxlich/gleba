import gleeunit
import gleeunit/should
import wisp/testing
import router
import db
import gleam/pgo.{Connection}
import gleam/dynamic
import gleam/int
import gleam/list
import gleam/http
import gleam/http/request
import gleam/option.{None, Some}
import wisp
import redis
import gluon

pub fn main() {
  let db = db.init(1)
  let _ = pgo.execute("DELETE FROM pessoas", db, [], dynamic.dynamic)
  let socket = redis.init()
  let _ = gluon.send_command(socket,"FLUSHALL")
  let _ = gluon.close(socket)
  gleeunit.main()
}

pub fn not_found_test() {
  let db = db.init(1)
  let request = testing.get("/not-found", [])
  let response = router.handle_request(db)(request)
  response.status
  |> should.equal(404)
}

pub fn forbidden_method_test() {
  let db = db.init(1)
  let request = testing.delete("/pessoas", [], "")
  let response = router.handle_request(db)(request)

  response.status
  |> should.equal(405)
  response.headers
  |> should.equal([#("allow", "GET, POST")])
}

fn find_id_by_apelido(db: Connection, apelido: String) -> Result(String, Nil) {
  let query = "SELECT id FROM pessoas where apelido = $1"
  let assert Ok(data) =
    pgo.execute(
      query,
      db,
      [pgo.text(apelido)],
      dynamic.element(0, dynamic.string),
    )
  let assert Ok(_) = list.at(data.rows, 0)
}

pub fn count_pessoas_test() {
  let db = db.init(1)
  let query = "SELECT COUNT(id) FROM pessoas"
  let assert Ok(response) =
    pgo.execute(query, db, [], dynamic.element(0, dynamic.int))
  let [count] = response.rows
  let request = testing.get("/contagem-pessoas", [])
  let response = router.handle_request(db)(request)

  response.status
  |> should.equal(200)
  response
  |> testing.string_body
  |> should.equal(int.to_string(count))
}

fn add_sample_pessoa(db: Connection) {
  let query = "DELETE FROM PESSOAS"
  let assert Ok(_) = pgo.execute(query, db, [], dynamic.dynamic)
  let query =
    "INSERT INTO PESSOAS(id, nome, apelido, nascimento, stack) VALUES ('dc951540-3224-4f0c-904e-3b1d18ace874', 'João', 'Silva', '1990-01-01', '[\"Gleam\"]') ON CONFLICT DO NOTHING"
  let assert Ok(_) = pgo.execute(query, db, [], dynamic.dynamic)
}

pub fn create_pessoa_test() {
  let db = db.init(1)
  let request =
    testing.post(
      "/pessoas",
      [],
      "{\"nome\": \"João\", \"apelido\": \"Fulano\", \"nascimento\": \"1990-01-01\", \"stack\": [\"Gleam\"]}",
    )
  let response = router.handle_request(db)(request)
  response.status
  |> should.equal(201)
  let assert Ok(id) = find_id_by_apelido(db, "Fulano")
  response.headers
  |> should.equal([#("Location", "/pessoas/" <> id)])

  let request =
    testing.post(
      "/pessoas",
      [],
      "{\"nome\": \"João\", \"apelido\": \"Das Neves\", \"nascimento\": \"1990-01-01\", \"stack\": null}",
    )
  let response = router.handle_request(db)(request)
  response.status
  |> should.equal(201)

  let request =
    testing.post(
      "/pessoas",
      [],
      "{\"nome\": \"João\", \"apelido\": \"Das Couves\", \"nascimento\": \"1990-01-01\"}",
    )
  let response = router.handle_request(db)(request)
  response.status
  |> should.equal(201)

  let request =
    testing.post(
      "/pessoas",
      [],
      "{\"nome\": \"João\", \"apelido\": \"Fulano\", \"nascimento\": \"1990-01-01\", \"stack\": [\"Gleam\"]}",
    )
  let response = router.handle_request(db)(request)
  response.status
  |> should.equal(422)

  let request =
    testing.post(
      "/pessoas",
      [],
      "{\"nome\": \"João\", \"apelido\": \"\", \"nascimento\": \"1990-01-01\", \"stack\": [\"Gleam\"]}",
    )
  let response = router.handle_request(db)(request)
  response.status
  |> should.equal(422)

  let request =
    testing.post(
      "/pessoas",
      [],
      "{\"nome\": \"João\", \"apelido\": \"\", \"nascimento\": \"3000-01-01\", \"stack\": [\"Gleam\"]}",
    )
  let response = router.handle_request(db)(request)
  response.status
  |> should.equal(422)

  let request =
    testing.post(
      "/pessoas",
      [],
      "{\"apelido\": \"Bla\", \"nascimento\": \"3000-01-01\", \"stack\": [\"Gleam\"]}",
    )
  let response = router.handle_request(db)(request)
  response.status
  |> should.equal(422)
}

pub fn get_pessoa_test() {
  let db = db.init(1)
  let assert Ok(id) = find_id_by_apelido(db, "Fulano")
  let request = testing.get("/pessoas/" <> id, [])
  let response = router.handle_request(db)(request)
  response.status
  |> should.equal(200)
  response.headers
  |> should.equal([#("Content-Type", "application/json")])
  response
  |> testing.string_body
  |> should.equal(
    "{\"id\":\"" <> id <> "\",\"apelido\":\"Fulano\",\"nome\":\"João\",\"nascimento\":\"1990-01-01\",\"stack\":[\"Gleam\"]}",
  )
  let request = testing.get("/pessoas/foo", [])
  let response = router.handle_request(db)(request)
  response.status
  |> should.equal(404)
}

// this is a workaround because wisp/testing doesn't parse the querystring yet
fn generate_request_with_qs(path: String, qs: String) {
  request.Request(
    method: http.Get,
    headers: [],
    body: <<>>,
    scheme: http.Https,
    host: "localhost",
    port: None,
    path: path,
    query: Some(qs),
  )
  |> request.set_body(wisp.create_canned_connection(
    <<>>,
    "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
  ))
}

pub fn get_pessoas_test() {
  let db = db.init(1)
  let _ = add_sample_pessoa(db)
  let request = generate_request_with_qs("/pessoas", "t=Silva")
  let response = router.handle_request(db)(request)
  let assert Ok(id) = find_id_by_apelido(db, "Silva")
  response.status
  |> should.equal(200)
  response.headers
  |> should.equal([#("Content-Type", "application/json")])
  response
  |> testing.string_body
  |> should.equal(
    "[{\"id\":\"" <> id <> "\",\"apelido\":\"Silva\",\"nome\":\"João\",\"nascimento\":\"1990-01-01\",\"stack\":[\"Gleam\"]}]",
  )
  let request = generate_request_with_qs("/pessoas", "")
  let response = router.handle_request(db)(request)
  response.status
  |> should.equal(400)
  let request = generate_request_with_qs("/pessoas", "t=")
  let response = router.handle_request(db)(request)
  response.status
  |> should.equal(400)
}
