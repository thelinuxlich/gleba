import gleeunit
import gleeunit/should
import wisp/testing
import router
import db
import gleam/pgo.{Connection}
import gleam/dynamic
import gleam/int

pub fn main() {
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

pub fn count_pessoas_test() {
  let db = db.init(1)
  let query = "SELECT COUNT(id) FROM pessoas"
  let assert Ok(response) = pgo.execute(query, db, [], dynamic.element(0, dynamic.int))
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

pub fn get_pessoa_test() {
  let db = db.init(1)
  let _ = add_sample_pessoa(db)
  let request = testing.get("/pessoas/dc951540-3224-4f0c-904e-3b1d18ace874", [])
  let response = router.handle_request(db)(request)
  response.status
  |> should.equal(200)
  response.headers
  |> should.equal([#("Content-Type", "application/json")])
  response
  |> testing.string_body
  |> should.equal(
    "{\"apelido\":\"Silva\",\"nome\":\"João\",\"nascimento\":\"1990-01-01\",\"stack\":[\"Gleam\"]}",
  )
  let request = testing.get("/pessoas/foo", [])
  let response = router.handle_request(db)(request)
  response.status
  |> should.equal(404)
}

pub fn get_pessoas_test() {
  let db = db.init(1)
  let _ = add_sample_pessoa(db)
  let request = testing.get("/pessoas?t=Silva", [])
  let response = router.handle_request(db)(request)
  response.status
  |> should.equal(200)
  response.headers
  |> should.equal([#("Content-Type", "application/json")])
  response
  |> testing.string_body
  |> should.equal(
    "[{\"id\":\"dc951540-3224-4f0c-904e-3b1d18ace874\",\"nome\":\"João\",\"apelido\":\"Silva\",\"nascimento\":\"1990-01-01\",\"stack\":[\"Gleam\"]}]",
  )
  let request = testing.get("/pessoas", [])
  let response = router.handle_request(db)(request)
  response.status
  |> should.equal(400)
  let request = testing.get("/pessoas?t=", [])
  let response = router.handle_request(db)(request)
  response.status
  |> should.equal(400)
}

pub fn create_pessoa_test() {
  let db = db.init(1)
  let request = testing.post(
    "/pessoas", [], "{\"nome\": \"João\", \"apelido\": \"Fulano\", \"nascimento\": \"1990-01-01\", \"stack\": [\"Gleam\"]}"
  )
  let response = router.handle_request(db)(request)
  //let query = "SELECT id FROM pessoas where apelido = 'Fulano'"
  //let assert Ok(data) = pgo.execute(query, db, [], dynamic.element(0, dynamic.string))
  //let [id] = data.rows
  response.status
  |> should.equal(201)
  response.headers
  |> should.equal([#("Location", "/pessoas/1")])
  let request = testing.post(
    "/pessoas", [], "{\"nome\": \"João\", \"apelido\": \"Silva\", \"nascimento\": \"1990-01-01\", \"stack\": [\"Gleam\"]}"
  )
  let response = router.handle_request(db)(request)
  response.status
  |> should.equal(422)
}
