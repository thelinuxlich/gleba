import gleam/erlang/process
import mist
import wisp
import router
import db

pub fn main() {
  let db = db.init(15)
  wisp.configure_logger()
  let secret_key_base = wisp.random_string(64)

  let assert Ok(_) =
    wisp.mist_service(router.handle_request(db), secret_key_base)
    |> mist.new
    |> mist.port(80)
    |> mist.start_http

  process.sleep_forever()
}
