import gleam/erlang/process
import gleam/erlang/os.{get_env}
import gleam/result.{unwrap}
import gleam/int
import mist
import wisp
import router
import db

pub fn main() {
  let assert Ok(pool_size) = int.parse(unwrap(get_env("POSTGRES_POOL"), "15"))
  let db = db.init(pool_size)
  wisp.configure_logger()
  let secret_key_base = wisp.random_string(64)
  let assert Ok(_) =
    wisp.mist_handler(router.handle_request(db), secret_key_base)
    |> mist.new
    |> mist.port(80)
    |> mist.start_http

  process.sleep_forever()
}
