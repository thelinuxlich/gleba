import gleam/erlang/process
import mist
import wisp
import router
import gleam/pgo

pub fn main() {
  let db =
    pgo.connect(
      pgo.Config(
        ..pgo.default_config(),
        host: "localhost",
        database: "postgres",
        pool_size: 15,
      ),
    )
  wisp.configure_logger()
  let secret_key_base = wisp.random_string(64)

  let assert Ok(_) =
    wisp.mist_service(router.handle_request(db), secret_key_base)
    |> mist.new
    |> mist.port(80)
    |> mist.start_http

  process.sleep_forever()
}
