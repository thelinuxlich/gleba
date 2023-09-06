import gleam/pgo
import gleam/option.{Some}
import gleam/erlang/os.{get_env}
import gleam/result.{unwrap}
import gleam/int

pub fn init(pool_size: Int) {
  let host = unwrap(get_env("POSTGRES_HOST"), "localhost")
  let assert Ok(port) = int.parse(unwrap(get_env("POSTGRES_PORT"), "5432"))
  let database = unwrap(get_env("POSTGRES_DB"), "postgres")
  let user = unwrap(get_env("POSTGRES_USER"), "postgres")
  let password = unwrap(get_env("POSTGRES_PASSWORD"), "test")
  pgo.connect(
    pgo.Config(
      ..pgo.default_config(),
      queue_target: 1,
      queue_interval: 1,
      host: host,
      port: port,
      database: database,
      user: user,
      password: Some(password),
      pool_size: pool_size,
    ),
  )
}
