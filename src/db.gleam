import gleam/pgo
import gleam/option.{Some}

pub fn init(pool_size: Int) {
  pgo.connect(
    pgo.Config(
      ..pgo.default_config(),
      host: "localhost",
      database: "postgres",
      user: "postgres",
      password: Some("test"),
      pool_size: pool_size,
    ),
  )
}
