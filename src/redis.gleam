import gluon
import gleam/erlang/os.{get_env}
import gleam/result.{unwrap}
import gleam/int

pub fn init() {
  let assert Ok(redis_port) = int.parse(unwrap(get_env("REDIS_PORT"), "6379"))
  let redis_host = unwrap(get_env("REDIS_HOST"), "localhost")
  let _ = gluon.main(redis_host, redis_port)
}
