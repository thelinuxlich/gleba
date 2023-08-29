import gleam/otp/actor
import gleam/erlang/process.{Subject}
import gleam/iterator.{map, repeatedly, take, to_list}
import ids/uuid.{generate_v4}
import gleam/result.{unwrap}

pub fn init() {
  let uuids =
    generate_v4
    |> repeatedly
    |> take(50_000)
    |> map(fn(x) { unwrap(x, "") })
    |> to_list
  let assert Ok(_) = actor.start(uuids, handle_message)
}

pub type Pop(element) {
  Pop(reply_with: Subject(Result(element, Nil)))
}

fn handle_message(message: Pop(e), stack: List(e)) -> actor.Next(List(e)) {
  case message {
    Pop(client) ->
      case stack {
        [] -> {
          process.send(client, Error(Nil))
          actor.Continue([])
        }

        [first, ..rest] -> {
          process.send(client, Ok(first))
          actor.Continue(rest)
        }
      }
  }
}
