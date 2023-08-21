import gleam/result.{replace_error, try}

pub fn try_nil(result: Result(a, e), fun: fn(a) -> Result(b, Nil)) {
  try(replace_error(result, Nil), fun)
}
