import gleam/result.{replace_error, try}

pub fn try_nil(result: Result(a, e), fun: fn(a) -> Result(b, Nil)) {
  try(replace_error(result, Nil), fun)
}

pub fn try_string(
  result: Result(a, e),
  error: String,
  fun: fn(a) -> Result(b, String),
) {
  try(replace_error(result, error), fun)
}
