abort_bug <- function(message, .envir = parent.frame()) {
  # Crude but want to allow glue expressions to be in message
  message <- sprintf("Internal error: %s", message)
  cli_abort(
    c(
      x = message,
      i = "This is a bug in statable. Please file a {.href [GitHub issue](https://github.com/james-atkins/statable/issues)}."
    ),
    .envir = .envir,
    class = "statable_error_bug"
  )
}

abort_bug_if <- function(condition, message, .envir = parent.frame()) {
  if (condition) {
    abort_bug(message, .envir = .envir)
  }
}

stop_stata <- function(code, message, call = NULL) {
  cli_abort(
    message = c(
      x = "Stata error {code}: {message}.",
      i = "Run {.code search rc {code}} for more information about this error."
    ),
    code = code,
    call = call,
    class = "statable_error_stata"
  )
}

stack <- function(mode = "list") {
  .data <- vector(mode)

  object <- list(
    set = function(data) {
      .data <<- as.vector(data, mode = mode)
    },

    push = function(...) {
      dots <- list(...)
      for (data in dots) {
        if (is.null(data))
          .data[length(.data) + 1] <<- list(NULL)
        else
          .data[[length(.data) + 1]] <<- data
      }
    },

    pop = function() {
      item <- .data[[length(.data)]]
      length(.data) <<- length(.data) - 1
      item
    },

    peek = function() {
      .data[[length(.data)]]
    },

    contains = function(data) {
      data %in% .data
    },

    length = function() {
      length(.data)
    },

    empty = function() {
      length(.data) == 0L
    },

    clear = function() {
      .data <<- vector(mode)
    },

    data = function() {
      .data
    }
  )

  object
}
