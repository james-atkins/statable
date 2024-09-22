abort_bug <- function(message, call = caller_env()) {
  cli_abort(c(
    x = "Internal error: {message}.",
    i = "This is a bug in statable. Please file a {.href [GitHub issue](https://github.com/james-atkins/statable/issues)}."
  ))
}

abort_bug_if <- function(condition, message, call = caller_env()) {
  if (condition) {
    abort_bug(message, call = call)
  }
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
