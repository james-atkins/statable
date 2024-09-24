#' Evaluate Stata commands and return details of their evaluation
#'
#' `stata_evaluate` similar to the [evaluate::evaluate()] function. It returns
#' an `evaluation` object that consists of the Stata commands run and the
#' corresponding output and errors.
#'
#' @param commands A vector of Stata commands to run
#' @inheritParams rlang::args_dots_empty
#' @param session The Stata session to run the commands in
#' @param env The R environment in which to look for data frames to transfer to Stata
#'
#' @export
stata_evaluate <- function(commands, ..., session = stata_default_session(), env = parent.frame()) {
  check_dots_empty()

  commands <- clean_commands(commands)
  pre_commands <- make_pre_commands(session, commands, env)

  result <- stack()
  output <- stack()

  flush_output <- function() {
    if (!output$empty()) {
      result$push(as.character(output$data()))
      output$clear()
    }
  }

  callback_input <- function(input) {
    flush_output()
    result$push(structure(list(src = input), class = "source"))
  }

  callback_output <- function(line) {
    output$push(line)
  }

  callback_error <- function(code, message) {
    flush_output()
    error <- catch_cnd(stop_stata(code, message))
    result$push(error)
  }

  run_commands(session, commands, pre_commands)

  parse_log(
    commands,
    session$log_file,
    session$process$is_alive,
    callback_input,
    callback_output,
    callback_error
  )
  flush_output()

  new_evaluation(result$data())
}

new_evaluation <- function(x) {
  structure(x, class = c("stata_evaluation", "list"))
}

is_evaluation <- function(x) {
  inherits(x, "stata_evaluation")
}

#' @export
print.stata_evaluation <- function(x, ...) {
  cat_line("<stata_evaluation>")
  for (output in x) {
    type <- output_type(output)
    if (type == "source") {
      cat_line("Source code: ")
      cat_line(indent(output$src))
    } else if (type == "text") {
      cat_line("Text output: ")
      cat_line(indent(output))
    } else if (type == "error") {
      cat_line("Error: ")
      cat_line(indent(output))
    } else {
      abort_bug("unknown output type {.val {type}}")
    }
  }

  invisible(x)
}

output_type <- function(x) {
  if (is.character(x)) {
    "text"
  } else if (is_error(x)) {
    "error"
  } else if (is_source(x)) {
    "source"
  } else {
    class(x)[[1]]
  }
}

is_source <- function(x) {
  inherits(x, "source")
}
