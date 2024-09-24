.onLoad <- function(libname, pkgname) {
  knitr::knit_engines$set(stata = engine_stata)
}

#' Get or set the path to the Stata executable
#'
#' @param path The path to the Stata executable
#'
#' @export
stata_path <- function(path = rlang::missing_arg()) {
  if (rlang::is_missing(path)) {
    path <- getOption("statable.path")

    if (is.null(path)) {
      # Try and look for Stata
      path <- find_stata()

      if (length(path) == 0) {
        cli_abort(c(
          x = "Cannot find a Stata executable.",
          i = "Use {.fun statable::stata_path} to set the path to a Stata executable."
        ))
      }
    }

    path
  } else {
    if (is.null(path)) {
      # Clear the options
      options(statable.path = NULL)
      return()
    }

    if (length(path) != 1) {
      cli_abort(c(x = "{.var path} must have length 1, not length {length(path)}."))
    }

    if (!is.character(path)) {
      cli_abort(c(x = "{.var path} must be a string, not {.val {typeof(path)}}."))
    }

    if (!file.exists(path)) {
      cli_abort(c(x = "{.var path} must exist: cannot find {.file {path}}."))
    }

    options(statable.path = path)
    cli_inform(c(v = "Setting default Stata path"))
  }
}

#' Run a Stata command
#'
#' @param commands A vector of Stata commands to run
#' @inheritParams rlang::args_dots_empty
#' @param session The Stata session to run the commands in
#' @param env The R environment in which to look for data frames to transfer to Stata
#'
#' @export
stata_run <- function(commands, ..., session = stata_default_session(), env = parent.frame()) {
  check_dots_empty()

  # Parse commands for any global macros
  globals <- .extract_globals(commands, env, session$dir)
  set_globals <- sprintf('global %s "%s"', names(globals), globals)

  # Send commands to Stata
  run_commands(session, commands, set_globals)

  callback_input <- function(input) {
    cli_code(input, language = "stata")
  }

  callback_output <- function(line) {
    cli_verbatim(line)
  }

  callback_error <- function(code, message) {
    cli_abort("Stata error {code}: {message}.", call = NULL)
  }

  parse_log(
    commands,
    session$log_file,
    session$process$is_alive,
    callback_input,
    callback_output,
    callback_error
  )
}

run_commands <- function(session, user_commands, pre_commands) {
  UseMethod("run_commands", session)
}

#' Start a new session of Stata running in batch mode
#'
#' @param path The path to the Stata executable
#'
#' @export
stata_start_session <- function(path = stata_path()) {
  session <- new.env(parent = emptyenv())
  new_session(session, path)
}

#' Close a Stata session
#'
#' @param session The Stata session to close
#'
#' @export
stata_close_session <- function(session = stata_default_session()) {
  stopifnot(inherits(session, "stata"))
  close_session(session)
}

close_session <- function(session) {
  UseMethod("close_session", session)
}

#' Get the default Stata session
#'
#' Returns the default Stata session. It will be started if it is not already running.
#'
#' @export
stata_default_session <- function() {
  if (!inherits(.stata, "stata") || .stata$closed) {
    new_session(.stata, stata_path())
  }

  .stata
}

.stata <- new.env(parent = emptyenv())

new_session <- function(session, stata_path) {
  if (!is.character(stata_path)) {
    cli_abort(c(x = "{.var stata_path} must be a string, not {.val {typeof(stata_path)}}."))
  }

  if (length(stata_path) == 0) {
    cli_abort(c(x = "{.var stata_path} must not be empty."))
  }

  stata_path <- stata_path[1]

  if (!file.exists(stata_path)) {
    cli_abort(c(x = "{.var stata_path} must exist: cannot find {.file {stata_path}}."))
  }

  if (.Platform$OS.type == "unix") {
    new_session_named_pipe(session, stata_path)
  } else if (.Platform$OS.type == "windows") {
    new_session_polling(session, stata_path)
  } else {
    cli_abort(c(
      x = "Stata is not supported on this platform.",
      i = "Run statable on Linux, Windows or macOS."
    ))
  }

  cli_inform(c(i = "Started Stata session: {.path {stata_path}}"))
  session
}
