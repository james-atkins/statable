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
#' @param session The Stata session to run the commands in
#' @param env The R environment in which to look for data frames to transfer to Stata
#'
#' @export
stata_run <- function(commands, session = stata_default_session(), env = caller_env()) {
  # Parse commands for any global macros
  globals <- .extract_globals(commands, env, session$dir)
  set_globals <- sprintf('global %s "%s"', names(globals), globals)

  # Send commands to Stata
  run_commands(session, commands, set_globals)

  # Extract output from log file
  prev_line <- NULL
  found_start <- FALSE
  found_end <- FALSE

  while (!found_end) {
    if (!session$process$is_alive()) {
      cli_abort(c(
        x = "Stata process has died.",
        i = "Please file a {.href [GitHub issue](https://github.com/james-atkins/statable/issues)} and upload this log file: {.file {session$log_path}}."
      ))
    }

    lines <- readLines(session$log_file)

    for (line in lines) {
      if (!found_start) {
        found_start <- grepl(START_COMMANDS, line, fixed = TRUE)
        next
      }

      found_end <- grepl(END_COMMANDS, line, fixed = TRUE)
      if (found_end) {
        break
      }

      # Any errors? This is currently implemented by searching for Stata's return code.
      # It could be made more robust in the future by checking whether _rc != 0.
      if (grepl("^r\\([0-9]+\\);", line)) {
        if (!is.null(prev_line)) {
          stop(prev_line, call. = TRUE)
        } else {
          stop("Stata error")
        }
      }

      cat(line, sep = "\n")
      prev_line <- line
    }
  }
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
