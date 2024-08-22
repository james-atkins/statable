#' Get or set the path to the Stata executable
#'
#' @param path The path to the Stata executable
#'
#' @export
stata_path <- function(path = NULL) {
  if (is.null(path)) {
    path <- getOption("statable.path")
    if (!is.null(path)) {
      path
    } else {
      find_stata()
    }
  } else {
    options(statable.path = path)
  }
}

#' Run a Stata command
#'
#' @param commands A vector of Stata commands to run
#' @param session The Stata session to run the commands in
#'
#' @export
stata_run <- function(commands, session = stata_default_session()) {
  # Parse commands for any global macros
  globals <- .extract_globals(commands, parent.frame(), session$dir)
  set_globals <- sprintf('global %s "%s"', names(globals), globals)

  # Send commands to Stata
  run_commands(session, commands, set_globals)

  # Extract output from log file
  prev_line <- NULL
  found_start <- FALSE
  found_end <- FALSE

  while (!found_end) {
    if (!session$process$is_alive()) {
      stop("Stata process has died")
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
#' @param stata_path The path to the Stata executable
#'
#' @export
stata_start_session <- function(stata_path = stata_path()) {
  session <- new.env(parent = emptyenv())
  new_session(session, stata_path)
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
  if (!inherits(.stata, "stata")) {
    new_session(.stata, stata_path())
  }

  .stata
}

.stata <- new.env(parent = emptyenv())

new_session <- function(session, stata_path) {
  if (.Platform$OS.type == "unix") {
    new_session_named_pipe(session, stata_path)
  } else if (.Platform$OS.type == "windows") {
    new_session_polling(session, stata_path)
  } else {
    stop("Stata is not supported on this platform")
  }
}
