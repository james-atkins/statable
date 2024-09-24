new_session_polling <- function(session, stata_path) {
  stopifnot(typeof(session) == "environment")
  stopifnot(file.exists(stata_path))

  dir <- tempfile("statable")
  dir.create(dir)

  run_file_path <- file.path(dir, "run")
  file.create(run_file_path)

  # Create the initial do file that starts polling for input files
  do_file_path <- tempfile(pattern = "statable", fileext = ".do", tmpdir = dir)
  file.copy(system.file("statable.do", package = "statable"), do_file_path)

  log_path <- stata_log_path(do_file_path)

  input_file_path <- tempfile("input", fileext = ".do", tmpdir = dir)

  env <- c(STATABLE_RUN = run_file_path, STATABLE_INPUT = input_file_path)
  process <- new_stata_batch_process(stata_path, do_file_path, env)

  while (!file.exists(log_path)) Sys.sleep(.LOG_POLL_SLEEP)
  log_file <- file(log_path, open = "rt")

  class(session) <- c("stata_polling", "stata")
  reg.finalizer(session, onexit = TRUE, close_session.stata_polling)

  session$closed <- FALSE
  session$process <- process
  session$dir <- dir
  session$run_file_path <- run_file_path
  session$log_file <- log_file
  session$log_path <- log_path
  session$input_file_path <- input_file_path

  session
}

#' @export
close_session.stata_polling <- function(session) {
  if (!session$closed) {
    unlink(session$run_file_path)

    close(session$log_file)
    unlink(session$log_path)

    # TODO: wait for Stata to gracefully exit

    if (!session$process$is_alive()) {
      session$process$kill()
    }

    unlink(session$dir, recursive = TRUE)

    session$closed <- TRUE
  }
}

#' @export
run_commands.stata_polling <- function(session, commands, pre_commands) {
  input_file <- file(session$input_file_path, open = "wt")

  next_input_file_path <- tempfile("input", fileext = ".do", tmpdir = session$dir)
  session$input_file_path <- next_input_file_path

  writeLines(
    c(
      pre_commands,
      "capture noisily {",
      START_COMMANDS,
      commands,
      END_COMMANDS,
      "}",
      # Clean up current input file and set path of next input file
      r"(rm "$STATABLE_INPUT")",
      sprintf('global STATABLE_INPUT "%s"', next_input_file_path)
    ),
    input_file
  )
  close(input_file)
}
