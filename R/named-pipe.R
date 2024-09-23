new_session_named_pipe <- function(session, stata_path) {
  stopifnot(typeof(session) == "environment")
  stopifnot(file.exists(stata_path))

  dir <- tempfile("statable")
  dir.create(dir)

  pipe_path <- tempfile(pattern = "statable", fileext = ".do", tmpdir = dir)
  log_path <- stata_log_path(pipe_path)

  pipe <- processx::conn_create_fifo(pipe_path, write = TRUE)
  process <- new_stata_batch_process(stata_path, pipe_path)

  while (!file.exists(log_path)) Sys.sleep(.LOG_POLL_SLEEP)
  log_file <- file(log_path, open = "rt")

  class(session) <- c("stata_named_pipe", "stata")
  reg.finalizer(session, onexit = TRUE, close_session.stata_named_pipe)

  session$closed <- FALSE
  session$dir <- dir
  session$process <- process
  session$pipe <- pipe
  session$log_file <- log_file
  session$log_path <- log_path

  session
}

#' @export
close_session.stata_named_pipe <- function(session = stata_default_session()) {
  stopifnot(inherits(session, "stata_named_pipe"))

  if (!session$closed) {
    processx::processx_conn_close(session$pipe)

    close(session$log_file)
    unlink(session$log_path)

    if (!session$process$is_alive()) {
      session$process$kill()
    }

    unlink(session$dir, recursive = TRUE)

    session$closed <- TRUE
  }
}

#' @export
run_commands.stata_named_pipe <- function(session, user_commands, pre_commands) {
  do_file_path <- tempfile("input", fileext = ".do", tmpdir = session$dir)
  do_file <- file(do_file_path, open = "wt")

  writeLines(
    c(
      pre_commands,
      "capture noisily {",
      START_COMMANDS,
      user_commands,
      END_COMMANDS,
      "}",
      # Clean up after ourselves
      sprintf('rm "%s"', do_file_path)
    ),
    do_file
  )
  close(do_file)

  processx::conn_write(
    session$pipe,
    sprintf('do "%s"\n', do_file_path)
  )
}
