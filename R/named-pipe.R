new_session_named_pipe <- function(session, stata_path) {
  stopifnot(typeof(session) == "environment")
  stopifnot(file.exists(stata_path))

  dir <- tempfile("statable")
  dir.create(dir)

  do_path <- tempfile(pattern = "statable", fileext = ".do", tmpdir = dir)
  log_path <- stata_log_path(do_path)

  do_file <- processx::conn_create_fifo(do_path, write = TRUE)
  process <- new_stata_batch_process(stata_path, do_path)

  while (!file.exists(log_path)) Sys.sleep(.LOG_POLL_SLEEP)
  log_file <- file(log_path, open = "r")

  class(session) <- c("stata_named_pipe", "stata")
  reg.finalizer(session, onexit = TRUE, close_session.stata_named_pipe)

  session$closed <- FALSE
  session$dir <- dir
  session$process <- process
  session$do_file <- do_file
  session$log_file <- log_file

  session
}

#' @export
close_session.stata_named_pipe <- function(session = stata_default_session()) {
  stopifnot(inherits(session, "stata_named_pipe"))

  if (!session$closed) {
    processx::processx_conn_close(session$do_file)

    log_file_path <- summary(session$log_file)$description
    close(session$log_file)
    unlink(log_file_path)

    if (!session$process$is_alive()) {
      session$process$kill()
    }

    unlink(session$dir, recursive = TRUE)

    session$closed <- TRUE
  }
}

#' @export
run_commands.stata_named_pipe <- function(session, user_commands, pre_commands) {
  processx::conn_write(
    session$do_file,
    c(
      pre_commands,
      "capture noisily {",
      START_COMMANDS,
      user_commands,
      END_COMMANDS,
      "}"
    )
  )
  processx::conn_write(session$do_file, "\n")
}
