.LOG_POLL_SLEEP <- 1/50

GLOBAL_MAX_LENGTH <- 32
START_COMMANDS <- "/* !!! statable: start of commands !!! */"
END_COMMANDS <- "/* !!! statable: end of commands !!! */"

new_stata_batch_process <- function(stata_path, do_path, env = character()) {
  env <- c(Sys.getenv(), env)
  if (.Platform$OS.type == "unix") {
    processx::process$new(stata_path, c("-b", "do", do_path), env = env)
  } else if (.Platform$OS.type == "windows") {
    processx::process$new(stata_path, c("/e", "do", do_path), env = env)
  } else {
    stop("unknown platform")
  }
}

stata_log_path <- function(do_path, working_directory = getwd()) {
  do_name <- basename(do_path)
  stopifnot(tools::file_ext(do_name) == "do")

  # The log file is created in the current working directory, with the same
  # file name as the do file with the file extension changed from .do to .log.
  log_name <- paste0(tools::file_path_sans_ext(do_name), ".log")

  file.path(working_directory, log_name)
}
