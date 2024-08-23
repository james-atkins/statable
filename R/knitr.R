engine_stata <- function(options) {
  session <- options$session
  if (is.null(session)) {
    session <- stata_default_session()
  }

  commands <- options$code
  if (length(commands) > 0) {
    out <- utils::capture.output(
      stata_run(commands, session = session, env = knitr::knit_global())
    )
  } else {
    out <- character(0)
  }

  code <- paste(options$code, collapse = "\n")
  knitr::engine_output(options, code, out)
}
