engine_wrap_knitr <- function(outputs, options) {
  knitr::engine_output(options, out = outputs)
}

# In RStudio notebooks, return evaluate-style (list) outputs rather than a
# character vector.
engine_wrap_rstudio <- function(outputs, options) {
  rstudio_outputs <- stack()

  for (output in outputs) {
    type <- output_type(output)

    if (type == "source") {
      # RStudio sets echo = FALSE but IMHO seems better to echo the command.
      # RStudio does not support "source" outputs so convert them to text.
      prefixes <- c("> ", rep_len(". ", length(output$src) - 1))
      rstudio_outputs$push(paste0(prefixes, output$src, collapse = "\n"))
      rstudio_outputs$push("\n")
    } else if (type == "text") {
      rstudio_outputs$push(paste0(output, collapse = "\n"))
      rstudio_outputs$push("\n\n")
    } else if (type == "error") {
      rstudio_outputs$push(output)
    } else {
      abort_bug("unknown output type {.val {type}}")
    }
  }

  rstudio_outputs$data()
}

engine_stata <- function(options) {
  commands <- options$code

  if (length(commands) == 0L) {
    return(character())
  }

  # https://stackoverflow.com/a/77956152/564817
  in_rstudio <- isTRUE(getOption("rstudio.notebook.executing"))
  wrap <- ifelse(in_rstudio, engine_wrap_rstudio, engine_wrap_knitr)

  # Just return the source code verbatim when eval = FALSE
  if (!options$eval) {
    if (options$echo) {
      outputs <- list(
        structure(list(src = commands), class = "source")
      )
    } else {
      outputs <- list()
    }
    return(wrap(outputs, options))
  }

  session <- options$session
  if (is.null(session)) {
    session <- stata_default_session()
  }

  outputs <- stata_evaluate(commands, session = session)
  wrap(outputs, options)
}
