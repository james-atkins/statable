#' Transfer data from R to Stata
#'
#' @param data_frame The data frame to be loaded in Stata
#' @param clear Whether to replace the data set already in Stata
#' @param session The Stata session to load the data set in
#'
#' @export
stata_data_in <- function(data_frame, clear = FALSE, session = stata_default_session()) {
  stopifnot(inherits(data_frame, "data.frame"))

  if (clear) {
    stata_run('use "$R_data_frame", clear', session = session)
  } else {
    stata_run('use "$R_data_frame"', session = session)
  }
}

#' Transfer data from Stata to R
#'
#' @param session The Stata session to read the data set from
#'
#' @return A data.frame containing the current Stata data set
#'
#' @export
stata_data_out <- function(session = stata_default_session()) {
  dta_path <- tempfile("stata", fileext = ".dta", tmpdir = session$dir)

  stata_run(sprintf('save "%s"', dta_path), session = session)

  dta <- haven::read_dta(dta_path)
  unlink(dta_path)
  dta
}

# Search commands for globals of the form $R_variable_name.
.extract_globals <- function(commands, data, dir) {
  stopifnot(typeof(commands) == "character")

  # Get the R data frames referenced by the Stata globals.
  # Note dots are valid in R variable names but are not valid in Stata.
  # Global macro names can be up to 32 characters long.
  matches <- regexec("\\$(R_([[:alpha:]][[:alnum:]_]+))", commands)
  matched_data <- regmatches(commands, matches)
  global_names <- sapply(matched_data, `[`, 2)
  data_frame_names <- sapply(matched_data, `[`, 3)

  global_names <- global_names[!is.na(global_names)]
  data_frame_names <- data_frame_names[!is.na(data_frame_names)]
  stopifnot(length(global_names) == length(data_frame_names))

  if (any(nchar(global_names) > GLOBAL_MAX_LENGTH)) {
    stop("Stata global macro names cannot be longer than 32 characters")
  }

  missing_data_frames <- data_frame_names[!env_has(data, data_frame_names, inherit = TRUE)]
  if (length(missing_data_frames) > 0) {
    stop(sprintf("Cannot find data frames: %s", paste0(missing_data_frames, collapse = ", ")))
  }

  mapply(
    function(global_name, data_frame_name) {
      dta_path <- tempfile(data_frame_name, fileext = ".dta", tmpdir = dir)
      haven::write_dta(env_get(data, data_frame_name, inherit = TRUE), dta_path)
      dta_path
    },
    global_names, data_frame_names,
    USE.NAMES = TRUE
  )
}
