# stata_run <- function(commands, data = parent.frame(), stata = stata_default()) {
#   data <- Filter(function(x) inherits(eval(x), "data.frame"), as.list(data))
#   globals <- .extract_globals(commands, data)
#   .stata_run(commands, globals, stata)
# }

# Search commands for globals of the form $R_variable_name.
.extract_globals <- function(commands, data, dir) {
  stopifnot(typeof(commands) == "character")
  data = as.list(data)

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

  # Error if Stata references non-existent R data frames
  missing_data_frames <- data_frame_names[!data_frame_names %in% names(data)]
  if (length(missing_data_frames) > 0) {
    stop(sprintf("Cannot find data frames: %s", paste0(missing_data_frames, collapse = ", ")))
  }

  mapply(
    function(global_name, data_frame_name) {
      dta_path <- tempfile(data_frame_name, fileext = ".dta", tmpdir = dir)
      haven::write_dta(data[[data_frame_name]], dta_path)
      dta_path
    },
    global_names, data_frame_names,
    USE.NAMES = TRUE
  )
}
