split_lines <- function(x) {
  unlist(strsplit(x, "\r?\n"), recursive = FALSE, use.names = FALSE)
}

line_iterator <- function(x) {
  UseMethod("line_iterator", x)
}

#' @export
line_iterator.character <- function(x) {
  .idx <- 0L
  .lines <- split_lines(x)

  object <- list(
    iterate = function() {
      .idx <<- .idx + 1L
      .lines[.idx]
    },

    peek = function() {
      .lines[.idx + 1L]
    }
  )

  object
}

#' @export
line_iterator.connection <- function(x) {
  .idx <- 0L
  .con <- x

  if (!isOpen(.con)) {
    open(.con, open = "rt")
  }
  .lines <- readLines(.con, warn = FALSE)

  read_more_if_needed <- function() {
    if (.idx >= length(.lines)) {
      .lines <<- readLines(.con)
      .idx <<- 0L
    }
  }

  object <- list(
    iterate = function() {
      read_more_if_needed()
      .idx <<- .idx + 1L
      .lines[.idx]
    },

    peek = function() {
      read_more_if_needed()
      .lines[.idx + 1L]
    }
  )

  object
}

# callback_input is called when the complete Stata command has been parsed. It is
# called with this command as a character vector, with each line an element.
# callback_output is called for every line of the output.
# callback_error is called with the integer error code and an optional message.
parse_log <- function(commands, log, is_alive, callback_input, callback_output, callback_error) {
  commands <- split_lines(commands)
  log_iter <- line_iterator(log)

  found_start <- FALSE
  found_end <- FALSE

  # current command being read, possibly across multiple lines
  current_input <- stack()

  reading_input <- function() {
    !current_input$empty()
  }

  push_input <- function(line, type = c("start", "continued", "loop")) {
    type <- rlang::arg_match(type)
    current_input$push(structure(line, type = type))
  }

  current_output <- stack()

  push_output <- function(line, next_line_is_overflow = FALSE) {
    if (next_line_is_overflow) {
      current_output$push(line)
      return()
    }

    if (current_output$empty()) {
      callback_output(line)
      return()
    }

    current_output$push(line)
    output <- paste0(current_output$data(), collapse = "")
    current_output$clear()
    callback_output(output)
  }

  while (!found_end) {
    if (!is_alive()) {
      abort_bug("Stata process has died")
    }

    line <- log_iter$iterate()
    if (is.na(line)) {
      Sys.sleep(1/10)  # TODO: organise polling times
      next
    }

    if (!found_start) {
      found_start <- grepl(START_COMMANDS, line, fixed = TRUE)
      next
    }

    found_end <- grepl(END_COMMANDS, line, fixed = TRUE)
    if (found_end) {
      # Make sure to process the final command if it has no output.
      if (reading_input()) {
        command <- match_inputs_with_commands(commands, current_input$data())
        callback_input(command)

        commands <- commands[-(1:length(command))]
        current_input$clear()
      }

      break
    }

    # This needs to be after check for found_end else there is a risk of looping
    # forever.
    next_line <- log_iter$peek()
    if (is.na(next_line)) {
      Sys.sleep(1/10)  # TODO: organise polling times
      next
    }

    if (reading_input()) {
      # The previous line was input. Is the current line still input?

      # If the line starts with an angle bracket (new line) or number dot
      # (loop) then the input is continued.
      if ((m <- regexpr("^> ", line)) != -1) {
        command <- regmatches(line, m, invert = TRUE)[[1]][[2]]
        push_input(command, "continued")
        next
      }
      if ((m <- regexpr("^\\s*\\d*\\. ", line)) != -1) {
        command <- regmatches(line, m, invert = TRUE)[[1]][[2]]
        push_input(command, "loop")
        next
      }

      # No new line or loop so assume that this is output.
      command <- match_inputs_with_commands(commands, current_input$data())
      callback_input(command)

      commands <- commands[-(1:length(command))]
      current_input$clear()
    }

    # Reading output or input?
    if ((m <- regexpr("^\\.\\s*", line)) != -1) {
      command <- regmatches(line, m, invert = TRUE)[[1]][[2]]
      push_input(command)
      next
    }

    # Any errors? This is currently implemented by searching for Stata's return code.
    # It could be made more robust in the future by checking whether _rc != 0.
    m <- regexec("^r\\(([0-9]+)\\);", next_line)
    if (any(unlist(m, recursive = FALSE, use.names = FALSE) != -1)) {
      code <- as.integer(regmatches(next_line, m)[[1]][[2]])
      callback_error(code, line)
      return()
    }

    # We are reading output.

    # Is this line an overflowed line?
    if ((m <- regexpr("^> ", line)) != -1) {
      output <- regmatches(line, m, invert = TRUE)[[1]][[2]]
    } else {
      output <- line
    }

    # Does this line overflow over to the next line?
    if (grepl("^> ", next_line)) {
      push_output(output, next_line_is_overflow = TRUE)
    } else {
      push_output(output)
    }
  }

  if (length(commands) != 0L) {
    abort_bug("commands remaining")
  }
}

# Try to match the inputs to Stata from the log file with the users' commands.
#
# Stata commands can be split across multiple lines by the user, or the inputs
# in the log file can be split across multiple lines by Stata if the command is
# too long.
match_inputs_with_commands <- function(user_commands, inputs) {
  inputs <- structure(
    unlist(inputs),
    type = map_chr(inputs, attr, "type")
  )

  # Stata seems to convert tabs to 8 spaces, so convert back to tabs
  # This may cause problems if users themselves enter 8 spaces...
  inputs <- gsub("        ", "\t", inputs, fixed = TRUE)
  types = attr(inputs, "type", exact = TRUE)

  ran_user_commands <- stack()

  for (user_command in user_commands) {
    if (length(inputs) == 0L) {
      # Read all the input from Stata
      break
    }

    current_command <- inputs[1]

    if (user_command == current_command) {
      ran_user_commands$push(user_command)
      inputs <- inputs[-1]
      types <- types[-1]
      next
    }

    if (!startsWith(user_command, current_command)) {
      abort_bug("current command has invalid prefix")
    }

    i <- 2
    while (i <= length(inputs) && isTRUE(types[i] == "continued")) {
      # Was the command was so long that Stata continued it to the next line?
      maybe_command <- paste0(current_command, inputs[i])

      if (user_command == maybe_command) {
        ran_user_commands$push(user_command)
        inputs <- inputs[-(1:i)]
        types <- types[-(1:i)]
        break
      }

      if (startsWith(user_command, maybe_command)) {
        current_command <- maybe_command
        i <- i + 1
        next
      }

      abort_bug("continued command has invalid prefix")
    }
  }

  unlist(ran_user_commands$data(), recursive = FALSE, use.names = FALSE)
}
