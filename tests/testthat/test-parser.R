always_alive <- function() {
  TRUE
}

read_parser_test_file <- function(file_name) {
  f <- file(file_name)
  on.exit(close(f))

  lines <- readLines(f)

  input_idx <- which(lines == "INPUT")
  log_idx <- which(lines == "LOG")
  output_idx <- which(lines == "OUTPUT")
  end_idx <- which(lines == "END")

  if (length(input_idx) != 1 || length(log_idx) != 1 || length(output_idx) != 1 || length(end_idx) != 1) {
    stop("invalid test file: invalid sections.")
  }

  if (!all(lines[c(input_idx, log_idx, output_idx)+1] == "")) {
    stop("invalid test file: should be blank after section header.")
  }

  if (!all(lines[c(log_idx, output_idx, end_idx)-1] == "")) {
    stop("invalid test file: should be blank before LOG, OUTPUT and END section headers.")
  }

  input <- lines[(input_idx+2):(log_idx-2)]
  log <- lines[(log_idx+2):(output_idx-2)]
  output <- lines[(output_idx+2):(end_idx-2)]

  list(input = input, log = log, output = output)
}

test_that("parser test files run", {
  test_files <- list.files(file.path(test_path(), "parser"), pattern = "\\.txt$", full.names = TRUE)

  for (test_file in test_files) {
    test <- read_parser_test_file(test_file)

    code <- 0L
    output <- stack("character")

    callback_input <- function(.input) {
      input <<- .input
    }

    callback_output <- function(.output) {
      output$push(.output)
    }

    callback_error <- function(.code, message = NULL) {
      code <<- .code
    }

    parse_log(test$input, test$log, always_alive, callback_input, callback_output, callback_error)
    expect_equal(output$data(), test$output)
    expect_equal(code, 0L)
  }
})

test_that("matching simple input works", {
  expect_equal(
    match_inputs_with_commands(
      "sysuse auto",
      list(structure("sysuse auto", type = "start"))
    ),
    "sysuse auto"
  )
})

test_that("additional commands are ignored", {
  expect_equal(
    match_inputs_with_commands(
      c("sysuse auto", "clear"),
      list(structure("sysuse auto", type = "start"))
    ),
    "sysuse auto"
  )
})

test_that("matching commands with /// works", {
  expect_equal(
    match_inputs_with_commands(
      c("sysuse ///", "  auto"),
      list(
        structure("sysuse ///", type = "start"),
        structure("  auto", type = "continued")
      )
    ),
    c("sysuse ///", "  auto")
  )

  expect_equal(
    match_inputs_with_commands(
      c("sysuse ///", "  auto", "clear"),
      list(
        structure("sysuse ///", type = "start"),
        structure("  auto", type = "continued")
      )
    ),
    c("sysuse ///", "  auto")
  )
})

test_that("matching commands with automatic line breaks works", {
  expect_equal(
    match_inputs_with_commands(
      'display "hello world"',
      list(
        structure('display "hello', type = "start"),
        structure(' world"', type = "continued")
      )
    ),
    'display "hello world"'
  )

  expect_equal(
    match_inputs_with_commands(
      c('display "hello world"'),
      list(
        structure('display "hello ', type = "start"),
        structure('world"', type = "continued")
      )
    ),
    'display "hello world"'
  )

  expect_equal(
    match_inputs_with_commands(
      c('display "hello world"'),
      list(
        structure('display "hello', type = "start"),
        structure(' wor', type = "continued"),
        structure('ld"', type = "continued")
      )
    ),
    'display "hello world"'
  )

  expect_error(
    match_inputs_with_commands(
      c('display "hello world"'),
      list(
        structure('display "hello', type = "start"),
        structure(' worl"', type = "continued")
      )
    )
  )
})

test_that("matching commands with /// and automatic line breaks works", {
  expect_equal(
    match_inputs_with_commands(
      c('display ///', '  "hello world"'),
      list(
        structure('display ///', type = "start"),
        structure('  "hello', type = "continued"),
        structure(' world"', type = "continued")
      )
    ),
    c('display ///', '  "hello world"')
  )
})
