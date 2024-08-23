test_that("can extract globals", {
  env <- rlang::new_environment()

  globals <- .extract_globals("describe", env, tempdir())
  expect_named(globals)
  expect_length(globals, 0)

  data("mtcars", envir = env)

  globals <- .extract_globals("use $R_mtcars", env, tempdir())
  expect_named(globals)
  expect_length(globals, 1)

  expect_true(file.exists(globals["R_mtcars"]))
  expect_true(is.na(globals["R_mtcars2"]))

  env$mtcars2 <- env$mtcars
  globals <- .extract_globals(c("use $R_mtcars", "use $R_mtcars2"), env, tempdir())
  expect_named(globals)
  expect_length(globals, 2)
  expect_true(file.exists(globals["R_mtcars"]))
  expect_true(file.exists(globals["R_mtcars2"]))
})

test_that("can extract globals in parent environments", {
  env_parent <- rlang::new_environment()
  env <- rlang::new_environment(parent = env_parent)

  data("mtcars", envir = env_parent)

  globals <- .extract_globals("use $R_mtcars", env, tempdir())
  expect_named(globals)
  expect_length(globals, 1)

  expect_true(file.exists(globals["R_mtcars"]))
})

test_that("error for invalid global names", {
  env <- environment()
  data("mtcars", envir = env)
  this_is_a_very_long_variable_name <- mtcars

  expect_error(.extract_globals("use $R_this_is_a_very_long_variable_name", env, tempdir()))
})
