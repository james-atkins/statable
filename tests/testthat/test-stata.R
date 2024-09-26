test_that("stata works", {
  skip_if_no_stata()

  suppressMessages({
    session <- stata_start_session()
    on.exit(stata_close_session(session))

    stata_run("sysuse auto", session = session)
    auto <- stata_data_out(session = session)

    expect_equal(mean(auto$price), 6165.2568)

    expect_error(stata_run("this_is_an_invalid_command, oops", session = session), class = "statable_error_stata")
  })
})

test_that("staggered output is read correctly", {
  session <- suppressMessages(stata_start_session())
  on.exit(stata_close_session(session))

  program <- r"{
program define test
  display "first"
  sleep 100
  display "second"
  sleep 500
  display "third"
end
}"
  stata_evaluate(program, session = session)
  output <- stata_evaluate("test", session = session)

  expect_equal(output[[2]], c("first", "second", "third"))
})
