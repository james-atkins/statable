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
