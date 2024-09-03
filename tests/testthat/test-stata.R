test_that("stata works", {
  skip_if_no_stata()

  session <- stata_start_session()
  on.exit(stata_close_session(session))

  stata_run("sysuse auto", session = session)
  auto <- stata_data_out(session = session)

  expect_equal(mean(auto$price), 6165.2568)
})
