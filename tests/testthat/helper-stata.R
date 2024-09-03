skip_if_no_stata <- function() {
  tryCatch(
    statable::stata_path(),
    error = function(cnd) {
      skip("Stata is not installed")
    }
  )
}
