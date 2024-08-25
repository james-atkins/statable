find_stata <- function() {
  os <- Sys.info()["sysname"]

  if (os == "Windows") {
    find_stata_windows()
  } else if (os == "Darwin") {
    find_stata_mac()
  } else if (os == "Linux") {
    find_stata_linux()
  } else {
    cli_abort(c(
      x = "{os} is not supported by Stata and so {.pkg statable} cannot be used.",
      i = "Use {.pkg statable} on Linux, Windows or macOS."
    ))
  }
}

STATA_VERSIONS <- 18:14

find_stata_linux <- function() {
  stata_exes <- c("stata-mp", "stata-se", "stata", "stata-ic", "stata-sm")

  # First check $PATH
  stata_exe_path <- Sys.which(stata_exes)
  stata_exe_path <- unname(stata_exe_path[stata_exe_path != ""])

  # Now look in standard locations
  stata_dirs <- c(
    paste0("/usr/local/stata", STATA_VERSIONS),
    "/opt/stata", "/software/stata", "/software/Stata",
    paste0("/software/Stata/stata", STATA_VERSIONS)
  )

  stata_exe_other <- as.vector(t(outer(stata_dirs, stata_exes, file.path)))
  stata_exe_other <- stata_exe_other[file.exists(stata_exe_other)]

  unique(c(stata_exe_path, stata_exe_other))
}

find_stata_windows <- function() {
  stata_dirs <- c(
    sprintf("C:/Program Files/Stata%i", STATA_VERSIONS),
    sprintf("C:/Program Files (x86)/Stata%i", STATA_VERSIONS)
  )

  stata_exes <- paste(
    c(
      "StataMP-64", "StataSE-64", "Stata-64", "StataBE-64", "StataIC-64",
      "StataMP", "StataSE", "Stata", "StataBE", "StataIC"
    ),
    "exe",
    sep = "."
  )

  exe_paths <- as.vector(t(outer(stata_dirs, stata_exes, file.path)))
  exe_paths <- exe_paths[file.exists(exe_paths)]

  exe_paths
}

find_stata_mac <- function() {
  app_names <- paste0(
    c("StataMP", "StataSE", "Stata", "StataIC", "StataBE"),
    "app",
    sep = "."
  )

  app_paths <- c(
    file.path("/Applications/Stata", app_names),
    file.path("/Applications", app_names)
  )
  app_paths <- app_paths[dir.exists(app_paths)]

  if (length(app_paths) == 0) {
    return(character(0))
  }

  find_stata_mac_from_app(app_paths)
}

find_stata_mac_from_app <- function(app_paths) {
  stata_exes <- c("stata-mp", "stata-se", "stata", "stata-ic", "stata-sm")

  exe_paths <- as.vector(t(outer(
    file.path(app_paths, "Contents/MacOS"),
    stata_exes,
    file.path
  )))

  exe_paths <- exe_paths[file.exists(exe_paths)]
  exe_paths
}
