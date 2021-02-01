## Are we running on a Unix-based operating system?
is_unix <- function() {
  identical(tolower(.Platform$OS.type), "unix")
}

## Are we running on Linux?
is_linux <- function() {
  identical(tolower(Sys.info()[["sysname"]]), "linux")
}

## Are we running on OS X?
is_os_x <- function() {
  identical(tolower(Sys.info()[["sysname"]]), "darwin")
}


## Given a string, indent every line by some number of spaces.
## The exception is to not add spaces after a trailing \n.
indent <- function(str, indent = 0) {
  gsub("(^|\\n)(?!$)",
       paste0("\\1", paste(rep(" ", indent), collapse = "")),
       str,
       perl = TRUE
  )
}

## Return a string of random letters and numbers, with an optional prefix.
random_name <- function(prefix = NULL, length = 6) {
  chars <- c(letters, 0:9)
  rand_str <- paste(sample(chars, length), collapse = "")
  paste(c(prefix, rand_str), collapse = "_")
}

## Run code, but with warnings as errors.
with_warnings_as_errors <- function(code) {
  withr::with_options(list(warn = 2), code)
}

## R's usual behaviour is to "recycle" shorter vectors as often as needs be
## (perhaps fractionally) until they match the length of the longest vector.
## This is error prone so this function checks that vectors all have the same length
## or a length equal to one.
check_vector_lengths <- function (...) {
  l <- purrr::map_int(list(...), length)
  if (!all(l == 1 | l == max(l))) {
    stop("Arguments must have lengths equal to each other, or one.")
  }
}

## Echo a connection to stdout, one line at a time.
connection_to_stdout <- function(con) {
  if (!isOpen(con)) {
    open(con, "r")
  }

  while(length(txt <- readLines(con, n = 1))) {
    cat(txt, fill = TRUE)
  }
}
