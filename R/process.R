### Collection of useful functions to help with running processes in R.


pipe_with_pid <- function(command, args) {

  if (!is_unix()) {
    stop("pipe_with_pid only works on Unix-based operating systems.")
  }

  ## R's pipe function runs a command in shell.
  ## In order to get the PID of the command, we first get the PID of the shell ran by R and then use
  ## pgrep to find the PID of command (the shell's only child process).
  ## We get the PID of the shell by echoing the PID on the first line.

  command <- paste(c("echo $$;", shQuote(command), args, "2>&1"), collapse = " ")
  con <- pipe(command, open = "r")
  shell_pid <- readLines(con, n = 1)

  ## Once we have the PID of the shell, we can then use pgrep to get the PID of our
  ## command (the shell's child).
  command_pid <- pgrep_children(shell_pid)
  assert_that(length(command_pid) <= 1)
  if (length(command_pid) == 0) {
    command_pid <- NA
  }

  structure(
    con,
    class = c("pipe_with_pid", class(con)),
    pid = command_pid
  )
}

close.pipe_with_pid <- function(con, kill = FALSE, ...) {
  if (kill) {
    pid <- pipe_pid(con)
    if (!is.na(pid)) {
      tools::pskill(pipe_pid(con), tools::SIGINT)
    }
  }
  NextMethod(con)
}

pipe_pid <- function(connection) {
  attr(connection, "pid")
}

### ---------- Code for most of below is based on processx (https://github.com/MangoTheCat/processx) ----------
### When that package is available on CRAN, most of this can be removed.

## pgrep does not include itself in the output, but on some systems
## (e.g. Linux) it does include its ancestors, which is a problem for us
## Here we make sure that ancestors are excluded on Linux

pgrep_children <- function(pid) {
  if (is_linux()) {
    cmd <- pgrep_children_linux(pid)

  } else {
    cmd <- pgrep_children_unix(pid)
  }

  scan(text = cmd$stdout, what = 1, quiet = TRUE)
}

## Some old Linux systems do not support pgrep -a, so we cannot filter the
## processes based on their command line. We get all child processes
## including pgrep's ancestors, and then use ps to list them again.
## This effectively filters out pgrep and its ancestors.

pgrep_children_linux <- function(pid) {
  allproc <- safe_system("pgrep", c("-d,", "-P", pid))
  assert_that(allproc$status == 0)

  cmd <- safe_system(
    "ps",
    c("-o", "pid", "--no-header", "-p", trimws(allproc$stdout))
  )
  assert_that(cmd$status == 0 | cmd$status == 1)

  cmd
}

pgrep_children_unix <- function(pid) {
  cmd <- safe_system("pgrep", c("-P", pid))
  assert_that(cmd$status == 0 | cmd$status == 1)

  cmd
}


safe_system <- function(command, args) {

  out <- tempfile()
  err <- tempfile()
  on.exit(unlink(out), add = TRUE)
  on.exit(unlink(err), add = TRUE)

  ## We suppress warnings, they are given if the command
  ## exits with a non-zero status
  suppressWarnings(
    res <- system2(command, args = args, stdout = out, stderr = err)
  )

  list(
    stdout = win2unix(read_char(out)),
    stderr = win2unix(read_char(err)),
    status = res
  )
}

win2unix <- function(str) {
  gsub("\r\n", "\n", str, fixed = TRUE)
}

read_char <- function(path, ...) {
  readChar(path, nchars = file.info(path)$size, ...)
}

