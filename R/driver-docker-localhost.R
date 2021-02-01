#' An object representing the current computer that R is running on.
localhost <- structure(list(), class = c("localhost", "host"))


#' @rdname localhost
#' @param x object
#' @param ... unused
#' @export
print.localhost <- function(x, ...) {
  cat("<localhost>")
}

#' @rdname docker_cmd
docker_cmd_con.localhost <- function(host, cmd = NULL, args = NULL, docker_opts = NULL, ...) {

  docker <- Sys.which("docker")
  if (docker == "") {
    stop("Cannot find Docker. Make sure that it is installed and in your PATH.")
  }

  args = c(cmd, docker_opts, args)
  pipe_with_pid(docker, args)
}
