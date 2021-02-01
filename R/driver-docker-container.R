#' Coerce an object into a container object.
#'
#' @param x An object to coerce
#' @param host A docker host
#'
#' A container object represents a Docker container on a host.
as.container <- function(x, host = localhost) {
  UseMethod("as.container")
}

#' @rdname as.container
as.container.character <- function(x, host = localhost) {
  info <- docker_inspect(host, x)[[1]]
  as.container(info, host)
}

#' @rdname as.container
as.container.list <- function(x, host = localhost) {
  # x should be the output of docker_inspect()
  if (is.null(x$Name) || is.null(x$Id))
    stop("`x` must be information about a single container.")

  structure(
    list(
      host = host,
      id = substr(x$Id, 1, 12),
      name = sub("^/", "", x$Name),
      image = x$Config$Image,
      cmd = x$Config$Cmd,
      info = x
    ),
    class = "container"
  )
}

#' @rdname as.container
as.container.container <- function(x, host = localhost) {
  x
}

#' Print method for container
#'
#' @param x A container object
#' @param ... unused
#' @export
print.container <- function(x, ...) {
  cat("<container>")
  cat(
    "\n  ID:      ", x$id,
    "\n  Name:    ", x$name,
    "\n  Image:   ", x$image,
    "\n  Command: ", x$cmd,
    "\n  Host:  ",
    indent(
      paste(utils::capture.output(print(x$host)), collapse = "\n"),
      indent = 2
    )
  )
}

#' Delete a container.
#'
#' @param container A container object.
#' @param force Force removal of a running container.
#' @examples
#' \dontrun{
#' container_rm(con)
#' }
container_rm <- function(container, force = FALSE) {
  args <- c(if (force) "-f", container$id)
  docker_cmd(container$host, "rm", args)
}

#' Stream logs for a container.
#'
#' @param container A container object.
#' @return A connection
container_stream_logs <- function(container) {
  docker_cmd_con(container$host, "logs", c("--follow", container$id))
}
