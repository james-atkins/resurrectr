docker_option_env_var <- function(name, value) {
  c(
    "-e",
    sprintf('%s="%s"', name, value)
  )
}

docker_option_mount_host_volume <- function(host_path, container_path, read_only = FALSE) {
  mounts <- sprintf("%s:%s", host_path, container_path)

  if (read_only) {
    paste0(mounts, ":ro")
  }

  c(
    "-v",
    mounts
  )
}

docker_option_publish_port <- function(container_port) {
  c(
    "-p",
    sprintf("127.0.0.1::%s", container_port)
  )
}
