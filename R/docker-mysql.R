#' Create MySQL initialisation directory.
#'
#' Create a directory from which to initialise the database. This will be bound to
#' /docker-entrypoint-initdb.d.
#'
#' This directory contains the MySQL dump file which is copied here, and if username is not null,
#' also a SQL file which creates the given user.
#' and grants them readonly privileges.
#'
#' @param path_to_mysql_dump Path to the MySQL dump file.
#' @param username Username
#' @param password Password, or NULL, if no password.
make_init_db_directory <- function(path_to_mysql_dump, username = NULL, password = NULL) {

  assert_that(is.null(username) || is.string(username))
  assert_that(is.null(password) || is.string(password))


  temp_directory <- mkdir_temporary()
  file_copy(path_to_mysql_dump, temp_directory)

  if (!is.null(username)) {

    if (is.null(password)) {
      password = ""
    }

    query <- c(
      mysql_query_create_user(username, password),
      mysql_query_grant_privileges(username, c("SELECT", "SHOW VIEW"))
    )

    cat(query, file = file.path(temp_directory, "create-user.sql"), sep = "\n")
  }

  temp_directory
}



#' Resurrect a MySQL database from a dump file
#'
#' Ideally, resurrectr should support different "drivers", eg Docker, Vagrant etc. However for now, only
#' Docker is supported.
#'
#' @param path_to_mysql_dump Path to the MySQL dump file.
#' @param version Version of MySQL to run. In Docker language, this is a tag of the mysql image.
#'     See https://hub.docker.com/r/library/mysql/tags/ for available versions.
#' @param quiet Whether container output is echoed.
#'
#' @return a resurrected_mysql object
#' @export
resurrect_mysql <- function(path_to_mysql_dump, version = "5.7", quiet = FALSE) {

  assert_that(is.string(path_to_mysql_dump))
  if (!file.exists(path_to_mysql_dump)) {
    stop("Could not find MySQL dump: ", path_to_mysql_dump)
  }

  ## Generate random database name, username and password.
  dbname   <- random_name()
  username <- random_name()
  password <- random_name()

  image <- paste0("mysql", ":", version)

  ## --- Run the Docker container and wait until MySQL is listening for connections ---

  ## Create the temporary directory to bind to /docker-entrypoint-initdb.d
  init_db_dir <- make_init_db_directory(path_to_mysql_dump, username = username, password = password)

  ## Configure the docker container
  docker_opts <- c(
    docker_option_env_var("MYSQL_DATABASE", dbname),
    docker_option_env_var("MYSQL_ROOT_PASSWORD", "you_should_not_use_the_root_user!"),
    docker_option_publish_port(3306), # Forward 3306 to a random port on localhost
    docker_option_mount_host_volume(init_db_dir, "/docker-entrypoint-initdb.d", read_only = TRUE)
  )

  ## If running in a virtual machine then create a temporary directory on the host
  ## and bind mount to the container data volume to speed things up.
  if (is_os_x()) {
    temp_data_dir <- mkdir_temporary()
    docker_opts <- c(docker_opts, docker_option_mount_host_volume(temp_data_dir, "/var/lib/mysql"))
  } else {
    temp_data_dir <- NULL
  }

  container <- docker_run(image = image, docker_opts = docker_opts, detach = TRUE)

  ## Wait until the database has been initialized and the server is listening for
  ## connections
  db_initialized <- FALSE
  okay <- FALSE

  con <- container_stream_logs(container)
  on.exit(close(con, kill = TRUE))
  if (!isOpen(con)) {
    open(con, "r")
  }

  while(length(txt <- readLines(con, n = 1))) {
    if (!quiet) {
      cat(txt, fill = TRUE)
    }

    if (!db_initialized && grepl("MySQL init process done. Ready for start up.", txt, fixed = TRUE)) {
      db_initialized <- TRUE
    }

    if (db_initialized && grepl("mysqld: ready for connections", txt, fixed = TRUE)) {
      okay <- TRUE
      break
    }
  }

  if (!okay) {
    stop("Error starting MySQL server.")
  }

  ## Find what port 3306 is forwarded to on the host
  host_port <- as.numeric(container$info$NetworkSettings$Ports$`3306/tcp`[[1]]$HostPort)

  resurrected("mysql",
              finalizer = docker_mysql_finalizer,
              container = container,
              init_db_dir = init_db_dir,
              temp_data_dir = temp_data_dir,
              config = list(host = "127.0.0.1",
                            port = host_port,
                            dbname = dbname,
                            user = username,
                            password = password))
}


docker_mysql_finalizer <- function(resurrected_mysql) {
  container_rm(resurrected_mysql$container, force = TRUE)

  unlink(resurrected_mysql$init_db_dir, recursive = TRUE)

  if (!is.null(resurrected_mysql$temp_data_dir)) {
    unlink(resurrected_mysql$temp_data_dir, recursive = TRUE)
  }
}
