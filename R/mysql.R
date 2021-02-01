mysql_query_create_user <- function(username, password = "") {

  check_vector_lengths(username, password)

  paste0(
    sprintf("CREATE USER '%s'@'%%'", username),
    ifelse(password != '', sprintf(" IDENTIFIED BY '%s'", password), ""),
    ";")
}


mysql_query_grant_privileges <- function(username, privileges) {
  assert_that(is.string(username))

  privileges_string <- paste0(privileges, collapse = ", ")

  # Only SELECT and SHOW VIEW privileges are supported, for now.
  if (!all(privileges %in% c("SELECT", "SHOW VIEW"))) {
    stop("Invalid privileges.")
  }

  sprintf("GRANT %s ON *.* TO '%s'@'%%';", privileges_string, username)
}
