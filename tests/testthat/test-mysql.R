context("Test MySQL")

test_that("valid CREATE USER query generated", {
  expect_equal(mysql_query_create_user("username"), "CREATE USER 'username'@'%';")
  expect_equal(mysql_query_create_user("username", "password"), "CREATE USER 'username'@'%' IDENTIFIED BY 'password';")

  expect_equal(mysql_query_create_user(c("username1", "username2"), "password"), c("CREATE USER 'username1'@'%' IDENTIFIED BY 'password';",
                                                                                   "CREATE USER 'username2'@'%' IDENTIFIED BY 'password';"))

  expect_equal(mysql_query_create_user(c("username1", "username2"), c("password1", "password2")),
               c("CREATE USER 'username1'@'%' IDENTIFIED BY 'password1';", "CREATE USER 'username2'@'%' IDENTIFIED BY 'password2';"))

  expect_error(mysql_query_create_user(c("username1", "username2", "username3"), c("password1", "password2")))
})

test_that("valid GRANT query generated", {
  expect_equal(mysql_query_grant_privileges("username", "SELECT"), "GRANT SELECT ON *.* TO 'username'@'%';")
  expect_equal(mysql_query_grant_privileges("username", c("SELECT", "SHOW VIEW")), "GRANT SELECT, SHOW VIEW ON *.* TO 'username'@'%';")
})
