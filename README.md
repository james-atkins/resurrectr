resurrectr
================

resurrectr is a tool to *resurrect* temporary databases from database dump files. Currently, only MySQL is supported. This is useful for reproducible research - rather than exporting database tables as CSV files, instead take a snapshot of the database and then spin-up a temporary database from that snapshot when necessary.

How to install
--------------

Use the `devtools` package to install `resurrectr`.

``` r
devtools::install_github("james-atkins/resurrectr.git")
```

Requirements
---------------

Docker must be installed to use resurrectr. Follow the links below to install it for your operating system.

* [OS X](https://docs.docker.com/docker-for-mac/install/)
* [Linux](https://docs.docker.com/engine/installation/linux/)


Getting Started
-------------------

The `resurrect_mysql` function creates a temporary database from the given MySQL dump file.

```r
library(resurrectr)
db <- resurrect_mysql("~/database_backup.sql")
```

When `db` is garbage collected and/or your R script ends, the MySQL database is stopped and deleted.

Database connection settings are stored in the `r db$config` list. These are randomly generated.
**Be sure to connect to the port specified in `db$config` rather than 3306.**

For example, to connect to the database using the `RMySQL` package:

```r
library(RMySQL)

conn <- dbConnect(MySQL(), host = db$config$host, port = db$config$port,
                  dbname = db$config$dbname,
                  user = db$config$user, password = db$config$password)
```
