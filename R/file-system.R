
#' Create a temporary directory
#'
#' Creates a temporary directory, returning the file path of the freshly created directory.
#' This function stops on failure rather than merely showing warnings.
#'
#' @param prefix A character vector giving the initial part of the temporary directory name.
#'
#' @return A character vector of the canonical file path of the created temporary directory.
mkdir_temporary <- function(prefix = "resurrectr") {
  assert_that(is.string(prefix))

  path <- tempfile(pattern = prefix)

  with_warnings_as_errors({
    dir.create(path, recursive = TRUE)
  })

  normalizePath(path)
}


## Wrapper around file.copy which stops on failure rather than mereley showing warnings.
file_copy <- function(from, to, overwrite = recursive, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE) {
  with_warnings_as_errors(file.copy(from, to, overwrite, recursive, copy.mode, copy.date))
}
