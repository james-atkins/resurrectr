#' Create a "resurrected" object
#'
#' `resurrected()` is the standard constructor for resurrected-objects and `is.resurrected()` tests.
#'
#' @param subclass name of subclass. "resurrected" is an abstract base class, so you
#'     must supply this value. `resurrected_` is automatically prepended to the
#'     class name.
#' @param finalizer function to call on finalization or NULL. Function must accept
#'     a single argument, which will be the object to finanlise.
#' @param ... fields used by object.
#' @param x object to test for "resurrected"-ness.
#'
#' @export
resurrected <- function(subclass, finalizer = NULL, ...) {
  subclass <- paste0("resurrected_", subclass)

  r <- structure(
    list2env(list(...)),
    class = c(subclass, "resurrected")
  )

  if (!is.null(finalizer)) {
    # Register a finalizer to run upon garbage collection and/or session exit.
    reg.finalizer(r, finalizer, onexit = TRUE)
  }

  r
}


#' @rdname resurrected
#' @export
is.resurrected <- function(x) {
  inherits(x, "resurrected")
}


#' @export
print.resurrected <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
}
