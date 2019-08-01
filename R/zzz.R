pluck <- function(x, name, type) {
  if (missing(type)) {
    lapply(x, "[[", name)
  } else {
    vapply(x, "[[", name, FUN.VALUE = type)
  }
}

last <- function(x) {
  if (length(x) == 0) {
    return(list())
  } else {
    x[length(x)][1]
  }
}

compact <- function(x) Filter(Negate(is.null), x)

`%||%` <- function(x, y) {
  if (is.null(x) || all(nchar(x) == 0) || length(x) == 0) y else x
}

`%|i|%` <- function(x, y) {
  if (is.null(x) || all(nchar(x) == 0) || length(x) == 0) y else x
}

stract <- function(str, pattern) regmatches(str, regexpr(pattern, str))

assert <- function(x, y) {
  if (!is.null(x)) {
    if (!inherits(x, y)) {
      stop(deparse(substitute(x)), " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}

check_for_a_pkg <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    stop("Please install ", x, call. = FALSE)
  } else {
    invisible(TRUE)
  }
}
