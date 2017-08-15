##' Rounds every numeric column of a data.table to given precision
##' @param x a \code{data.table} like thing
##' @param digits the number of digits to round to
##' @return the rounded \code{x}
round.dt <- function(x, digits=3) {
  stopifnot(is(x, 'data.table'))
  for (cname in names(x)[sapply(x, is.numeric)]) {
    vals <- x[[cname]]
    if (!is.integer(vals)) {
      x[, (cname) := round(vals, digits=3)]
    }
  }
  x
}
