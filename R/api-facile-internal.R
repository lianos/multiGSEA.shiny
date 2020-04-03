# This functions are defined in facilebio, but we don't want to rely on those.

#' Tests if a module is initialized
#'
#' @param a module
#' @return logical TRUE/FALSE
initialized <- function(x, ...) {
  UseMethod("initialized", x)
}

#' @noRd
initialized.ReactiveGeneSetDb <- function(x, ...) {
  is(x$gdb(), "GeneSetDb") && nrow(x$geneSets()) > 0
}

#' Checks if the return value from a selectInput looks like it's unselected
#'
#' Copied from FacileViz, but we don't want to depend on that.
#'
#' For some reason, sometimes a selectInput returns `NULL` and other times
#' it returns `""`, so I'm just making this utility function to deal with that
#'
#' NOTE: This is really a function that is used by shiny modules, but instead
#' of having every shiny function check if something is "unselected" and setting
#' it to NULL, we move that functionality in here so that internal vizualization
#' functions can do that jusst once
#'
#' @param value The (character) object returned from a `selectInput`
unselected <- function(value, ignore = c("---", "__initializing__", "")) {
  if (is.null(value)) return(TRUE)
  # Otherwise this is a character
  if (is(value, "data.frame") || is(value, "tbl")) {
    val <- nrow(value) == 0
  } else {
    val <- length(value) == 0L || nchar(value) == 0L || all(value %in% ignore)
  }
  val
}
