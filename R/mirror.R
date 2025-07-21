#' @export
mirror <- function(x, mirror) {
  stopifnot(is.logical(mirror))
  x[mirror] <- -x[mirror]
  x
}
