#' @export
mirror <- function(x, mirror) {
  mirror <- as_mirror(mirror, x)
  x[mirror] <- -x[mirror]
  class(x) <- "ggpile_mirror"
  x
}

as_mirror <- function(mirror, x) {
  if (is.null(mirror)) return(logical(length(x)))
  stopifnot(length(mirror) == length(x))
  if (is.logical(mirror)) return(mirror)
  mirror <- as.factor(mirror)
  stopifnot(nlevels(mirror) == 2)
  as.numeric(mirror) == 2
}
