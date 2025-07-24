#' @export
scale_type.ggpile_mirror <- function(x) c("mirror", "continuous")

#' @export
scale_x_mirror <- function(..., labels = abs) {
  ggplot2::scale_x_continuous(..., labels = labels)
}

#' @export
scale_y_mirror <- function(..., labels = abs) {
  ggplot2::scale_y_continuous(..., labels = labels)
}
