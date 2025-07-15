#' @export
scale_x_piled <- function(..., n = 5, guide = "none") {
  parent <- ggplot2::scale_x_discrete(..., guide = guide)
  piled_scale(parent, n)
}

#' @export
scale_y_piled <- function(..., n = 5, guide = "none") {
  parent <- ggplot2::scale_y_discrete(..., guide = guide)
  piled_scale(parent, n)
}

piled_scale <- function(parent, n) {
  force(n)
  ggplot2::ggproto(
    "ScaleDiscretePositionPiled", parent,
    map = function(self, x, limits = self$get_limits()) {
      x <- ggplot2::ggproto_parent(parent, self)$map(x, limits)
      pile <- x >= n + 1
      pile[is.na(pile)] <- FALSE
      r <- x[pile] %% n
      x[pile] <- ifelse(r < 1, r + n, r)
      x
    }
  )
}
