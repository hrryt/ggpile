#' @export
scale_type.ggpile_pile <- function(x) c("pile", "discrete")

#' @export
scale_x_pile <- function(..., guide = "none") {
  parent <- ggplot2::scale_x_discrete(..., guide = guide)
  pile_scale(parent)
}

#' @export
scale_y_pile <- function(..., guide = "none") {
  parent <- ggplot2::scale_y_discrete(..., guide = guide)
  pile_scale(parent)
}

pile_scale <- function(parent) {
  ggplot2::ggproto(
    "ScalePilePosition", parent,
    pile_map = NULL,
    transform = function(self, x) {
      stopifnot(is_pile(x))
      self$pile_map <- attr(x, "map")
      x
    },
    map = function(self, x, limits = self$get_limits()) {
      x <- ggplot2::ggproto_parent(parent, self)$map(x, limits = limits)
      ind <- round(x)
      if (all(!is.finite(x))) return(x)
      if (max(ind) != length(self$pile_map)) return(x)
      self$pile_map[ind] + x - ind
    }
  )
}
