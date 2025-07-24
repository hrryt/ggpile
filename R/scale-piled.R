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
    rescale = function(self, x, limits = self$get_limits(), range) {
      # need expansion of range; where is it implemented?
      # must be some function of limits without map
      range <- self$dimension(limits = NULL)
      scales::rescale(self$map(x, limits = limits), from = range)
    },
    transform = function(self, x) {
      stopifnot(is_pile(x))
      self$pile_map <- attr(x, "map")
      class(x) <- "factor"
      x
    },
     map = function(self, x, limits = self$get_limits()) {
      discrete <- is.discrete(x)
      x <- ggplot2::ggproto_parent(parent, self)$map(x, limits = limits)
      if (!discrete) {
        return(x)
      }
      # fine as position_pile() already warns of overlapping intervals
      ind <- round(x)
      self$pile_map[ind] + x - ind
    }
  )
}

is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}
