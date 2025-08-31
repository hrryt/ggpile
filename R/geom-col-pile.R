#' @export
geom_col_pile <- function(mapping = NULL, data = NULL,
                          stat = "pile", position = "identity", ...,
                          just = 0.5, width = NULL, na.rm = FALSE,
                          show.legend = FALSE, inherit.aes = TRUE) {
  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomColPile,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = rlang::list2(just = just, width = width, na.rm = na.rm, ...)
  )
}

#' @export
GeomColPile <- ggplot2::ggproto(
  "GeomColPile", ggplot2::GeomCol,
  draw_panel = function(self, data, panel_params, coord,
                        lineend = "butt", linejoin = "mitre",
                        width = NULL, flipped_aes = FALSE) {
    data <- ggplot2::flip_data(data, flipped_aes)

    data$level_above <- data$level + sign(data$level)
    data <- merge(
      data,
      data[c("x", "level_above", "y")],
      by.x = c("x", "level"),
      by.y = c("x", "level_above"),
      all.x = TRUE,
      suffixes = c("", "below")
    )
    data$ybelow[is.na(data$ybelow)] <- 0

    max_in_level <- tapply(data$y, data$level, function(x) max(abs(x)) * sign(x[1]))
    level_below <- data$level - sign(data$level)
    data$max_ybelow <- max_in_level[as.character(level_below)]

    data$negative <- data$y < 0
    data$ymin_grad <- data$negative + data$ybelow / abs(data$y)
    data$ymax_grad <- data$negative + data$max_ybelow / abs(data$y)
    data$no_grad <- is.na(data$ymax_grad) | data$ymax_grad == data$ymin_grad
    data$fill <- linear_gradients(data, flipped_aes)

    data <- data[order(abs(data$level), decreasing = TRUE), ]

    data <- ggplot2::flip_data(data, flipped_aes)
    ggplot2::ggproto_parent(ggplot2::GeomCol, self)$draw_panel(
      data, panel_params, coord, lineend = lineend, linejoin = linejoin
    )
  }
)

linear_gradients <- function(data, flipped_aes) {
  linear_gradient <- if (flipped_aes) {
    function(i) {
      x <- data[i, , drop = FALSE]
      if (x$no_grad) return(x$fill)
      grid::linearGradient(
        c("white", x$fill),
        x1 = x$ymin_grad,
        x2 = x$ymax_grad,
        y2 = 0,
        group = FALSE
      )
    }
  } else {
    function(i) {
      x <- data[i, , drop = FALSE]
      if (x$no_grad) return(x$fill)
      grid::linearGradient(
        c("white", x$fill),
        y1 = x$ymin_grad,
        y2 = x$ymax_grad,
        x2 = 0,
        group = FALSE
      )
    }
  }
  lapply(seq_len(nrow(data)), linear_gradient)
}
