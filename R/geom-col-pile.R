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

    max_in_level <- tapply(data$y, data$level, signed_max)
    level_below <- data$level - sign(data$level)
    data$max_ybelow <- max_in_level[as.character(level_below)]

    linear_gradient_fun <- get_linear_gradient_fun(flipped_aes)
    data$negative <- data$y < 0

    data$ymin_grad <- data$negative + data$ybelow / abs(data$y)
    data$ymax_grad <- data$negative + data$max_ybelow / abs(data$y)
    data$no_grad <- is.na(data$ymax_grad) | data$ymax_grad == data$ymin_grad
    data$fill <- linear_gradients(data, linear_gradient_fun, "white")

    data <- data[order(abs(data$level), decreasing = TRUE), ]

    shadow_width <- 0.8

    data_shadow <- data
    data_shadow$xmin <- data_shadow$x + shadow_width / 2
    data_shadow$xmax <- data_shadow$x - shadow_width / 2
    data_shadow$colour <- NA
    data_shadow$fill <- '#00000000'

    shadow_depth <- 0.5

    data_shadow$ymin_grad <- as.numeric(data$negative)
    data_shadow$ymax_grad <- as.numeric(!data$negative)
    data_shadow$ymin <- data$y - data_shadow$ymin_grad * shadow_depth
    data_shadow$ymax <- data_shadow$ymin + shadow_depth
    data_shadow$no_grad <- FALSE

    data_shadow$fill <- linear_gradients(
      data_shadow, linear_gradient_fun, "grey30"
    )

    data <- rbind(data, data_shadow)

    data <- ggplot2::flip_data(data, flipped_aes)
    ggplot2::ggproto_parent(ggplot2::GeomCol, self)$draw_panel(
      data, panel_params, coord, lineend = lineend, linejoin = linejoin
    )
  }
)

signed_max <- function(x) {
  max(abs(x)) * sign(x[1])
}

linear_gradients <- function(df, linear_gradient_fun, highlight) {
  get_gradient <- function(i) {
    x <- df[i, , drop = FALSE]
    if (x$no_grad) return(x$fill)
    linear_gradient_fun(
      c(highlight, x$fill),
      ymin = x$ymin_grad,
      ymax = x$ymax_grad
    )
  }
  lapply(seq_len(nrow(df)), get_gradient)
}

get_linear_gradient_fun <- function(flipped_aes) {
  if (flipped_aes) {
    function(colours, ymin, ymax) {
      grid::linearGradient(
        colours = colours,
        x1 = ymin,
        x2 = ymax,
        y2 = 0,
        group = FALSE
      )
    }
  } else {
    function(colours, ymin, ymx) {
      grid::linearGradient(
        colours = colours,
        y1 = ymin,
        y2 = ymax,
        x2 = 0,
        group = FALSE
      )
    }
  }
}
