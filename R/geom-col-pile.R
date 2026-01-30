#' @export
geom_col_pile <- function(
  mapping = NULL,
  data = NULL,
  stat = "pile",
  position = "identity",
  ...,
  just = 0.5,
  lineend = "butt",
  linejoin = "mitre",
  na.rm = FALSE,
  draw_shadow = TRUE,
  shadow_width = NULL,
  shadow_depth = NULL,
  show.legend = NA,
  inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomColPile,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = rlang::list2(
      na.rm = na.rm,
      just = just,
      lineend = lineend,
      linejoin = linejoin,
      draw_shadow = draw_shadow,
      shadow_width = shadow_width,
      shadow_depth = shadow_depth,
      ...
    )
  )
}

#' @export
GeomColPile <- ggplot2::ggproto(
  "GeomColPile", ggplot2::GeomCol,
  draw_panel = function(
    self,
    data,
    panel_params,
    coord,
    lineend = "butt",
    linejoin = "mitre",
    flipped_aes = FALSE,
    draw_shadow = TRUE,
    shadow_width = NULL,
    shadow_depth = NULL
  ) {
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
    data$is_negative <- as.numeric(data$y < 0)

    data$ymin_grad <- data$is_negative + data$ybelow / abs(data$y)
    data$ymax_grad <- data$is_negative + data$max_ybelow / abs(data$y)
    data$no_grad <- is.na(data$ymax_grad) | data$ymax_grad == data$ymin_grad
    data$fill <- bar_gradients(data, linear_gradient_fun, "white")

    data <- data[order(abs(data$level), decreasing = TRUE), ]

    if (draw_shadow) {
      shadow_width <- shadow_width %||% (0.85 * data$width %||% 0.9)
      shadow_depth <- shadow_depth %||% (max(abs(data$y)) / 200)
      data_shadow <- shadow_data(data, linear_gradient_fun, shadow_width, shadow_depth)
      data <- rbind(data, data_shadow)
    }

    data <- ggplot2::flip_data(data, flipped_aes)
    ggplot2::ggproto_parent(ggplot2::GeomCol, self)$draw_panel(
      data, panel_params, coord, lineend = lineend, linejoin = linejoin
    )
  }
)

signed_max <- function(x) {
  max(abs(x)) * sign(x[1])
}

shadow_data <- function(data, linear_gradient_fun, shadow_width, shadow_depth) {
  data$ymin_grad <- data$is_negative
  data$ymax_grad <- 1 - data$is_negative
  data$fill <- shadow_gradients(
    data, linear_gradient_fun, c("#4d4d4ddd", "#4d4d4d22")
  )

  data$ymin <- data$y - data$ymin_grad * shadow_depth
  data$ymax <- data$ymin + shadow_depth
  data$xmin <- data$x + shadow_width / 2
  data$xmax <- data$x - shadow_width / 2
  data$colour <- NA
  data
}

shadow_gradients <- function(df, linear_gradient_fun, colours) {
  get_gradient <- function(i) {
    x <- df[i, , drop = FALSE]
    linear_gradient_fun(
      colours,
      ymin = x$ymin_grad,
      ymax = x$ymax_grad
    )
  }
  lapply(seq_len(nrow(df)), get_gradient)
}

bar_gradients <- function(df, linear_gradient_fun, highlight) {
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
    function(colours, ymin, ymax) {
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
