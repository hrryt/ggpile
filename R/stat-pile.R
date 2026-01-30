#' @export
stat_pile <- function(mapping = NULL, data = NULL,
                      geom = "bar", position = "identity", ...,
                      na.rm = FALSE, orientation = NA, desc = FALSE, mod = 10,
                      show.legend = NA, inherit.aes = TRUE) {
  params <- rlang::list2(
    na.rm = na.rm, orientation = orientation,
    desc = desc, mod = mod, ...
  )
  ggplot2::layer(
    data = data, mapping = mapping, stat = StatPile, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = params
  )
}

#' @export
StatPile <- ggplot2::ggproto(
  "StatPile", ggplot2::Stat,
  retransform = FALSE,
  required_aes = "x|y",
  optional_aes = "mirror",
  default_aes = ggplot2::aes(
    fill = after_stat(dlevel),
    colour = after_stat(dlevel)
  ),
  extra_params = c("na.rm", "orientation", "desc", "mod"),
  setup_params = function(self, data, params) {
    params$flipped_aes <- ggplot2::has_flipped_aes(
      data, params, main_is_orthogonal = TRUE
    )
    params
  },
  compute_panel = function(self, data, scales, flipped_aes = FALSE,
                           desc = FALSE, mod = 10) {
    if (empty(data)) return(data.frame())

    if (!rlang::is_scalar_integerish(mod) || mod < 1) {
      cli::cli_abort("{.var mod} must be a positive integer")
    }
    if (!rlang::is_scalar_logical(desc)) {
      cli::cli_abort("{.var desc} must be a logical value")
    }

    data$flipped_aes <- flipped_aes
    data <- ggplot2::flip_data(data, flipped_aes)

    if (is.null(data$mirror)) {

      split_data <- split(data, data$y < 0)
      if (length(split_data) != 2) {
        split_data[[2]] <- data.frame()
      }

    } else {

      split_data <- split(data, data$mirror)
      if (length(split_data) != 2) {
        cli::cli_abort("{.var mirror} aesthetic must have two levels")
      }
      split_data[[2]]$y <- -split_data[[2]]$y

    }

    pile_data <- rbind(
      pile(split_data[[1]], flipped_aes = flipped_aes, desc = desc, mod = mod),
      pile(split_data[[2]], flipped_aes = flipped_aes, desc = desc, mod = mod)
    )
    pile_data$dlevel = as.factor(pile_data$level)
    flip_data(pile_data, flipped_aes)
  }
)

empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || inherits(df, "waiver")
}

pile <- function(df, flipped_aes, desc, mod) {
  if (empty(df)) return(data.frame())

  negative <- any(df$y < 0)
  if (negative && any(df$x > 0)) {
    y <- if (flipped_aes) "x" else "y"
    cli::cli_abort(
      "{.var {y}} must be non-negative if {.var mirror} is supplied"
    )
  }

  if (negative) {
    df$y <- -df$y
  }

  df <- df[order(df$y, decreasing = TRUE), ]
  nrow <- nrow(df)

  level <- rep(rev(seq_len(ceiling(nrow / mod))), each = mod)
  df$level <- level[seq_len(nrow)]

  x <- seq_len(mod)
  if (!desc) x <- rev(x)
  df$x <- rep(x, length.out = nrow)

  if (negative) {
    df$y <- -df$y
    df$level <- -df$level
  }

  df
}
