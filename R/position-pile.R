#' @export
position_pile <- function (vjust = 1, reverse = FALSE) {
  ggplot2::ggproto(NULL, PositionPile, vjust = vjust, reverse = reverse)
}

PositionPile <- ggplot2::ggproto(
  "PositionPile", ggplot2::PositionStack,
  compute_panel = function(data, params, scales) {
    data <- ggplot2::flip_data(data, params$flipped_aes)
    if (is.null(params$var)) {
      return(data)
    }
    negative <- data$ymax < 0
    negative[is.na(negative)] <- FALSE
    neg <- data[negative, , drop = FALSE]
    pos <- data[!negative, , drop = FALSE]
    if (any(negative)) {
      neg <- ggplot2:::collide(
        neg, NULL, "position_pile", pos_pile,
        vjust = params$vjust, fill = params$fill, reverse = params$reverse
      )
    }
    if (!all(negative)) {
      pos <- ggplot2:::collide(
        pos, NULL, "position_pile", pos_pile,
        vjust = params$vjust, fill = params$fill, reverse = params$reverse
      )
    }
    data <- ggplot2:::vec_rbind0(neg, pos)[match(
      seq_len(nrow(data)),
      c(which(negative), which(!negative))
    ), ]
    ggplot2::flip_data(data, params$flipped_aes)
  }
)

pos_pile <- function(df, width, vjust = 1, fill = FALSE) {
  n <- nrow(df) + 1
  y <- ifelse(is.na(df$y), 0, df$y)
  df <- df[order(y), ]
  heights <- c(0, df$y)
  if (fill) {
    heights <- heights / abs(heights[length(heights)])
  }
  if (!is.null(df$ymin) && !is.null(df$ymax)) {
    max_is_lower <- df$ymax < df$ymin
  }
  else {
    max_is_lower <- rep(FALSE, nrow(df))
  }
  ymin <- pmin(heights[-n], heights[-1])
  ymax <- pmax(heights[-n], heights[-1])
  df$y <- (1 - vjust) * ymin + vjust * ymax
  df$ymin <- as.numeric(ifelse(max_is_lower, ymax, ymin))
  df$ymax <- as.numeric(ifelse(max_is_lower, ymin, ymax))
  df
}
