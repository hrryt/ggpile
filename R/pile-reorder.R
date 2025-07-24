#' @export
pile_reorder <- function(.f, .x, .fun = median, ..., .m = NULL, .n = 10,
                         .na_rm = NULL, .default = Inf, .desc = FALSE) {
  .m <- as_mirror(.m, .f)
  left <- forcats::fct_reorder(
    .f[.m], .x[.m], .fun, ..., .na_rm = .na_rm, .default = .default, .desc = .desc
  )
  right <- forcats::fct_reorder(
    .f[!.m], .x[!.m], .fun, ..., .na_rm = .na_rm, .default = .default, .desc = .desc
  )
  pile(.f, left, right, mirror = .m, n = .n, desc = .desc)
}

#' @export
pile_infreq <- function(f, w = NULL, ordered = NA, m = NULL, n = 10) {
  stop("not implemented")
}

# needs tidying
pile <- function(f, left, right, mirror, n, desc) {
  stopifnot(length(n) == 1 && rlang::is_integerish(n))
  n <- as.integer(n)

  left_map <- function(x) n + 1 - rev(seq_along(x)) %mod% n
  right_map <- function(x) seq_along(x) %mod% n

  if (desc) {
    map_left <- right_map(levels(left))
    map_right <- right_map(levels(right))
  } else {
    map_left <- left_map(levels(left))
    map_right <- left_map(levels(right))
  }

  levels <- c(levels(left), levels(right))
  map <- c(map_left, map_right)

  left_numeric <- as.integer(left)
  right_numeric <- as.integer(right) + nlevels(left)
  x <- integer(length(levels))
  x[mirror] <- left_numeric
  x[!mirror] <- right_numeric

  stopifnot(length(map) == length(levels))
  stopifnot(rlang::is_integerish(map))
  map <- as.integer(map)
  stopifnot(rlang::is_integerish(x))
  x <- as.integer(x)

  new_pile(x, levels, map)
}

new_pile <- function(x, levels, map) {
  attr(x, "levels") <- levels
  attr(x, "map") <- map
  class(x) <- c("ggpile_pile")
  x
}

`%mod%` <- function(x, y) {
  (x - 1) %% y + 1
}

is_pile <- function(x) {
  inherits(x, "ggpile_pile")
}
