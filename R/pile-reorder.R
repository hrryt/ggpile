#' @export
pile_reorder <- function(.f, .x, .fun = median, ..., .mirror = NULL, .n = 10,
                         .na_rm = NULL, .default = Inf, .desc = TRUE) {
  reordered <- forcats::fct_reorder(
    .f, .x, .fun, ..., .na_rm = .na_rm, .default = .default, .desc = .desc
  )
  pile(reordered, mirror = .mirror, n = .n)
}

#' @export
pile_infreq <- function(f, w = NULL, ordered = NA, mirror = NULL, n = 10) {
  infreq <- forcats::fct_infreq(f, w = w, ordered = ordered)
  pile(infreq, mirror = mirror, n = n)
}

pile <- function(f, mirror, n) {
  stopifnot(all(sapply(split(mirror, f), function(x) all(x[-1] == x[1]))))
  mirror_level <- sapply(split(mirror, f), function(x) x[1])
  map <- integer(length(mirror_level))
  map[mirror_level] <- seq_len(sum(mirror_level)) %mod% n
  map[!mirror_level] <- seq_len(sum(!mirror_level)) %mod% n
  new_pile(f, map)
}

new_pile <- function(f, map) {
  attr(f, "map") <- map
  class(f) <- c("ggpile_pile")
  f
}

`%mod%` <- function(x, y) {
  (x - 1) %% y + 1
}

is_pile <- function(x) {
  inherits(x, "ggpile_pile")
}
