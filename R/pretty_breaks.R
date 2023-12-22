#' Pretty breaks
#' @param n a
#' @param formatting_fn Formatting function
#' @param ... dots
#' @returns A function that can be used for breaks on graphing.
#' @export
pretty_breaks <- function(n = 5, formatting_fn = csstyle::format_num_as_nor_num_0, ...) {
  force_all(n, formatting_fn, ...)
  n_default <- n
  function(x, n = n_default) {
    breaks <- pretty(x, n, ...)
    names(breaks) <- formatting_fn(breaks)
    breaks
  }
}
