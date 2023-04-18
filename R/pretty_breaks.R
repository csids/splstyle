#' pretty_breaks
#' @param n a
#' @param digits number of decimal places
#' @param break_with_four_digits Whether break with four digits. Default is TRUE (optional)
#' @param ... dots
#' @export
pretty_breaks <- function(n = 5, digits = 0, break_with_four_digits = T, ...) {
  force_all(n, digits, break_with_four_digits, ...)
  n_default <- n
  function(x, n = n_default) {
    breaks <- pretty(x, n, ...)
    names(breaks) <- format_num_as_nor(breaks,
      digits = digits,
      break_with_four_digits = break_with_four_digits
    )
    breaks
  }
}
