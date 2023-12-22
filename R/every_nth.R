#' Returns every nth discrete value
#' @param n nth discrete value
#' @examples
#' every_nth(4)(c(1:10))
#' \dontrun{
#' scale_x_discrete(NULL, breaks = every_nth(n = 2))
#' }
#' @returns A function that returns every nth discrete value.
#' @export
every_nth <- function(n) {
  force(n)
  return(function(x) {
    # old:  x[c(TRUE, rep(FALSE, n - 1))]: prints T,F,F, T,F,F
    # alt1: x[c(rep(FALSE, n - 1), TRUE)]: this also prints F,F,T, F,F,T
    rev(rev(x)[c(TRUE, rep(FALSE, n - 1))]) # prints c(2,5)
  })
}
