#' Save ggplot in A4 scale
#' @param q Plot
#' @param filename filename
#' @param landscape landscape dimensions?
#' @param scaling_factor How much larger/smaller than A4?
#' @returns Nothing.
#' @export
save_a4 <- function(q, filename, landscape = T, scaling_factor = 1) {
  if (landscape) {
    ggsave(
      filename,
      plot = q,
      width = 297 * scaling_factor,
      height = 210 * scaling_factor,
      units = "mm"
    )
  } else {
    ggsave(
      filename,
      plot = q,
      width = 210 * scaling_factor,
      height = 297 * scaling_factor,
      units = "mm"
    )
  }
}
