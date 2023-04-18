#' huxtable_theme_fhi_basic
#' @param ht A huxtable
#' @param position Position
#' @export
huxtable_theme_fhi_basic <- function(ht, position = "center") {
  ht <- huxtable::set_top_padding(ht, 0.2)
  ht <- huxtable::set_bottom_padding(ht, 0.2)
  # ht <- huxtable::set_background_color(ht, huxtable::evens, huxtable::everywhere, "#F2F2F2")
  ht <- huxtable::set_bold(ht, 1, huxtable::everywhere, TRUE)
  ht <- huxtable::set_all_borders(ht, 1)
  ht <- huxtable::set_position(ht, position)
  ht <- huxtable::set_align(ht, huxtable::everywhere, huxtable::everywhere, "center")
  ht <- huxtable::set_top_padding(ht, huxtable::everywhere, huxtable::everywhere, 0.1)
  ht <- huxtable::set_bottom_padding(ht, huxtable::everywhere, huxtable::everywhere, 0.1)
  ht <- huxtable::set_left_padding(ht, huxtable::everywhere, huxtable::everywhere, 5)
  ht <- huxtable::set_right_padding(ht, huxtable::everywhere, huxtable::everywhere, 5)
  ht
}
