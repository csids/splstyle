#' format_ages
#' @param x a string (or multiple strings) with an age or an age group
#' @examples
#' format_ages("009")
#' format_ages("005-014")
#' @export
format_ages <- function(x){
  age_levels <- unique(x)
  age_levels <- sort(age_levels)

  # remove all 0s at the start
  age_labels <- stringr::str_remove_all(age_levels, "^[0]+")

  # if there was just 000 at the start, it is now empty, so we need to put a zero back in
  age_labels <- stringr::str_replace_all(age_labels, "^-", "0-")

  # remove all 0s after hyphens
  age_labels <- stringr::str_replace_all(age_labels, "\\-[0]+", "-")

  # if there was just 000 after the hyphen, it is now empty, so we need to put a zero back in
  age_labels <- stringr::str_replace_all(age_labels, "\\-$", "-")
  age_labels <- paste0(age_labels, " ", csdata::nb$aa,"r")

  return(factor(x, levels = age_levels, labels = age_labels))
}
