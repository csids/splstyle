#' Formats a cstidy age as a factor
#' @param x a string (or multiple strings) with an age or an age group
#' @examples
#' csstyle::format_cstidy_age_as_factor("009")
#' csstyle::format_cstidy_age_as_factor("005-014")
#' @returns A vector of factors
#' @rdname format_cstidy_age_as
#' @export
format_cstidy_age_as_factor <- function(x){
  age_levels <- unique(x)
  age_levels <- sort(age_levels)

  # remove all 0s at the start
  age_labels <- stringr::str_remove_all(age_levels, "^[0]+")

  # if there was just 000 at the start, it is now empty, so we need to put a zero back in
  age_labels <- stringr::str_replace_all(age_labels, "^_", "0_")

  # remove all 0s after hyphens
  age_labels <- stringr::str_replace_all(age_labels, "\\_[0]+", "_")

  # if there was just 000 after the hyphen, it is now empty, so we need to put a zero back in
  age_labels <- stringr::str_replace_all(age_labels, "\\_$", "_")
  age_labels <- stringr::str_replace_all(age_labels, "_", "-")
  age_labels <- paste0(age_labels, " ", csdata::nb$aa,"r")

  return(factor(x, levels = age_levels, labels = age_labels))
}

#' Formats a cstidy age as a character
#' @param x a string (or multiple strings) with an age or an age group
#' @examples
#' csstyle::format_cstidy_age_as_character("009")
#' csstyle::format_cstidy_age_as_character("005-014")
#' @returns A vector of characters
#' @rdname format_cstidy_age_as
#' @export
format_cstidy_age_as_character <- function(x){
  return(as.character(format_cstidy_age_as_factor(x)))
}
