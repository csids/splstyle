#' Makes a character from a vector of location codes
#' @param x A vector of location codes
#' @param label The label you want the elements in the character to have
#' @param label_if_not_unique The label of elements that are not unique
#' @param reference Where you get your data
#' @param direction -1 is reverse
#' @export
location_code_to_character <- function(
  x,
  label = csstyle::config$location_code_to_factor_label, # location_name
  label_if_not_unique = csstyle::config$location_code_to_factor_label_if_not_unique, # location_name_description_nb
  reference = fhidata::norway_locations_names(),
  direction = 1
){

  retval <- location_code_to_factor(
    x = x,
    label = label,
    label_if_not_unique = label_if_not_unique,
    reference = reference,
    direction = direction
  )
  retval <- as.character(retval)
  return(retval)
}

