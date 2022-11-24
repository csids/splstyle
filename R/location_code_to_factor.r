#' Makes a factor with a label from a vector of location codes
#' @param x A vector of location codes
#' @param label The label of the elements in the new factor
#' @param label_if_not_unique The label of elements that are not unique
#' @param reference Where you get your data
#' @param direction -1 is reverse
#' @export
location_code_to_factor <- function(
  x,
  label = csstyle::config$location_code_to_factor_label, # location_name
  label_if_not_unique = csstyle::config$location_code_to_factor_label_if_not_unique, # location_name_description_nb
  reference = csdata::nor_locations_names(),
  direction = 1
){
  new_labels_x <- reference[[label]][reference$location_code %in% x]
  location_order_x <- reference$location_order[reference$location_code %in% x]
  levels_x <- reference$location_code[reference$location_code %in% x]
  if(length(new_labels_x[duplicated(new_labels_x)]) != 0){
    if(is.null(label_if_not_unique)){
      stop("Your labels are not unique, also use label_if_not_unique")
    }
    duplicate_x <- new_labels_x[duplicated(new_labels_x)]
    for(i in 1:length(new_labels_x)){
      if(new_labels_x[i] %in% duplicate_x){
        new_labels_x[i] <- reference[[label_if_not_unique]][location_order_x[i]]
      }
    }
    # print(new_labels_x)

  }

  if(direction == -1){
    levels_x <- rev(levels_x)
    new_labels_x <- rev(new_labels_x)
  }

  retval <- factor(
    x,  # data
    levels = levels_x, # unique levels indata in the right order
    labels = new_labels_x # pretty names
  )
  return(retval)
}
