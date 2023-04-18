#' format_date_nor
#' @param x value
#' @param format the desired format
#' @rdname format_date
#' @export
format_date_as_nor <- function(x = lubridate::today(), format = "%d.%m.%Y") {
  retval <- format.Date(x, format = format)
  return(retval)
}

#' format_datetime_nor
#' @param x value
#' @param format the desired format
#' @rdname format_date
#' @export
format_datetime_as_nor <- function(x = lubridate::now(), format = "%d.%m.%Y kl. %H:%S") {
  retval <- format.Date(x, format = format)
  return(retval)
}

#' format_date_nor
#' @param x value
#' @param format the desired format
#' @rdname format_date
#' @export
format_datetime_as_file <- function(x = lubridate::now(), format = "%Y_%m_%d_%H%M%S") {
  retval <- format.Date(x, format = format)
  return(retval)
}
