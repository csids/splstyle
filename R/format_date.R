#' Format date in the Norwegian format
#' @param x Date
#' @param format The desired format
#' @returns A character vector
#' @rdname format_date
#' @export
format_date_as_nor <- function(x = lubridate::today(), format = "%d.%m.%Y") {
  retval <- format.Date(x, format = format)
  return(retval)
}

#' Format datetime in the Norwegian format
#' @param x Datetime
#' @param format The desired format
#' @returns A character vector
#' @rdname format_date
#' @export
format_datetime_as_nor <- function(x = lubridate::now(), format = "%d.%m.%Y kl. %H:%S") {
  retval <- format.Date(x, format = format)
  return(retval)
}

#' Format datetime in a file format
#' @param x Datetime
#' @param format The desired format
#' @returns A character vector
#' @rdname format_date
#' @export
format_datetime_as_file <- function(x = lubridate::now(), format = "%Y_%m_%d_%H%M%S") {
  retval <- format.Date(x, format = format)
  return(retval)
}
