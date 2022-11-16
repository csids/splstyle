#' fhi_caption
#' @param caption text
#' @export
fhi_caption <- function(caption = "Folkehelseinstituttet {format_date_nor()}") {
  return(glue::glue(caption))
}

#' format_date_nor
#' @param x value
#' @param format the desired format
#' @export
format_date_nor <- function(x = lubridate::today(), format = "%d.%m.%Y") {
  retval <- format.Date(x, format = format)
  return(retval)
}

#' format_datetime_nor
#' @param x value
#' @param format the desired format
#' @export
format_datetime_nor <- function(x = lubridate::now(), format = "%d.%m.%Y kl. %H:%S") {
  retval <- format.Date(x, format = format)
  return(retval)
}

#' format_date_nor
#' @param x value
#' @param format the desired format
#' @export
format_datetime_file <- function(x = lubridate::now(), format = "%Y-%m-%d_%H%M%S") {
  retval <- format.Date(x, format = format)
  return(retval)
}

#' format_nor
#' @param x value
#' @param digits Number of digits after the decimal place (required)
#' @param sig Number of significant digits (optional)
#' @param break_with_four_digits Whether break with four digits. Default is TRUE (optional)
#' @export
format_nor <- function(x, digits = 0, sig = NULL, break_with_four_digits = T) {
  retval <- vector("character", length = length(x))
  index_not_na <- !is.na(x)
  retval[!index_not_na] <- "IK"

  if(!is.null(sig)) {
    retval[index_not_na] <- formatC(signif(x[index_not_na], digits = sig), big.mark = " ", decimal.mark = ",", format = "f", digits = digits)
  } else {
    retval[index_not_na] <- formatC(x[index_not_na], big.mark = " ", decimal.mark = ",", format = "f", digits = digits)
  }
  index <- which(x[index_not_na] >= 1000 & x[index_not_na] < 10000)
  if (length(index) > 0 & break_with_four_digits == F) retval[index_not_na][index] <- stringr::str_remove(retval[index_not_na][index], " ")
  return(retval)
}

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
    names(breaks) <- format_nor(breaks,
      digits = digits,
      break_with_four_digits = break_with_four_digits
    )
    breaks
  }
}

#' format_nor_num_0
#' Formats as a norwegian number with 0 digits.
#' Useful for scale labels
#' @param x value
#' @export
format_nor_num_0 <- function(x) csstyle::format_nor(x, digits = 0)

#' format_nor_num_1
#' Formats as a norwegian number with 1 digits.
#' Useful for scale labels
#' @param x value
#' @export
format_nor_num_1 <- function(x) csstyle::format_nor(x, digits = 1)

#' format_nor_num_2
#' Formats as a norwegian number with 2 digits.
#' Useful for scale labels
#' @param x value
#' @export
format_nor_num_2 <- function(x) csstyle::format_nor(x, digits = 2)


#' format_nor_num_0
#' Formats as a norwegian number with 0 digits and the suffix " /100k".
#' Useful for scale labels
#' @param x value
#' @export
format_nor_per100k_0 <- function(x){
  retval <- paste0(csstyle::format_nor(x, digits = 0), " /100k")
  retval[retval=="IK /100k"] <- "IK"
  return(retval)
}

#' format_nor_num_1
#' Formats as a norwegian number with 1 digits and the suffix " /100k"
#' Useful for scale labels
#' @param x value
#' @export
format_nor_per100k_1 <- function(x){
  retval <- paste0(csstyle::format_nor(x, digits = 1), " /100k")
  retval[retval=="IK /100k"] <- "IK"
  return(retval)
}

#' format_nor_num_2
#' Formats as a norwegian number with 2 digits and the suffix " /100k"
#' Useful for scale labels
#' @param x value
#' @export
format_nor_per100k_2 <- function(x){
  retval <- paste0(csstyle::format_nor(x, digits = 2), " /100k")
  retval[retval=="IK /100k"] <- "IK"
  return(retval)
}

#' format_nor_perc_0
#' Formats as a norwegian number with 0 digits and puts a % sign afterwards.
#' Useful for scale labels
#' @param x value
#' @export
format_nor_perc_0 <- function(x){
  retval <- paste0(csstyle::format_nor(x, digits = 0), " %")
  retval[retval=="IK %"] <- "IK"
  return(retval)
}

#' format_nor_perc_1
#' Formats as a norwegian number with 1 digits and puts a % sign afterwards.
#' Useful for scale labels
#' @param x value
#' @export
format_nor_perc_1 <- function(x){
  retval <- paste0(csstyle::format_nor(x, digits = 1), " %")
  retval[retval=="IK %"] <- "IK"
  return(retval)
}

#' format_nor_perc_2
#' Formats as a norwegian number with 2 digits and puts a % sign afterwards.
#' Useful for scale labels
#' @param x value
#' @export
format_nor_perc_2 <- function(x){
  retval <- paste0(csstyle::format_nor(x, digits = 2), " %")
  retval[retval=="IK %"] <- "IK"
  return(retval)
}


#' format_nor_invlog2_1
#' Formats as a norwegian number with 1 digit on log-2 scale.
#' Useful for scale labels
#' @param x value
#' @export
format_nor_invlog2_1 <- function(x){format_nor_num_1(2^x)}


#' format_nor_invlog2_2
#' Formats as a norwegian number with 2 digits on log-2 scale.
#' Useful for scale labels
#' @param x value
#' @export
format_nor_invlog2_2 <- function(x){format_nor_num_2(2^x)}


#' format_nor_invlog10_1
#' Formats as a norwegian number with 1 digit on log-10 scale.
#' Useful for scale labels
#' @param x value
#' @export
format_nor_invlog10_1 <- function(x){format_nor_num_1(10^x)}


#' format_nor_invlog10_2
#' Formats as a norwegian number with 2 digits on log-10 scale.
#' Useful for scale labels
#' @param x value
#' @export
format_nor_invlog10_2 <- function(x){format_nor_num_2(10^x)}


#' format_nor_invloge_1
#' Formats as a norwegian number with 1 digit on log scale.
#' Useful for scale labels
#' @param x value
#' @export
format_nor_invloge_1 <- function(x){format_nor_num_1(exp(x))}



#' format_nor_invloge_2
#' Formats as a norwegian number with 2 digits on log scale.
#' Useful for scale labels
#' @param x value
#' @export
format_nor_invloge_2 <- function(x){format_nor_num_2(exp(x))}






