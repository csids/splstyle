format_num_as_nor <- function(x, digits = 0, sig = NULL, break_with_four_digits = F) {
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

#' Formats as a norwegian number with 0 digits.
#' Useful for scale labels
#' @param x value
#' @returns Character vector
#' @rdname format_num_as_nor_num
#' @export
format_num_as_nor_num_0 <- function(x) format_num_as_nor(x, digits = 0)

#' Formats as a norwegian number with 1 digits.
#' Useful for scale labels
#' @param x value
#' @returns Character vector
#' @rdname format_num_as_nor_num
#' @export
format_num_as_nor_num_1 <- function(x) format_num_as_nor(x, digits = 1)

#' Formats as a norwegian number with 2 digits.
#' Useful for scale labels
#' @param x value
#' @returns Character vector
#' @rdname format_num_as_nor_num
#' @export
format_num_as_nor_num_2 <- function(x) format_num_as_nor(x, digits = 2)


#' Formats as a norwegian number with 0 digits and the suffix " /100k".
#' Useful for scale labels
#' @param x value
#' @returns Character vector
#' @rdname format_num_as_nor_per100k
#' @export
format_num_as_nor_per100k_0 <- function(x){
  retval <- paste0(format_num_as_nor(x, digits = 0), " /100k")
  retval[retval=="IK /100k"] <- "IK"
  return(retval)
}

#' Formats as a norwegian number with 1 digits and the suffix " /100k"
#' Useful for scale labels
#' @param x value
#' @returns Character vector
#' @rdname format_num_as_nor_per100k
#' @export
format_num_as_nor_per100k_1 <- function(x){
  retval <- paste0(format_num_as_nor(x, digits = 1), " /100k")
  retval[retval=="IK /100k"] <- "IK"
  return(retval)
}

#' Formats as a norwegian number with 2 digits and the suffix " /100k"
#' Useful for scale labels
#' @param x value
#' @returns Character vector
#' @rdname format_num_as_nor_per100k
#' @export
format_num_as_nor_per100k_2 <- function(x){
  retval <- paste0(format_num_as_nor(x, digits = 2), " /100k")
  retval[retval=="IK /100k"] <- "IK"
  return(retval)
}

#' Formats as a norwegian number with 0 digits and puts a % sign afterwards.
#' Useful for scale labels
#' @param x value
#' @returns Character vector
#' @rdname format_num_as_nor_perc
#' @export
format_num_as_nor_perc_0 <- function(x){
  retval <- paste0(format_num_as_nor(x, digits = 0), " %")
  retval[retval=="IK %"] <- "IK"
  return(retval)
}

#' Formats as a norwegian number with 1 digits and puts a % sign afterwards.
#' Useful for scale labels
#' @param x value
#' @returns Character vector
#' @rdname format_num_as_nor_perc
#' @export
format_num_as_nor_perc_1 <- function(x){
  retval <- paste0(format_num_as_nor(x, digits = 1), " %")
  retval[retval=="IK %"] <- "IK"
  return(retval)
}

#' Formats as a norwegian number with 2 digits and puts a % sign afterwards.
#' Useful for scale labels
#' @param x value
#' @returns Character vector
#' @rdname format_num_as_nor_perc
#' @export
format_num_as_nor_perc_2 <- function(x){
  retval <- paste0(format_num_as_nor(x, digits = 2), " %")
  retval[retval=="IK %"] <- "IK"
  return(retval)
}


#' Formats as a norwegian number with 1 digit on log-2 scale.
#' Useful for scale labels
#' @param x value
#' @returns Character vector
#' @rdname format_num_as_nor_invlog2
#' @export
format_num_as_nor_invlog2_1 <- function(x){format_num_as_nor_num_1(2^x)}


#' Formats as a norwegian number with 2 digits on log-2 scale.
#' Useful for scale labels
#' @param x value
#' @returns Character vector
#' @rdname format_num_as_nor_invlog2
#' @export
format_num_as_nor_invlog2_2 <- function(x){format_num_as_nor_num_2(2^x)}


#' Formats as a norwegian number with 1 digit on log-10 scale.
#' Useful for scale labels
#' @param x value
#' @returns Character vector
#' @rdname format_num_as_nor_invlog10
#' @export
format_num_as_nor_invlog10_1 <- function(x){format_num_as_nor_num_1(10^x)}


#' Formats as a norwegian number with 2 digits on log-10 scale.
#' Useful for scale labels
#' @param x value
#' @returns Character vector
#' @rdname format_num_as_nor_invlog10
#' @export
format_num_as_nor_invlog10_2 <- function(x){format_num_as_nor_num_2(10^x)}


#' Formats as a norwegian number with 1 digit on log scale.
#' Useful for scale labels
#' @param x value
#' @returns Character vector
#' @rdname format_num_as_nor_invloge
#' @export
format_num_as_nor_invloge_1 <- function(x){format_num_as_nor_num_1(exp(x))}



#' Formats as a norwegian number with 2 digits on log scale.
#' Useful for scale labels
#' @param x value
#' @returns Character vector
#' @rdname format_num_as_nor_invloge
#' @export
format_num_as_nor_invloge_2 <- function(x){format_num_as_nor_num_2(exp(x))}






