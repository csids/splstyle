#' An easy way to print github code
#' @param url URL from github
#' @examples
#' \dontrun{
#' x <- as_github_code("https://github.com/x/y/blob/main/file.txt")
#' print(x, lines = 3:5)
#' }
#' @returns Class github_code
#' @export
as_github_code <- function(url){
  if(stringr::str_detect(url, "^https://github.com/") | stringr::str_detect(url, "^https://www.github.com/")){
    url <- stringr::str_replace(url, "^https://www.github.com/", "https://raw.githubusercontent.com/")
    url <- stringr::str_replace(url, "^https://github.com/", "https://raw.githubusercontent.com/")
    url <- stringr::str_replace(url, "/blob/", "/")
  }
  pretty_url <- stringr::str_replace(url, "^https://raw.githubusercontent.com/", "https://github.com/")
  pretty_url <- stringr::str_split(pretty_url, "/")[[1]]
  pretty_url <- c(pretty_url[1:5], "blob", pretty_url[6:length(pretty_url)])
  pretty_url <- paste0(pretty_url, collapse="/")

  x <- readLines(url)
  attr(x, "pretty_url") <- pretty_url
  attr(x, "class") <- c("github_code", class(x))
  x
}

#' Print github_code
#' @param x an object
#' @param ... Further arguments
#' @returns x as invisible.
#' @export
print.github_code <- function(x, ...){
  dots <- list(...)
  if("lines" %in% names(dots)){
    lines <- dots[["lines"]]
  } else {
    lines <- seq_len(length(x))
  }

  min_lines <- min(lines)
  max_lines <- max(lines)

  if(min_lines == 1 & max_lines == length(x)){
    link <- attr(x, "pretty_url")
  } else {
    link <- paste0(attr(x, "pretty_url"),"#L",min_lines,"-L",max_lines)
  }

  if("include_url" %in% names(dots)) if(dots[["include_url"]] == TRUE) {
    cat(glue::glue("{link}\n\n\n"))
  }

  if("include_url_as_link" %in% names(dots)) if(dots[["include_url_as_link"]] == TRUE) {
    cat(glue::glue("<a href='{link}'>{link}</a>\n\n\n"))
  }

  max_width <- log(max_lines, base = 10) %>%
    floor() + 1
  for(i in lines){
    cat(formatC(i, width = max_width), " | ", x[i], "\n", sep = "")
  }
  invisible(x)
}
