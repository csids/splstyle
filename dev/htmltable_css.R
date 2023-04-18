#' CSS for htmltable table
#' @export
htmltable_css_table <- function() {
  "width: 100%; border-collapse: separate;"
}

#' CSS for htmltable header
#' @param font_size font_size
#' @param border_top Border top style
#' @export
htmltable_css_header <- function(font_size = 16, border_top = "2px solid grey") {
  retval <- glue::glue("font-size: {font_size}px; font-weight: bold; border-top: {border_top};")
  retval
}

#' CSS for htmltable cell
#' @param font_size font_size
#' @param border_bottom Border bottom style
#' @export
htmltable_css_cell <- function(font_size = 14, border_bottom = "1px dashed grey") {
  retval <- glue::glue("padding-left: .4em; padding-right: .2em; padding-top: .4em; padding-bottom: .4em; font-size: {font_size}px; border-bottom: {border_bottom};")
}

htmltable_css_cell_add_width <- function(css_cell, widths) {
  adding <- matrix(paste0("width: ", widths, "%;"), ncol = ncol(css_cell), nrow = nrow(css_cell), byrow = T)
  for (i in 1:ncol(css_cell)) css_cell[, i] <- paste0(css_cell[, i], adding[, i])
  return(css_cell)
}

#' Empty CSS style matrix
#' @param tab The table that gives the dimensions
#' @export
htmltable_css_style_matrix <- function(tab){
  retval <- matrix("", ncol=ncol(tab), nrow=nrow(tab))
  return(retval)
}

#' CSS for htmltable header
#' @param tab tab
#' @param widths Vector
#' @param css_style_matrix matrix containing css styles of the same dimensions as tab
#' @param n_cgroup The number of cgroups that are in your table
#' @param font_size_cell font size cell
#' @param font_size_header font size header
#' @export
htmltable_quick_style <- function(
  tab,
  widths = rep(round(100 / ncol(tab)), ncol(tab)),
  css_style_matrix = NULL,
  n_cgroup = 1,
  font_size_cell = 14,
  font_size_header = 16
  ) {
  if(!is.null(css_style_matrix)) stopifnot(identical(dim(tab), dim(css_style_matrix)))
  css_table <- htmltable_css_table()
  css_rgroup <- css_header <- htmltable_css_header(font_size = font_size_header)
  css_cgroup <- rep(htmltable_css_header(font_size = font_size_header), n_cgroup)
  css_cell <- matrix(
    htmltable_css_cell(font_size = font_size_cell),
    nrow = nrow(tab),
    ncol = ncol(tab)
  )

  css_cell <- htmltable_css_cell_add_width(css_cell, widths)
  if(!is.null(css_style_matrix)) for(i in 1:nrow(css_style_matrix)) for(j in 1:ncol(css_style_matrix)){
    if(is.null(css_style_matrix[i,j])) next
    if(css_style_matrix[i,j]=="") next
    css_cell[i,j] <- paste0(css_cell[i,j],css_style_matrix[i,j])
  }

  ui <- tab %>%
    htmlTable::addHtmlTableStyle(css.table = css_table) %>%
    htmlTable::addHtmlTableStyle(css.cgroup = css_cgroup) %>%
    htmlTable::addHtmlTableStyle(css.header = css_header) %>%
    htmlTable::addHtmlTableStyle(css.rgroup = css_rgroup) %>%
    htmlTable::addHtmlTableStyle(css.cell = css_cell) %>%
    htmlTable::addHtmlTableStyle(pos.caption = "bottom")
  return(ui)
}

#' CSS for htmltable cell with a risk scale from 1-5
#' @param x Risk value
#' @param font_size font_size
#' @param max_risk_value Max risk value
#' @param palette blue/red/green/grayblue/grayred/graygreen
#' @export
htmltable_css_risk_1_5 <- function(x, font_size = 14, max_risk_value = 5, palette = "blue") {
  stopifnot(x %in% 1:5)
  stopifnot(palette %in% c("blue", "red", "red", "grayblue", "grayred", "grayred"))
  color <- fhi_pal(palette, direction = 1)(max_risk_value)[x]
  retval <- htmltable_css_cell(font_size = font_size)
  retval <- glue::glue("{retval} background-color: {color};")
  if (x >= 4) retval <- paste0(retval, "color: white;")
  retval <- as.character(retval)
  return(retval)
}


#' CSS style for htmltable for yellow background
#' @export
htmltable_css_background_yellow <- htmltable_css_risk_1_5(3, palette = "red")

#' CSS style for htmltable for red background
#' @export
htmltable_css_background_red <- htmltable_css_risk_1_5(5, palette = "red")

