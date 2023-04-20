#' ggplot2 theme
#' @param base_size a
#' @param base_family a
#' @param base_line_size a
#' @param base_rect_size a
#' @param legend_position a
#' @param x_axis_vertical FALSE
#' @param panel_on_top a
#' @param panel.grid.major.x Set to FALSE
#' @param panel.grid.minor.x Set to FALSE
#' @param panel.grid.major.y Set to FALSE
#' @param panel.grid.minor.y Set to FALSE
#' @rdname theme
#' @export
theme_cs <- function(
    base_size = 16,
    base_family = "",
    base_line_size = base_size / 22,
    base_rect_size = base_size / 22,
    legend_position = "right",
    x_axis_vertical = FALSE,
    panel_on_top = TRUE,
    panel.grid.major.x = FALSE,
    panel.grid.minor.x = FALSE,
    panel.grid.major.y = TRUE,
    panel.grid.minor.y = TRUE
  ) {
  half_line <- base_size / 2

  if(identical(panel.grid.major.x, TRUE)){
    panel.grid.major.x <- element_line(colour = "black", size = rel(0.1))
  } else if(identical(panel.grid.major.x, FALSE)){
    panel.grid.major.x <- element_blank()
  }

  if(identical(panel.grid.minor.x, TRUE)){
    panel.grid.minor.x <- element_line(colour = "black", size = rel(0.05))
  } else if(identical(panel.grid.minor.x, FALSE)){
    panel.grid.minor.x <- element_blank()
  }

  if(identical(panel.grid.major.y, TRUE)){
    panel.grid.major.y <- element_line(colour = "black", size = rel(0.1))
  } else if(identical(panel.grid.major.y, FALSE)){
    panel.grid.major.y <- element_blank()
  }

  if(identical(panel.grid.minor.y, TRUE)){
    panel.grid.minor.y <- element_line(colour = "black", size = rel(0.05))
  } else if(identical(panel.grid.minor.y, FALSE)){
    panel.grid.minor.y <- element_blank()
  }

  retval <- theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      axis.text = element_text(colour = "black", size = rel(0.8)),
      axis.ticks = element_line(colour = "black", size = rel(0.5)),
      axis.ticks.length = unit(rel(.25), "cm"),
      axis.title.x = element_text(margin = margin(t = base_size), vjust = 1),
      axis.title.y = element_text(angle = 90, margin = margin(r = base_size), vjust = 1),
      axis.line = element_line(colour = "black", size = rel(1)),
      panel.border = element_rect(
        fill = NA,
        colour = NA,
        size = rel(1)
      ),
      strip.background = element_rect(colour = "white", fill = "white"),
      panel.background = element_rect(fill = NA, colour = NA),
      panel.grid.major.x = panel.grid.major.x,
      panel.grid.minor.x = panel.grid.minor.x,
      panel.grid.major.y = panel.grid.major.y,
      panel.grid.minor.y = panel.grid.minor.y,
      panel.grid = element_line(),
      legend.position = legend_position,
      complete = TRUE
    )

  if (legend_position == "bottom") {
    retval <- retval %+replace%
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal"
      )
  }

  if(x_axis_vertical){
    retval <- retval %+replace%
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  }

  if (panel_on_top) {
    retval <- retval %+replace%
      theme(
        panel.background = element_rect(fill = NA, colour = NA),
        panel.ontop = TRUE
      )
  }

  return(retval)
}

#' set_x_axis_vertical
#' @import ggplot2
#' @rdname theme
#' @export
set_x_axis_vertical <- function() {
  return(theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)))
}

