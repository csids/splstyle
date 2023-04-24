#' plot_seasonal_timeseries
#' @param x Dataset
#' @param ... X
#' @rdname plot_seasonal_timeseries
#' @export
plot_seasonal_timeseries <- function(x,
                          ...) {
  UseMethod("plot_seasonal_timeseries", x)
}

#' plot_seasonal_timeseries
#'
#' If the dataset is already long it needs to include the following columns: variable, name_outcome and n.
#' @param x Dataset
#' @param var_x "date" or "isoyearweek"
#' @param var_y The name of the variable to use on the y-axis of the graph
#' @param breaks_x Use csstyle::every_nth() to choose how many ticks to show on the x-axis
#' @param breaks_y Use csstyle::pretty_breaks() to add ticks on the y-axis
#' @param lab_main The main title of the graph
#' @param lab_sub The subtitle of the graph
#' @param lab_caption If not specified, csstyle::fhi_caption() is used as the lab_caption.
#' @param lab_date How the dates on the x-axis should be formatted if var_x = "date"
#' @param lab_x The label of the x-axis
#' @param lab_y The label of the y-axis
#' @param lab_legend The label of the legend.
#' @param legend_position The position the legend should have. If not specified, "bottom" is used.
#' @param legend_direction layout of items in legend ("horizontal" or "vertical")
#' @param format_y How the y-axis ticks should be formatted. For example csstyle::format_nor_num_0 or csstyle::format_nor_perc_0
#' @param facet_wrap What column in the dataset to use to split the dataset.
#' @param facet_ncol How many columns with graphs if facet_wrap is used.
#' @param palette What palette to use for the lines. The default is "primary".
#' @param palette_dir 1 or -1.
#' @param scale_y How to scale the y-axis if the graph is split with facet_wrap. Free or fixed.
#' @param base_size The base size of the plot.
#' @param geom_point TRUE if points should be included in the graph.
#' @param fn_convert_isoyearweek_to_season A function that converts from isoyearweek to season
#' @param fn_convert_isoyearweek_to_seasonweek A function that converts from isoyearweek to seasonweek
#' @param fn_convert_seasonweek_to_isoweek A function that converts from seasonweek to isoweek
#' @param fn_convert_isoweek_to_seasonweek A function that convets from isoweek to seasonweek
#' @param ... Not currently used.
#' @rdname plot_seasonal_timeseries
#' @export
plot_seasonal_timeseries.default <- function(
  x,
  var_x = "isoyearweek",
  var_y,
  breaks_x = seq(1,52, 4),
  breaks_y = csstyle::pretty_breaks(6),
  lab_main = NULL,
  lab_sub = NULL,
  lab_caption = "hi",#fhi_caption(),
  lab_date = "%Y-%m-%d",
  lab_y = NULL,
  lab_x = NULL,
  lab_legend = NULL,
  legend_position = "bottom",
  legend_direction = "horizontal",
  format_y = csstyle::format_num_as_nor_num_0,
  facet_wrap = NULL,
  facet_ncol = NULL,
  palette = "primary",
  palette_dir = 1,
  scale_y = "free",
  base_size = 12,
  geom_point = FALSE,
  fn_convert_isoyearweek_to_season = cstime::isoyearweek_to_season_c,
  fn_convert_isoyearweek_to_seasonweek = cstime::isoyearweek_to_seasonweek_n,
  fn_convert_seasonweek_to_isoweek = cstime::seasonweek_to_isoweek_c,
  fn_convert_isoweek_to_seasonweek = cstime::isoweek_to_seasonweek_n,
  ...
  ) {

  pd <- copy(x)
  pd[, plot_season := fn_convert_isoyearweek_to_season(get(var_x))]
  pd[, plot_seasonweek := fn_convert_isoyearweek_to_seasonweek(get(var_x))]

  q <- ggplot(pd, aes_string(y = var_y, x = "plot_seasonweek", color = "plot_season"))
  q <- q + geom_line()

  if(geom_point) {
    q <- q + geom_point(aes(y = n))
  }

  if(identical(breaks_x, ggplot2::waiver())) breaks_x <- csstyle::every_nth(n = 4)
  breaks_x <- fn_convert_isoweek_to_seasonweek(breaks_x)
  q <- q + scale_x_continuous(
    name = lab_x,
    breaks = breaks_x,
    labels = fn_convert_seasonweek_to_isoweek
  )

  q <- q + scale_y_continuous(
    name = lab_y,
    breaks = breaks_y,
    expand = expand_scale(mult = c(0, 0.1)),
    labels = format_y
  )

  if(!is.null(facet_wrap)){
    q <- q + facet_wrap(~get(facet_wrap), ncol = facet_ncol)
    q <- q + annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)
    q <- q + annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)
    #q <- q + lemon::facet_rep_wrap(~get(facet_wrap), repeat.tick.labels = "y", scales = scale_y, ncol = facet_ncol)
  }

  # guide = guide_legend(ncol = 3)

  q <- q + expand_limits(y = 0)
  q <- q + scale_color_cs(lab_legend, palette = palette, direction = palette_dir)
  q <- q + labs(
    title = lab_main,
    subtitle = lab_sub,
    caption = lab_caption,
  )
  q <- q + theme_cs(
    legend_position = legend_position,
    base_size = base_size,
    x_axis_vertical = TRUE
  )
  # q <- q + theme(legend.direction = legend_direction)
  q

}
