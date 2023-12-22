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
#' @param x_var "date" or "isoyearweek"
#' @param y_var The name of the variable to use on the y-axis of the graph
#' @param x_breaks Use csstyle::every_nth() to choose how many ticks to show on the x-axis
#' @param y_breaks Use csstyle::pretty_breaks() to add ticks on the y-axis
#' @param y_labels How the y-axis ticks should be formatted. For example csstyle::format_nor_num_0 or csstyle::format_nor_perc_0
#' @param lab_title The main title of the graph
#' @param lab_subtitle The subtitle of the graph
#' @param lab_caption If not specified, csstyle::fhi_caption() is used as the lab_caption.
#' @param lab_date How the dates on the x-axis should be formatted if x_var = "date"
#' @param x_lab The label of the x-axis
#' @param y_lab The label of the y-axis
#' @param legend_lab The label of the legend.
#' @param legend_position The position the legend should have. If not specified, "bottom" is used.
#' @param legend_direction layout of items in legend ("horizontal" or "vertical")
#' @param facet_wrap What column in the dataset to use to split the dataset.
#' @param facet_ncol How many columns with graphs if facet_wrap is used.
#' @param scale_color What palette to use for the lines. The default is "primary".
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
  x_var = "isoyearweek",
  x_breaks = seq(1,52, 4),
  x_lab = "Isoweek",
  y_var,
  y_breaks = csstyle::pretty_breaks(6),
  y_lab = NULL,
  y_labels = csstyle::format_num_as_nor_num_0,
  lab_title = NULL,
  lab_subtitle = NULL,
  lab_caption = lubridate::today(),
  legend_lab = "Season",
  legend_position = "bottom",
  legend_direction = "horizontal",
  scale_color = csstyle::scale_color_cs(),
  facet_wrap = NULL,
  facet_ncol = NULL,
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
  pd[, plot_season := fn_convert_isoyearweek_to_season(get(x_var))]
  pd[, plot_seasonweek := fn_convert_isoyearweek_to_seasonweek(get(x_var))]

  q <- ggplot(pd, aes_string(y = y_var, x = "plot_seasonweek", color = "plot_season"))
  q <- q + geom_line()

  if(geom_point) {
    q <- q + geom_point(aes(y = n))
  }

  if(identical(x_breaks, ggplot2::waiver())) x_breaks <- csstyle::every_nth(n = 4)
  x_breaks <- fn_convert_isoweek_to_seasonweek(x_breaks)
  q <- q + scale_x_continuous(
    name = x_lab,
    breaks = x_breaks,
    labels = fn_convert_seasonweek_to_isoweek
  )

  q <- q + scale_y_continuous(
    name = y_lab,
    breaks = y_breaks,
    expand = expand_scale(mult = c(0, 0.1)),
    labels = y_labels
  )

  if(!is.null(facet_wrap)){
    q <- q + facet_wrap(~get(facet_wrap), ncol = facet_ncol)
    q <- q + annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)
    q <- q + annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)
    #q <- q + lemon::facet_rep_wrap(~get(facet_wrap), repeat.tick.labels = "y", scales = scale_y, ncol = facet_ncol)
  }

  # guide = guide_legend(ncol = 3)

  q <- q + expand_limits(y = 0)
  q <- q + scale_color
  q <- q + labs(
    title = lab_title,
    subtitle = lab_subtitle,
    caption = lab_caption,
    color = legend_lab
  )
  q <- q + theme_cs(
    legend_position = legend_position,
    base_size = base_size,
    x_axis_vertical = FALSE
  )
  # q <- q + theme(legend.direction = legend_direction)
  q

}
