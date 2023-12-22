#' plot_timeseries
#' @param x Dataset
#' @param ... X
#' @rdname plot_timeseries
#' @export
plot_timeseries <- function(x,
                          ...) {
  UseMethod("plot_timeseries", x)
}

#' plot_timeseries
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
#' @param wide_table TRUE if the data.table is wide and FALSE if the data.table is long.
#' @param var_group variable to group by
#' @param geom_point TRUE if points should be included in the graph.
#' @param ... Not currently used.
#' @rdname plot_timeseries
#' @examples
#' # csstyle::plot_timeseries(
#' # nor_covid19_cases_by_time_location[granularity_geo == "nation" & granularity_time == "isoweek"], var_y = c("Covid cases" = "covid19_cases_testdate_n"), breaks_x = every_nth(8), breaks_y = csstyle::pretty_breaks(5))
#' # plot_timeseries(nor_covid19_cases_by_time_location[location_code %in% c("county_nor03", "county_nor18", "county_nor30", "county_nor54") & granularity_time == "isoweek"], var_y = c("Covid cases" = "covid19_cases_testdate_n"), breaks_x = every_nth(8), breaks_y = csstyle::pretty_breaks(5), facet_wrap = "location_code")
#' # plot_timeseries(nor_covid19_cases_by_time_location[granularity_geo == "county" & granularity_time == "isoweek"], var_y = c("Covid cases" = "covid19_cases_testdate_n"), breaks_x = every_nth(8), breaks_y = csstyle::pretty_breaks(5), var_group = "location_code")
#' @export
plot_timeseries.default <- function(
  x,
  var_x,
  var_y,
  breaks_x = ggplot2::waiver(),
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
  wide_table = TRUE,
  var_group = NULL,
  geom_point = FALSE,
  ...
  ) {

  n <- NULL
  name_outcome <- NULL

  if(wide_table){
    d <- melt(x,
              id.vars = c(facet_wrap, var_x, var_group),
              measure.vars = list(n = var_y),
              value.name = "n"
    )

    d_name <- data.table(name_outcome= names(var_y), variable = var_y)

    d <- d_name[d, on = 'variable']
  } else {
    d <- copy(x)
    d[, n := get(var_y)]
  }

  # if(!"name_outcome" %in% names(d)){
  #   stop("name_outcome is not a column in x")
  # }

  # if(!"variable" %in% names(d)){
  #   stop("variable is not a column in x")
  # }

  if(!"n" %in% names(d)){
    stop("n is not a column in x")
  }

  if(is.null(var_group)){
    q <- ggplot(d, aes_string(x = var_x))
    q <- q + geom_path(aes(y = n, color = name_outcome, group = name_outcome), lwd = 1)
  } else {
    q <- ggplot(d, aes_string(x = var_x, color = var_group, group = var_group))
    q <- q + geom_path(aes(y = n), lwd = 1)
  }

  if(geom_point) {
    q <- q + geom_point(aes(y = n))
  }

  if(var_x == "date"){
    q <- q + scale_x_date(name = lab_x, date_labels = lab_date, breaks = breaks_x)
  } else if(var_x == "isoyearweek") {
    if(identical(breaks_x, ggplot2::waiver())) breaks_x <- csstyle::every_nth(n = 4)
    q <- q + scale_x_discrete(name = lab_x, breaks = breaks_x)
  } else if(var_x == "seasonweek") {
    if(identical(breaks_x, ggplot2::waiver())) breaks_x <- csstyle::every_nth(n = 4)
    q <- q + scale_x_continuous(name = lab_x, breaks = breaks_x, labels = cstime::seasonweek_to_isoweek_c)
  }

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
