DateBreaks <- function(breaksDF, limits, weekNumbers) {
  if (weekNumbers) {
    if (as.numeric(difftime(limits[2], limits[1], "days")) / 7 < 52 * 0.5) {
      desiredGap <- 2
    } else if (as.numeric(difftime(limits[2], limits[1], "days")) / 7 < 52 * 1) {
      desiredGap <- 2
    } else if (as.numeric(difftime(limits[2], limits[1], "days")) / 7 < 52 * 2) {
      desiredGap <- 4
    } else if (as.numeric(difftime(limits[2], limits[1], "days")) / 7 < 52 * 3) {
      desiredGap <- 13
    } else if (as.numeric(difftime(limits[2], limits[1], "days")) / 7 < 52 * 6) {
      desiredGap <- 26
    } else if (as.numeric(difftime(limits[2], limits[1], "days")) / 7 < 52 * 20) {
      desiredGap <- 50
    }

    desiredWeeks <- formatC(seq(1, 52 - desiredGap / 2 + 1, desiredGap), flag = "0", width = 2)
    breaksDF <- breaksDF[breaksDF$printWeek %in% desiredWeeks, ]
    breaksDF$printLabel <- paste0(breaksDF$printWeek, "/", breaksDF$printYear)
  } else {
    if (as.numeric(difftime(limits[2], limits[1], "days")) / 1 < 52 * 0.5) {
      desiredGap <- 2
    } else if (as.numeric(difftime(limits[2], limits[1], "days")) / 1 < 52 * 1) {
      desiredGap <- 2
    } else if (as.numeric(difftime(limits[2], limits[1], "days")) / 1 < 52 * 2) {
      desiredGap <- 7
    } else if (as.numeric(difftime(limits[2], limits[1], "days")) / 1 < 52 * 3) {
      desiredGap <- 14
    } else if (as.numeric(difftime(limits[2], limits[1], "days")) / 1 < 52 * 6) {
      desiredGap <- 14
    } else if (as.numeric(difftime(limits[2], limits[1], "days")) / 1 < 52 * 20) {
      desiredGap <- 30
    }

    desiredWeeks <- formatC(seq(1, 31 - desiredGap / 2 + 1, desiredGap), flag = "0", width = 2)
    breaksDF <- breaksDF[breaksDF$printDay %in% desiredWeeks, ]
    breaksDF$printLabel <- paste0(breaksDF$printDay, "/", breaksDF$printMonth)
  }
  return(breaksDF)
}

#' Creates a line threshold plot
#' @param pd A data.frame
#' @param x Name of the column in `pd` that is the `x` value
#' @param dataVal Name of the column in `pd` that is the `y` value
#' @param dataCIL Name of the column in `pd` that is the value of the lower confidence interval of the `y` value
#' @param dataCIU Name of the column in `pd` that is the value of the upper confidence interval of the `y` value
#' @param L1 Name of the column in `pd` that is the value of the first threshold
#' @param L2 Name of the column in `pd` that is the value of the second threshold
#' @param L3 Name of the column in `pd` that is the value of the third threshold
#' @param L4 Name of the column in `pd` that is the value of the fourth threshold
#' @param allPoints Should all points be displayed on the line?
#' @param title Title of the graph
#' @param pointShift How much will the points be shifted?
#' @param xShift How much will the x values be shifted?
#' @param weekNumbers Display week numbers?
#' @param step Step graph or normal line graph?
#' @param GetCols Vector of colours
#' @param legend_position a
#' @import ggplot2
#' @export
make_line_threshold_plot <- function(pd,
                                     x,
                                     dataVal,
                                     dataCIL = NULL,
                                     dataCIU = NULL,
                                     L1,
                                     L2,
                                     L3,
                                     L4,
                                     allPoints = TRUE,
                                     title = NULL,
                                     pointShift = 0,
                                     xShift = 0,
                                     weekNumbers = FALSE,
                                     step = FALSE,
                                     GetCols,
                                     legend_position = "right") {
  pd <- as.data.frame(pd)
  pd$printYear <- format.Date(pd[[x]], "%G")
  pd$printWeek <- format.Date(pd[[x]], "%V")
  pd$printMonth <- format.Date(pd[[x]], "%m")
  pd$printDay <- format.Date(pd[[x]], "%d")
  if (step) {
    pd$xShifted <- pd[[x]] + pointShift
    pd[[x]] <- pd[[x]] + xShift
  } else {
    pd$xShifted <- pd[[x]]
    pd[[x]] <- pd[[x]]
  }
  includeMedium <- nrow(pd[pd$status == "Medium", ]) > 0
  includeHigh <- nrow(pd[pd$status == "High", ]) > 0

  colours <- NULL
  if (includeHigh) colours <- c(colours, GetCols()[1])
  if (includeMedium) colours <- c(colours, GetCols()[2])

  limits <- range(pd[[x]])
  limitsSize <- max(1, (limits[2] - limits[1]) * 0.005)
  limits[1] <- limits[1] - limitsSize
  limits[2] <- limits[2] + limitsSize

  limitsY <- diff(range(c(pd[[L1]], pd[[L4]])))

  q <- ggplot(pd, aes_string(x = x))
  if (step) {
    q <- q + geom_stepribbon(aes_string(ymin = L3, ymax = L4, fill = shQuote("L1")), direction = "vh", alpha = 1)
    q <- q + geom_stepribbon(aes_string(ymin = L2, ymax = L3, fill = shQuote("L2")), direction = "vh", alpha = 1)
    q <- q + geom_stepribbon(aes_string(ymin = L1, ymax = L2, fill = shQuote("L3")), direction = "vh", alpha = 1)
    if (!is.null(dataCIL) & !is.null(dataCIU)) q <- q + geom_stepribbon(aes_string(ymin = dataCIL, ymax = dataCIU), fill = "black", direction = "vh", alpha = 1)
    q <- q + geom_step(aes_string(y = dataVal), direction = "vh", lwd = 1)
  } else {
    q <- q + geom_ribbon(aes_string(ymin = L3, ymax = L4, fill = shQuote("L1")), alpha = 1)
    q <- q + geom_ribbon(aes_string(ymin = L2, ymax = L3, fill = shQuote("L2")), alpha = 1)
    q <- q + geom_ribbon(aes_string(ymin = L1, ymax = L2, fill = shQuote("L3")), alpha = 1)
    if (!is.null(dataCIL) & !is.null(dataCIU)) q <- q + geom_ribbon(aes_string(ymin = dataCIL, ymax = dataCIU), fill = "black", alpha = 1)
    q <- q + geom_line(aes_string(y = dataVal), lwd = 1)
  }
  if ("low_n" %in% colnames(pd)) {
    q <- q + geom_ribbon(aes(ymin = low_n, ymax = high_n, fill = "test"), alpha = 0.4)
  }
  if (allPoints) {
    q <- q + geom_point(aes_string(x = "xShifted", y = dataVal), size = 4, fill = "black")
  } else {
    if (includeMedium | includeHigh) q <- q + geom_point(aes_string(x = "xShifted", y = dataVal), size = 4, fill = "black", data = pd[pd$status %in% c("Medium", "High"), ])
  }
  if (includeMedium) q <- q + geom_point(aes_string(x = "xShifted", y = dataVal, colour = shQuote("L2")), size = 2, data = pd[pd$status == "Medium", ])
  if (includeHigh) q <- q + geom_point(aes_string(x = "xShifted", y = dataVal, colour = shQuote("L1")), size = 2, data = pd[pd$status == "High", ])
  q <- q + theme_fhi_lines(legend_position = legend_position)



  breaksDF <- pd[pd$printWeek != "", ]
  breaksDF <- DateBreaks(breaksDF, limits, weekNumbers)

  q <- q + scale_x_date("", breaks = breaksDF$xShifted, labels = breaksDF$printLabel)
  # q <- q + scale_xcontinuous("Dato", breaks = breaksDF$xShifted,  labels = breaksDF$printLabel)

  q <- q + scale_y_continuous("")
  fill_labels <- c(
    glue::glue("Betydelig h{csdata::nb$oe}syere enn forventet"),
    glue::glue("H{csdata::nb$oe}syere enn forventet"),
    "Forventet"
  )
  fill_values <- GetCols()
  if ("low_n" %in% colnames(pd)) {
    fill_labels <- c(fill_labels, "Usikkerhet fra manglende kompletthet")
    fill_values <- c(fill_values, "grey")
  }
  q <- q + scale_fill_manual("", values = fill_values, labels = fill_labels)
  if (!is.null(colours)) q <- q + scale_colour_manual(values = colours)
  q <- q + guides(colour = FALSE)
  q <- q + coord_cartesian(xlim = limits, expand = FALSE)
  if (!is.null(title)) q <- q + labs(title = title)
  return(q)
}

#' MakeLineBrushPlot
#' @param pd a
#' @param x a
#' @param dataVal a
#' @param L2 a
#' @param L3 a
#' @param GetCols a
#' @import ggplot2
#' @export
make_line_brush_plot <- function(pd, x, dataVal, L2, L3, GetCols) {
  pd <- as.data.frame(pd)
  pd$printYear <- format.Date(pd[[x]], "%G")
  pd$printWeek <- format.Date(pd[[x]], "%V")
  pd$printMonth <- format.Date(pd[[x]], "%m")
  pd$printDay <- format.Date(pd[[x]], "%d")

  includeHigh <- sum(pd$status == "High") > 0
  includeMedium <- sum(pd$status == "Medium") > 0
  includeNormal <- sum(pd$status == "Normal") > 0

  colours <- NULL
  if (includeHigh) colours <- c(colours, GetCols()[1])
  if (includeMedium) colours <- c(colours, GetCols()[2])

  limitsX <- range(pd[[x]])
  limitsSize <- limitsX[2] - limitsX[1]
  limitsX[1] <- limitsX[1] - limitsSize * 0.005
  limitsX[2] <- limitsX[2] + limitsSize * 0.005

  limitsY <- range(pd[[dataVal]])
  limitsSize <- limitsY[2] - limitsY[1]
  limitsY[1] <- limitsY[1] - limitsSize * 0.05
  limitsY[2] <- limitsY[2] + limitsSize * 0.05

  limits <- range(pd[[x]])
  breaksDF <- pd[pd$printWeek != "", ]
  breaksDF <- DateBreaks(breaksDF, limits, weekNumbers = TRUE)

  q <- ggplot(pd, aes_string(x = x))
  q <- q + geom_line(aes_string(y = dataVal), lwd = 1)
  if (includeMedium | includeHigh) q <- q + geom_point(aes_string(y = dataVal), size = 4, fill = "black", data = pd[pd$status %in% c("Medium", "High"), ])
  if (includeMedium) q <- q + geom_point(aes_string(y = dataVal, colour = shQuote("L2")), size = 2, data = pd[pd$status == "Medium", ])
  if (includeHigh) q <- q + geom_point(aes_string(y = dataVal, colour = shQuote("L1")), size = 2, data = pd[pd$status == "High", ])
  q <- q + theme_fhi_lines()
  q <- q + scale_x_date("", breaks = breaksDF[[x]], labels = breaksDF$printLabel)
  q <- q + scale_y_continuous("", breaks = NULL)
  if (!is.null(colours)) q <- q + scale_colour_manual(values = colours)
  q <- q + guides(colour = FALSE)
  q <- q + coord_cartesian(xlim = limitsX, ylim = limitsY, expand = FALSE)
  q <- q + labs(title = "Velg tidsintervall")
  return(q)
}
