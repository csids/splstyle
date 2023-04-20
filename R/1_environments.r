global <- new.env()
global$location_code_to_factor_label <- "location_name"
global$location_code_to_factor_label_if_not_unique <- "location_name_description_nb"

#' set_global
#' @param location_code_to_factor_label The label
#' @param location_code_to_factor_label_if_not_unique The label if something is not unique with the first laben
#' @export
set_global <- function(
  location_code_to_factor_label = "location_name",
  location_code_to_factor_label_if_not_unique = "location_name_description_nb"
  ){
  global$location_code_to_factor_label <- location_code_to_factor_label
  global$location_code_to_factor_label_if_not_unique <- location_code_to_factor_label_if_not_unique
}

#' Colors
#' @export
colors <- new.env()

colors$v2$primary <- c(
  "H1" = "#393C61",
  "H2" = "#0975B5",
  "H3" = "#2EA1C0",
  "H4" = "#709900",
  "H5" = "#B11643",
  "H6" = "#FC5F56",
  "H7" = "#F7B500"
)

colors$v2$neutral <- c(
  "S1" = "#3C515E",
  "S2" = "#566F7B",
  "S3" = "#A9B7BF",
  "S4" = "#C5D0D4"
)

colors$v2$red <- c(
  "R1" = "#F6BDC0",
  "R2" = "#F1959B",
  "R3" = "#F07470",
  "R4" = "#E2433D",
  "R5" = "#B73932"
)

colors$v2$green <- c(
  "G1" = "#DBFA9F",
  "G2" = "#AED267",
  "G3" = "#6D9926",
  "G4" = "#4D7E13",
  "G5" = "#2C6300"
)

# misc
colors$v2$misc <- c(
  "M1" = "#ebf4fc" # lightblue
)

colors$v2$map_sequential <- c(
  "MS1" = "#045a8d",
  "MS2" = "#467fa9",
  "MS3" = "#76a5c4",
  "MS4" = "#9fcfef",
  "MS5" = "#c5dcdf"
)


colors$v2$map_divergent <- c(
  "MD1" = "#2c6300",
  "MD2" = "#4d7e13",
  "MD3" = "#6d9926",
  "MD4" = "#aed267"
)


colors$v1$primary <- c(
  "B1" = "#393C61",
  "B2" = "#0F6986",
  "B3" = "#4A9DAB",
  "B4" = "#38BCD7",
  "B5" = "#93CAE3",
  "B6" = "#92B147"
)

colors$v1$secondary <- c(
  "R1" = "#6A1445",
  "R2" = "#B10640",
  "R3" = "#ED6F67",
  "R4" = "#F8B766",
  "R5" = "#FEE090"
)

colors$v1$neutral <- c(
  "X1" = "#38474E",
  "X2" = "#556E7A",
  "X3" = "#79909A",
  "X4" = "#ABB9C1",
  "X5" = "#CFD8DB"
)

colors$v1$map_sequential <- c(
  "MS1" = "#0F6896",
  "MS2" = "#4486A8",
  "MS3" = "#6FA5BA",
  "MS4" = "#9BC3CE",
  "MS5" = "#C8E1E4"
)

colors$v1$map_divergent <- c(
  "MD1" = "#C95851",
  "MD2" = "#E6655C",
  "MD3" = "#EE8A7F",
  "MD4" = "#F3ACA3"
)


vals <- new.env()
vals$pals <- list()

# main sequential maps

vals$pals$map_seq_complete_1 <- colors$v2$map_sequential[1]
vals$pals$map_seq_complete_2 <- colors$v2$map_sequential[c(1, 4)]
vals$pals$map_seq_complete_3 <- colors$v2$map_sequential[c(1, 3, 4)]
vals$pals$map_seq_complete_4 <- colors$v2$map_sequential[c(1, 2, 3, 4)]
vals$pals$map_seq_complete_5 <- colors$v2$map_sequential[c(1, 2, 3, 4, 5)]

vals$pals$blue_1 <- rev(colors$v2$map_sequential)[1]
vals$pals$blue_2 <- rev(colors$v2$map_sequential)[c(1, 4)]
vals$pals$blue_3 <- rev(colors$v2$map_sequential)[c(1, 3, 4)]
vals$pals$blue_4 <- rev(colors$v2$map_sequential)[c(1, 2, 3, 4)]
vals$pals$blue_5 <- rev(colors$v2$map_sequential)[c(1, 2, 3, 4, 5)]

vals$pals$grayblue_1 <- colors$v2$neutral["S3"]
vals$pals$grayblue_2 <- c(colors$v2$neutral["S3"], rev(colors$v2$map_sequential)[c(4)])
vals$pals$grayblue_3 <- c(colors$v2$neutral["S3"], rev(colors$v2$map_sequential)[c(3, 4)])
vals$pals$grayblue_4 <- c(colors$v2$neutral["S3"], rev(colors$v2$map_sequential)[c(2, 3, 4)])
vals$pals$grayblue_5 <- c(colors$v2$neutral["S3"], rev(colors$v2$map_sequential)[c(2, 3, 4, 5)])

vals$pals$red_1 <- colors$v2$red[1]
vals$pals$red_2 <- colors$v2$red[c(1, 4)]
vals$pals$red_3 <- colors$v2$red[c(1, 3, 4)]
vals$pals$red_4 <- colors$v2$red[c(1, 2, 3, 4)]
vals$pals$red_5 <- colors$v2$red[c(1, 2, 3, 4, 5)]

vals$pals$grayred_1 <- c(colors$v2$neutral["S3"])
vals$pals$grayred_2 <- c(colors$v2$neutral["S3"], colors$v2$red[c(4)])
vals$pals$grayred_3 <- c(colors$v2$neutral["S3"], colors$v2$red[c(3, 4)])
vals$pals$grayred_4 <- c(colors$v2$neutral["S3"], colors$v2$red[c(2, 3, 4)])
vals$pals$grayred_5 <- c(colors$v2$neutral["S3"], colors$v2$red[c(2, 3, 4, 5)])

vals$pals$green_1 <- colors$v2$green[1]
vals$pals$green_2 <- colors$v2$green[c(1, 4)]
vals$pals$green_3 <- colors$v2$green[c(1, 3, 4)]
vals$pals$green_4 <- colors$v2$green[c(1, 2, 3, 4)]
vals$pals$green_5 <- colors$v2$green[c(1, 2, 3, 4, 5)]

vals$pals$graygreen_1 <- c(colors$v2$neutral["S3"])
vals$pals$graygreen_2 <- c(colors$v2$neutral["S3"], colors$v2$green[c(4)])
vals$pals$graygreen_3 <- c(colors$v2$neutral["S3"], colors$v2$green[c(3, 4)])
vals$pals$graygreen_4 <- c(colors$v2$neutral["S3"], colors$v2$green[c(2, 3, 4)])
vals$pals$graygreen_5 <- c(colors$v2$neutral["S3"], colors$v2$green[c(2, 3, 4, 5)])

vals$pals$map_seq_missing_1 <- colors$v2$primary["H7"]
vals$pals$map_seq_missing_2 <- c(colors$v2$map_sequential[1], colors$v2$primary["H7"])
vals$pals$map_seq_missing_3 <- c(colors$v2$map_sequential[c(1, 4)], colors$v2$primary["H7"])
vals$pals$map_seq_missing_4 <- c(colors$v2$map_sequential[c(1, 3, 4)], colors$v2$primary["H7"])
vals$pals$map_seq_missing_5 <- c(colors$v2$map_sequential[c(1, 2, 3, 4)], colors$v2$primary["H7"])
vals$pals$map_seq_missing_6 <- c(colors$v2$map_sequential[c(1, 2, 3, 4, 5)], colors$v2$primary["H7"])

vals$pals$map_seq_missing_x2_1 <- colors$v2$neutral["S3"]
vals$pals$map_seq_missing_x2_2 <- c(colors$v2$map_sequential[1], colors$v2$neutral["S3"])
vals$pals$map_seq_missing_x2_3 <- c(colors$v2$map_sequential[c(1, 4)], colors$v2$neutral["S3"])
vals$pals$map_seq_missing_x2_4 <- c(colors$v2$map_sequential[c(1, 3, 4)], colors$v2$neutral["S3"])
vals$pals$map_seq_missing_x2_5 <- c(colors$v2$map_sequential[c(1, 2, 3, 4)], colors$v2$neutral["S3"])
vals$pals$map_seq_missing_x2_6 <- c(colors$v2$map_sequential[c(1, 2, 3, 4, 5)], colors$v2$neutral["S3"])


# misc

vals$pals$map_seq_missing_x3_1 <- colors$v2$misc["M1"]
vals$pals$map_seq_missing_x3_2 <- c(colors$v2$map_sequential[1], colors$v2$misc["M1"])
vals$pals$map_seq_missing_x3_3 <- c(colors$v2$map_sequential[c(1, 4)], colors$v2$misc["M1"])
vals$pals$map_seq_missing_x3_4 <- c(colors$v2$map_sequential[c(1, 3, 4)], colors$v2$misc["M1"])
vals$pals$map_seq_missing_x3_5 <- c(colors$v2$map_sequential[c(1, 2, 3, 4)], colors$v2$misc["M1"])
vals$pals$map_seq_missing_x3_6 <- c(colors$v2$map_sequential[c(1, 2, 3, 4, 5)], colors$v2$misc["M1"])


# secondary sequantial maps

vals$pals$map_seq_x2_complete_1 <- colors$v2$map_divergent[1]
vals$pals$map_seq_x2_complete_2 <- colors$v2$map_divergent[c(1, 4)]
vals$pals$map_seq_x2_complete_3 <- colors$v2$map_divergent[c(1, 3, 4)]
vals$pals$map_seq_x2_complete_4 <- colors$v2$map_divergent[c(1, 2, 3, 4)]

vals$pals$map_seq_x2_missing_1 <- colors$v2$primary["H7"]
vals$pals$map_seq_x2_missing_2 <- c(colors$v2$map_divergent[1], colors$v2$primary["H7"])
vals$pals$map_seq_x2_missing_3 <- c(colors$v2$map_divergent[c(1, 4)], colors$v2$primary["H7"])
vals$pals$map_seq_x2_missing_4 <- c(colors$v2$map_divergent[c(1, 3, 4)], colors$v2$primary["H7"])
vals$pals$map_seq_x2_missing_5 <- c(colors$v2$map_divergent[c(1, 2, 3, 4)], colors$v2$primary["H7"])

vals$pals$map_seq_x2_missing_x2_1 <- colors$v2$neutral["S3"]
vals$pals$map_seq_x2_missing_x2_2 <- c(colors$v2$map_divergent[1], colors$v2$neutral["S3"])
vals$pals$map_seq_x2_missing_x2_3 <- c(colors$v2$map_divergent[c(1, 4)], colors$v2$neutral["S3"])
vals$pals$map_seq_x2_missing_x2_4 <- c(colors$v2$map_divergent[c(1, 3, 4)], colors$v2$neutral["S3"])
vals$pals$map_seq_x2_missing_x2_5 <- c(colors$v2$map_divergent[c(1, 2, 3, 4)], colors$v2$neutral["S3"])

# divergent maps

vals$pals$map_div_complete_1 <- c(
  colors$v2$map_sequential["MS1"]
)
vals$pals$map_div_complete_2 <- c(
  colors$v2$map_sequential["MS1"],
  colors$v2$map_divergent["MD1"]
)
vals$pals$map_div_complete_3 <- c(
  colors$v2$map_sequential["MS1"],
  colors$v2$neutral["S3"],
  colors$v2$map_divergent["MD1"]
)
vals$pals$map_div_complete_4 <- c(
  colors$v2$map_sequential["MS1"],
  colors$v2$map_sequential["MS2"],
  colors$v2$map_divergent["MD2"],
  colors$v2$map_divergent["MD1"]
)
vals$pals$map_div_complete_5 <- c(
  colors$v2$map_sequential["MS1"],
  colors$v2$map_sequential["MS2"],
  colors$v2$neutral["S3"],
  colors$v2$map_divergent["MD2"],
  colors$v2$map_divergent["MD1"]
)

vals$pals$map_div_missing_1 <- c(
  colors$v2$map_sequential["MS1"]
)
vals$pals$map_div_missing_2 <- c(
  colors$v2$map_sequential["MS1"],
  colors$v2$primary["H7"]
)
vals$pals$map_div_missing_3 <- c(
  colors$v2$map_sequential["MS1"],
  colors$v2$map_divergent["MD1"],
  colors$v2$primary["H7"]
)
vals$pals$map_div_missing_4 <- c(
  colors$v2$map_sequential["MS1"],
  colors$v2$neutral["S3"],
  colors$v2$map_divergent["MD1"],
  colors$v2$primary["H7"]
)
vals$pals$map_div_missing_5 <- c(
  colors$v2$map_sequential["MS1"],
  colors$v2$map_sequential["MS2"],
  colors$v2$map_divergent["MD2"],
  colors$v2$map_divergent["MD1"],
  colors$v2$primary["H7"]
)
vals$pals$map_div_missing_6 <- c(
  colors$v2$map_sequential["MS1"],
  colors$v2$map_sequential["MS2"],
  colors$v2$neutral["S3"],
  colors$v2$map_divergent["MD2"],
  colors$v2$map_divergent["MD1"],
  colors$v2$primary["H7"]
)

vals$pals$primary_1 <- c(
  colors$v2$primary["H1"]
)
vals$pals$primary_2 <- c(
  vals$pals$primary_1,
  colors$v2$primary["H4"]
)
vals$pals$primary_3 <- c(
  vals$pals$primary_2,
  colors$v2$primary["H2"]
)
vals$pals$primary_4 <- c(
  vals$pals$primary_3,
  colors$v2$primary["H6"]
)
vals$pals$primary_5 <- c(
  vals$pals$primary_4,
  colors$v2$primary["H3"]
)
vals$pals$primary_6 <- c(
  vals$pals$primary_5,
  colors$v2$primary["H5"]
)
vals$pals$primary_7 <- c(
  vals$pals$primary_6,
  colors$v2$primary["H7"]
)
vals$pals$primary_8 <- c(
  vals$pals$primary_7,
  colors$v2$neutral["S3"]
)
vals$pals$primary_9 <- c(
  vals$pals$primary_8,
  colors$v2$neutral["S1"]
)
vals$pals$primary_10 <- c(
  vals$pals$primary_9,
  colors$v2$neutral["S4"]
)
vals$pals$primary_11 <- c(
  vals$pals$primary_10,
  colors$v2$neutral["S2"]
)
vals$pals$primary_12 <- c(
  vals$pals$primary_11,
  colors$v2$green["G1"]
)

# warning - low medium high
vals$pals$warning_3 <- c(
  colors$v2$primary["H3"],
  colors$v2$primary["H7"],
  colors$v2$primary["H6"]
)

# posneg - yes-no
vals$pals$posneg_1 <- c(
  colors$v2$primary["H6"]
)

vals$pals$posneg_2 <- c(
  colors$v2$primary["H3"],
  colors$v2$primary["H6"]
)

vals$pals$contrast_1 <- c(
  colors$v2$primary["H1"]
)
vals$pals$contrast_2 <- c(
  colors$v2$primary["H1"],
  colors$v2$primary["H5"]
)
vals$pals$contrast_3 <- c(
  colors$v2$primary["H1"],
  colors$v2$neutral["S3"],
  colors$v2$primary["H5"]
)
vals$pals$contrast_4 <- c(
  colors$v2$primary["H1"],
  colors$v2$primary["H2"],
  colors$v2$primary["H7"],
  colors$v2$primary["H5"]
)
vals$pals$contrast_5 <- c(
  colors$v2$primary["H1"],
  colors$v2$primary["H2"],
  colors$v2$neutral["S3"],
  colors$v2$primary["H7"],
  colors$v2$primary["H5"]
)
vals$pals$contrast_6 <- c(
  colors$v2$primary["H1"],
  colors$v2$primary["H2"],
  colors$v2$primary["H3"],
  colors$v2$primary["H7"],
  colors$v2$primary["H6"],
  colors$v2$primary["H5"]
)
vals$pals$contrast_7 <- c(
  colors$v2$primary["H1"],
  colors$v2$primary["H2"],
  colors$v2$primary["H3"],
  colors$v2$neutral["S3"],
  colors$v2$primary["H7"],
  colors$v2$primary["H6"],
  colors$v2$primary["H5"]
)

vals$pals$combination_1 <- c(
  colors$v1$primary["R2"]
)
vals$pals$combination_2 <- c(
  colors$v1$primary["R2"],
  colors$v1$secondary["B2"]
)
vals$pals$combination_3 <- c(
  colors$v1$secondary["R2"],
  colors$v1$primary["B2"],
  colors$v1$primary["R4"]
)
vals$pals$combination_4 <- c(
  colors$v1$secondary["R2"],
  colors$v1$primary["B2"],
  colors$v1$secondary["R4"],
  colors$v1$primary["B4"]
)
vals$pals$combination_5 <- c(
  colors$v1$secondary["R2"],
  colors$v1$primary["B2"],
  colors$v1$secondary["R4"],
  colors$v1$primary["B4"],
  colors$v1$primary["B6"]
)

vals$palettes <- c(
  "combination",
  "map_div_complete",
  "map_div_missing",
  "map_seq_x2_missing_x2",
  "map_seq_x2_missing",
  "map_seq_x2_complete",
  "map_seq_missing_x3",
  "map_seq_missing_x2",
  "map_seq_missing",
  "map_seq_complete",
  "grayblue",
  "grayred",
  "graygreen",
  "blue",
  "red",
  "green",
  "contrast",
  "posneg",
  "warning",
  "primary"
)

#' Base color
#' @export base_color
base_color <- colors$v2$primary["H1"]

#' Warning color
#' @export warning_color
warning_color <- c(
  "low" = colors$v2$primary["H3"],
  "med" = colors$v2$primary["H7"],
  "hig" = colors$v2$primary["H6"]
)
names(warning_color) <- c("low", "med", "hig")
