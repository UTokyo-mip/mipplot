#' Manual coloring
#'
#' Generate mapper from name of variable to name of color
#'
#' @param raw_table rule table which includes "Color_code" column.
#' @param category_separator regular expression for separating
#' right-hand-side variable name into categories.
#' For example: separator should be "\\|" for "Secondary Energy|Electricity|Coal"
#' @return named list of named string vectors.
#' for example,
#'
#'   result = list(
#'     "Emissions|CO2" = c(
#'       "Fossi Fuels and Industry" = "#17202a",
#'       "Land Use" = "#008000", ...),
#'     "Emissions|CO2|Fossil Fuels and Industry" = c(
#'       "Energy Demand" = "#444444", ...
#'     ),...
#'
#' @export

mipplot_generate_color_mapper <- function(raw_table, category_separator = "\\|") {
    ########################################################################
    # Add columns of variable names that contain only most detailed category
    # and parent category separately.
    #
    # c.f.
    # full_variable_name = common_part + deepest_category_part
    #
    #######################################################################

    COLUMN_OF_COMMON_PART <- 2
    COLUMN_OF_FULL_VARIABLE_NAME_PART <- 3
    COLUMN_OF_COLOR_CODE <- 4

    mapper <- list()

    aggregated_mapping <- c()

    for (i in 1:nrow(raw_table)) {

      # update common_part if common_part is available.
      if (raw_table[i, COLUMN_OF_COMMON_PART] != "") {
        common_part <- raw_table[i, COLUMN_OF_COMMON_PART]
      }

      # update full_variable_name if it is available.
      if (raw_table[i, COLUMN_OF_FULL_VARIABLE_NAME_PART] != "") {
        full_variable_name_part <- raw_table[i, COLUMN_OF_FULL_VARIABLE_NAME_PART]
      } else {
        next
      }

      # update color_code if color_code is available.
      if (raw_table[i, COLUMN_OF_COLOR_CODE] != "") {
        color_code <- raw_table[i, COLUMN_OF_COLOR_CODE]
      }

      # split category names like "a|b|c" to c("a", "b", "c")
      splitted_categories <- strsplit(full_variable_name_part, split = category_separator)[[1]]

      # get deepest_category_part
      deepest_category_part <- tail(splitted_categories, n = 1)

      # store color_code
      mapper[[common_part]][deepest_category_part] <- color_code

      # add color_code too to aggreated_mapping
      aggregated_mapping[deepest_category_part] <- color_code

    }

    # merge mapper and aggregated_mapping
    for (i in 1:length(mapper)) {
      for (j in 1:length(aggregated_mapping)) {
        additional_map <- aggregated_mapping[j]
        additional_variable_name <- names(additional_map)
        additional_color_code <- additional_map
        if (is.na(mapper[[i]][additional_variable_name])) {
          mapper[[i]][additional_variable_name] <- additional_color_code
        }
      }
    }

    return(mapper)
}
