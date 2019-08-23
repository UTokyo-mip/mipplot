#' @title correct data format of given IAMC data table
#' @description Dataset in IAMC format rule is not rigid.
#' This function corrects data types of columns in the dataset.
#' If necessary columns is missing, it throws exception.
#' Output object of this function is as follows:
#'
#' type: data.table
#' columns:
#'   model:    factor
#'   scenario: factor
#'   region:   factor
#'   variable: factor
#'   unit:     factor
#'   period:   double
#'   value:    double
#' @export
correct_format_of_iamc_dataframe <- function(iamc_data) {

  check_format_of_iamc_dataframe(iamc_data)
  corrected_iamc_data <- change_data_types_of_iamc_dataframe(iamc_data)

  return(corrected_iamc_data);
}

#' @title check if the format of given data is valid
#' as an IAMC dataset.
check_format_of_iamc_dataframe <- function(iamc_data) {

  check_column_availability(iamc_data)

}

#' @title check if the dataset has required fields of IAMC dataset
check_column_availability <- function(iamc_data) {

  # list of required field of IAMC data.
  NECESSARY_COLUMNS = c(
    "model", "scenario", "region", "variable", "unit", "period", "value")

  # check availability of columns
  for (necessary_column in NECESSARY_COLUMNS) {
    if (!(necessary_column %in% names(iamc_data))) {
      stop(sprintf("iamc_data doesn't contain %s column", necessary_column))
    }
  }
}

#' @title change column data type in dataset to be able to
#' be treated as an IAMC dataset.
change_data_types_of_iamc_dataframe <- function(iamc_data) {

  # convert data type
  iamc_data$model <- as.factor(iamc_data$model)
  iamc_data$scenario <- as.factor(iamc_data$scenario)
  iamc_data$region <- as.factor(iamc_data$region)
  iamc_data$variable <- as.factor(iamc_data$variable)
  iamc_data$period <- as.double(iamc_data$period)
  iamc_data$unit <- as.factor(iamc_data$unit)

  return(iamc_data)
}

#' @title Add credit text to a list of ggplot2 plot object
add_credit_to_list_of_plot <- function(list_of_plot) {
  return(
    lapply(list_of_plot, add_credit_to_plot)
  )
}

#' @title Add credit text to a ggplot2 plot object
add_credit_to_plot <- function(plot_object) {
  return (
    plot_object +

      # credit text
      ggplot2::labs(
        caption = "Created with https://github.com/UTokyo-mip/mipplot") +

      # formatting of the text
      ggplot2::theme(
        plot.caption = ggplot2::element_text(size=10, colour = "#666666"))
  )
}



#' @title Get expression of vector of string in string format
#' @description To evaluate expression, get string of expression
#' @param vector_of_strings vector of strings, such as c("A", "B")
#' @examples
#' \donttest{
#' noquote(
#'   get_string_expression_of_vector_of_strings(c("A", "B"))
#' )
#' }
get_string_expression_of_vector_of_strings <- function(vector_of_strings) {
  return (paste("c(\"", stringr::str_c(vector_of_strings, collapse = "\", \""), "\")", sep=""))
}

#' @title Get variable-group-name list
#' @description variable-group is a combination of one LHS and one or more RHS.
#' this function outputs the list of names of variable-group in given rule-table.
#' the format of return value is "LHS|RHS1,RHS2,RHS3,...".
#' @param R rule-table
#' @examples
#' \donttest{
#' noquote(
#'   get_variable_group_name_list(R)
#' )
#' }
#' @export
get_variable_group_name_list <- function(rule_table) {

  # initialize result
  group_name_list <- c()

  # initialize temporary variables
  current_left_side_name <- ""
  current_right_side_name_list <- c()

  # scan all rows in rule_table
  for (i_row in 1:nrow(rule_table)) {

    new_row <- rule_table[i_row, ]

    # if find new LHS entry
    if (new_row$Left_side != "") {

      # add current_group_name to result
      if (i_row > 1) {

        # formatting such as
        # current_left_side_name = current_right_side_name_list[1] + current_...[2] + ...
        new_group_name <- generate_variable_group_name_from_lhs_and_rhs_list(
          current_left_side_name, current_right_side_name_list)

        # add new variable group name
        group_name_list <- c(group_name_list, new_group_name)
      }

      # re-set temporary variables
      current_left_side_name <- new_row$Left_side  #paste(, " = ", sep="")
      current_right_side_name_list <- c()
    }

    # if find new RHS entry
    if (new_row$Right_side != "") {

      # remove parent part, then get base name
      new_right_side_name <- stringr::str_replace(new_row$Right_side, "^.*\\|", "")

      # add RHS entry to current_group_name with " + " separator.
      current_right_side_name_list <- c(current_right_side_name_list, new_right_side_name)
    }
  }

  # add last entry to result
  ## TODO:
  group_name_list <- c(group_name_list, generate_variable_group_name_from_lhs_and_rhs_list(
    current_left_side_name, current_right_side_name_list))

  # remove "|" or "," if they are placed on the end of current_group_name
  # ex. "Population|" or "Emissions|CO2,Land Use,"
  # group_name_list <- stringr::str_replace_all(group_name_list, "\\|$|,$", "")

  return(group_name_list)
}

generate_variable_group_name_from_lhs_and_rhs_list <- function(lhs, rhs_list) {

  if (length(rhs_list) > 0) {

    return(paste(lhs, stringr::str_flatten(rhs_list, collapse = " + "), sep = " = "))
  } else {
    # if rhs is empty (ex. Population in AR5 rule table)
    # output lhs only.
    return(lhs)
  }

}

#' @title Get variable name list in given variable-group
#' @description Scan rule-table and extract variable names in given variable-group.
#' @param group_name variable-group-name
#' @examples
#' \donttest{
#' noquote(
#'   get_variable_name_list_in_variable_group(
#'     ar5_db_sample_rule_table,
#'     "Final Energy|Industry,Residential and Commercial,Transportation")
#' )
#' }
#' @export
get_variable_name_list_in_variable_group <- function(group_name) {

  # CASE: includes both of LHS and RHS.

  # check if group_name includes "="
  if (stringr::str_detect(group_name, "\\=")) {

    # trim parent level part and get child level part
    # ex.
    # input: "Emissions|CO2|Fossil Fuels and Industry = Energy Demand + Energy Supply"
    # output: "Energy Demand,Energy Supply"
    child_part <- stringr::str_replace_all(group_name, "^.* \\= ", "")

    # separate elements in child level part
    # ex.
    # input: "Energy Demand + Energy Supply"
    # output: c("Energy Demand", "Energy Supply")
    child_part_elements <- stringr::str_split(child_part, " \\+ ")[[1]]

    # trim child level part and get parent level part
    # ex.
    # input: "Emissions|CO2|Fossil Fuels and Industry = Energy Demand + Energy Supply"
    # output: "Energy Demand,Energy Supply"
    parent_part <- gsub("^| \\=.*$", "", group_name)

    if (is.na(parent_part)) {

      return(child_part)

    } else {

      full_path_variable_name_list <- paste(parent_part, child_part_elements, sep="|")

      parent_part_without_last_vertical_line <- stringr::str_replace(parent_part, "\\|$", "")

      return(c(parent_part_without_last_vertical_line, full_path_variable_name_list))
    }

  } else {

    return(group_name)

  }


}
