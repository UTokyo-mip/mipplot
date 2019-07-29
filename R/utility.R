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
        caption = "copyright 2019 UTokyo-mip All Rights Reserved.") +

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
