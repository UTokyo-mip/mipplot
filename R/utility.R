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
