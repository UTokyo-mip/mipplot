#--------------------------------------------------------------------
# FILE READ FUNCTION: READ IAMC DATA AS TIBBLE DATAFRAME
#--------------------------------------------------------------------

#' @title Read IAMC scenario input data.
#' @description Read scenario input data (in IAMC format) as tibble format dataframe.
#' @param filename Path to a file containing scenario data in IAMC format.
#' @param sep A character indicating the separator used in the input file.
#' @param interactive open a dialog for selecting file if interactive=TRUE.
#' @param DEBUG experimental.
#' @return A dataframe in tibble format ("model, scenario, variable, unit, period, value")
#' @examples
#' \dontrun{
#' mipplot_read_iamc("filename")
#' }
#' @export
mipplot_read_iamc <- function(filename=NULL, sep=",", interactive=FALSE, DEBUG=TRUE){

  if (interactive == TRUE) {
    filename <- file.choose()
  }
  filenameext <- tools::file_ext(filename)

  if (filenameext != "csv" & sep == ",") {
    print("File is not comma separated values (csv), please specify data delimiter.")
    sep <- readline()
  }

  loaded_iamc = read_iamc(filename, sep = sep)

  if (nrow(loaded_iamc) == 0) {
    stop("No rows available in the given iamc file.")
  }

  return(loaded_iamc)
}

#' @importFrom readr cols col_factor col_double
read_iamc <- function(file_path, sep = ",") {

  # read column names from data file
  all_columns <- tolower(scan(file_path, sep = sep, what = "character", nlines = 1, quiet = TRUE))

  # extract year columns
  year_columns <- extract_year_columns(all_columns)

  # read procedure:
  # 1. try lower case columns first.
  # 2. if some lower case column doesn't exist,
  #    then retry upper case columns.
  content <- tryCatch({
    return(readr::read_delim(file_path, delim = sep, col_types = cols(
      .default = col_double(),
      Model = col_factor(NULL),
      Scenario = col_factor(NULL),
      Region = col_factor(NULL),
      Variable = col_factor(NULL),
      Unit = col_factor(NULL))))
  }, warning = function(e){
    return(readr::read_delim(file_path, delim = sep, col_types = cols(
      .default = col_double(),
      MODEL = col_factor(NULL),
      SCENARIO = col_factor(NULL),
      REGION = col_factor(NULL),
      VARIABLE = col_factor(NULL),
      UNIT = col_factor(NULL))))
  },
  silent=TRUE)

  colnames(content) <- tolower(colnames(content))
  content <- tidyr::gather_(content, "period", "value", year_columns)
  content$period <- as.integer(content$period)

  return(content)
}

extract_year_columns <- function(columns) {

  return(columns[grep("^[0-9].*?$", columns)])
}

#' @title Read IAMC scenario input data in Excel format
#' @description Read scenario input data (in IAMC format) as tibble format dataframe from Excel
#' @param file_path Path to a file containing scenario data in IAMC format.
#' @param sheet the index of sheet which contains records.
#' @return A dataframe in tibble format ("model, scenario, variable, unit, period, value")
#' @examples
#' \dontrun{
#' read_iamc_xlsx("filename", sheet = 2)
#' }
#' @export
read_iamc_xlsx <- function(file_path, sheet = 2) {

  # load a sheet from xlsx file
  df <- readxl::read_xlsx(file_path, sheet = sheet)

  # strings of header must be in lower case
  names(df) <- tolower(names(df))

  # select columns which start with numbers
  # (for example, excludes "abcd", includes "2019-abcd")
  col_names <- names(df)
  year_col_names <- col_names[grep("^[0-9].*?$", col_names)]

  # convert contents to tidy data
  df <- tidyr::gather_(df, "period", "value", year_col_names)

  # convert data type
  df$model <- as.factor(df$model)
  df$scenario <- as.factor(df$scenario)
  df$region <- as.factor(df$region)
  df$variable <- as.factor(df$variable)
  df$period <- as.integer(df$period)
  df$unit <- as.factor(df$unit)

  return(df)
}

#END
