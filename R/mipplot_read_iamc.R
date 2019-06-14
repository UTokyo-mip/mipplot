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
#' \donttest{
#' mipplot_read_iamc("filename")
#' }
#' @export
mipplot_read_iamc <- function(filename=NULL, sep=",", interactive=FALSE, DEBUG=T){

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

read_iamc <- function(file_path, sep = ",") {

  # read column names from data file
  all_columns <- tolower(scan(file_path, sep = sep, what = "character", nlines = 1, quiet = TRUE))

  # extract year columns
  year_columns <- extract_year_columns(all_columns)

  # read contents
  content <- readr::read_delim(file_path, delim = sep, col_types = cols(
    .default = col_double(),
    MODEL = col_factor(NULL),
    SCENARIO = col_factor(NULL),
    REGION = col_factor(NULL),
    VARIABLE = col_factor(NULL),
    UNIT = col_factor(NULL)
  ))

  colnames(content) <- tolower(colnames(content))
  content <- tidyr::gather_(content, "period", "value", year_columns)
  content$period <- as.integer(content$period)

  return(content)
}

extract_year_columns <- function(columns) {

  return(columns[grep("^[0-9].*?$", columns)])
}

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

  return(df)
}

#END
