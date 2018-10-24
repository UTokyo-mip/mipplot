#--------------------------------------------------------------------
# FILE READ FUNCTION: READ IAMC DATA AS QUITTE DATAFRAME
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
  all_columns <- tolower(scan(file_path, sep = sep, what = "character", nlines = 1))

  # extract year columns
  year_columns <- extract_year_columns(all_columns)

  # read contents
  content <- readr::read_delim(file_path, delim = sep)
  colnames(content) <- tolower(colnames(content))

  return(tidyr::gather_(content, "period", "value", year_columns))
}

extract_year_columns <- function(columns) {

  return(columns[grep("^[0-9].*?$", columns)])
}


#END
