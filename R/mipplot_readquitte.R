#--------------------------------------------------------------------
# FILE READ FUNCTION: READ IAMC DATA AS QUITTE DATAFRAME
#--------------------------------------------------------------------

#' @title Read IAMC scenario input data.
#' @description Read scenario input data (in IAMC format) as quitte format dataframe.
#' @param filename Path to a file containing scenario data in IAMC format.
#' @param sep A character indicating the separator used in the input file.
#' @return A dataframe in quitte format ("model, scenario, variable, unit, period, value")
#' @examples
#' mipplot_readquitte(ar5_db_sample_data)
#' @export
mipplot_readquitte <- function(filename=NULL, sep=",", interactive=FALSE, DEBUG=T){

  if (interactive == TRUE) {
    filename <- file.choose()
  }
  filenameext <- tools::file_ext(filename)

  if (filenameext != "csv" & sep == ",") {
    print("File is not comma separated values (csv), please specify data delimiter.")
    sep <- readline()
  }

  loaded_quitte = read_quitte(filename, sep = sep)

  if (nrow(loaded_quitte) == 0) {
    stop("No rows available in the given quitte file.")
  }

  return(loaded_quitte)
}

read_quitte <- function(file_path, sep = ",") {

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
