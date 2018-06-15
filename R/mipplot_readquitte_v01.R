#--------------------------------------------------------------------
# FILE READ FUNCTION: READ IAMC DATA AS QUITTE DATAFRAME
#--------------------------------------------------------------------

#' @title A function to read scenario input data.
#' @description Read scenario input data (in IAMC format) as quitte format dataframe.
#' @param filename Path to a file containing scenario data in IAMC format.
#' @param sep A character indicating the separator used in the input file.
#' @return A dataframe in quitte format ("model, scenario, variable, unit, period, value")
#' @example mipplot_readquitte(AR5_Sample_data)
#' @export empty

mipplot_readquitte <- function(filename=NULL, sep=",", interactive=FALSE, DEBUG=T){

  if (interactive == TRUE) {
    filename <- file.choose()
  }
  filenameext <- tools::file_ext(filename)

  if (filenameext != "csv" & sep == ",") {
    print("File is not comma separated values (csv), please specify data delimiter.")
    sep <- readline()
  }

  loaded_quitte = quitte::read.quitte(filename, sep = sep)

  if (nrow(loaded_quitte) == 0) {
    stop("No rows available in the given quitte file.")
  }

  if (!("quitte" %in% class(loaded_quitte))) {
    stop("This is not quitte format file.")
  }

  return(loaded_quitte)
}

#END
