#--------------------------------------------------------------------
# PREPARE DATA FOR ANALYSIS
#--------------------------------------------------------------------
#====================================================================
# Process input data (IAMC data table) from wide to long format.
#       Headers are defined explicitly, and year values converted
#       to numerical values.
#====================================================================

mipplot_indata_prepare <- function(df1) {

  # Check if input data is in data.frame format.
  if (class(df1) != "data.frame") {
    stop("The input data is not in data.frame format.")
  }

  # Check wheter the number of rows of input data is one or more.
  if (nrow(df1) < 1) {
    stop("The number of rows of input data is zero.")
  }

  # Check that the input data contains all necessary columns.
  required_columns = c("model","scenario","region","variable","unit")
  if (!all(required_columns %in% colnames(df1))) {
    stop(sprintf(
      "Input data must contain following columns: %s",
      paste(required_columns, collapse=", ")
      ))
  }

  D1 <- melt(df1, id.vars=c("model","scenario","region","variable","unit"))
  names(D1) <- c("model","scenario","region","variable","unit","period","value")

  D1$model <- as.factor(D1$model)
  D1$scenario <- as.factor(D1$scenario)
  D1$region <- as.factor(D1$region)
  D1$variable <- as.factor(D1$variable)
  D1$unit <- as.factor(D1$unit)
  D1$period <- as.numeric(as.character(D1$period))

  #D1$period <- as.numeric(gsub("X([0-9]+)","\\1",D1$period))

  return(D1)

}


# END
