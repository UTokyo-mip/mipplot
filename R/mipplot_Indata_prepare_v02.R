#--------------------------------------------------------------------
# PREPARE DATA FOR ANALYSIS
#--------------------------------------------------------------------
#====================================================================
# Process input data (IAMC data table) from wide to long format.
#       Headers are defined explicitly, and year values converted
#       to numerical values.
#====================================================================

mipplot_indata_prepare <- function(df1){

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
