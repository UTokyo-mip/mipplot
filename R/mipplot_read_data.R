#--------------------------------------------------------------------
# INPUT DATA
#--------------------------------------------------------------------
#====================================================================
# Setup libraries, read input data, and prepare data for analysis.
#       This script integrates functions for loading libraries,
#       selecting and reading input file, and prepare data for analysis,
#       and reading rule table (additivity).
#       It returns a dataframe ready to use for diagnostics and plotting.
#====================================================================

mipplot_read_data <- function(filename = NULL, rule_table_filename=NULL,
                          additivity_test = FALSE, variable_check = FALSE){



  # # Install and import libraries.
  # mipplot_setup("ggplot2")
  # mipplot_setup("data.table")
  # mipplot_setup("openxlsx")
  # mipplot_setup("stringr")
  # mipplot_setup("reshape2")
  # mipplot_setup("dplyr")


  # Select and read input file.
  if(is.null(filename)){
      filename <- file.choose()
  }
  df1 <- data.table::fread(filename, header=TRUE)


  # Transform input data into format ready-to-use for diagnostics and plotting.
  D1 <- mipplot_indata_prepare(df1)


  # Read table with additivity rules.
  #  R1 <- mipplot_rule_tab(rule_table_filename)

  # Execute additivity test.
#  if(additivity_test == TRUE){
#    T_out <- mipplot_additivity(D1)
#  }

  # Execute variable submission check.
#  if(variable_check == TRUE){
#    var_list <- read.csv("../data/Variable_list_ar5_v01.csv", header = TRUE)
#    V_out <- mipplot_var_submission(D1, var_list)
#  }

  return(D1)
}

#TEST
#rule_tab1 <- "../data/ar5_db_rule_table_v02_DSH.csv"
#fname <- "../data/ar5_db_sample05_EMF.csv"
#xD1 <- mipplot_read_data(fname,rule_table_filename=rule_tab1, additivity_test = T, variable_check = T)
#END
