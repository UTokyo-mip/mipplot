# TITLE: mipplot variable submission check
# OUTLINE: function to verify which data have been submitted for a scenario with IAMC format.
# Developed by: Masahiro SUGIYAMA and Diego SILVA HERRAN.
# Last revision: 2017.10.16 Diego SILVA HERRAN.
# List of revisions:
#--------------------------------------------------------------------
#IMPORT LIBRARIES
#--------------------------------------------------------------------
library(ggplot2)
library(data.table)
library(openxlsx)
library(stringr)
library(reshape2)

#--------------------------------------------------------------------
# variable SUBMISSION CHECK
#====================================================================
# Verify whether data of variables included in list template have been
#       submitted.
#====================================================================

mipplot_var_submission <- function(D,V, na_name="N/A"){

  if(missing(D)) stop("Input data is missing! Specify input data!")
  if(missing(V)) stop("Input list of variables is missing! Specify input data!")

  # Filter input table to exclude periods without data for any variables.
  # Useful for submissions covering timeframes smaller than 2100 (e.g. until 2050).
  D1 <- D
  D1$value <- as.numeric(as.character(D$value))
  D1 <- D1[complete.cases(D1$value),]
  D <- D[D$period %in% levels(as.factor(D1$period)), ]

  V1 <- V
  #V1 <- V[,"variable"]    # Vector with list of variables to be submitted.
  #names(V1) <- "variable"    # Edit header name to comply with IAMC DB format.

  D[is.na(D$model),"model"] <- "Model_empty"    # Fill name of model if empty.

  # Add "Member" column (identifier for each unique combination of model/scenario/region).
  D$Member <- as.factor(paste(D$model, D$scenario, D$region, sep=" | "))

  D1 <- D

  #D1_empty <- D[D$value=="",]    # DF with empty values in submission.
  D1_na <- na.omit(D[D$value==na_name,])    # DF with "NA" values in submission.

  D1_empty <- D[!complete.cases(D),]    # DF with empty values in submission (includes "empty "N/A" values).

  D1 <- na.omit(D[D$value!=na_name,])    # DF with "non-NA" values in submission.
  D1$value <- as.numeric(as.character(D1$value))    # Replace non-numerical data by "NA".
  D1_invalid <- D1[is.na(D1$value),]    # DF with invalid values (i.e. non-numeric AND NOT empty/na) in submission.
  D1_valid <- na.omit(D1)    # DF with valid values (i.e. numeric) in submission.

  D1_valid[D1_valid$value!=0,"value"] <- 1    # Replace all non-zero valid values with 1 for further operations.

  #S1 <- aggregate(D1_valid$value, by=D1_valid[c("model","scenario","region","variable","unit")], mean)
  S1 <- aggregate(D1_valid$value, by=D1_valid[,c("model","scenario","region","variable","unit")], mean)
  D1_zero <- S1[S1$x == 0,]    # DF with "all zero" values in submission.

  # Select data of variables with values (i.e. not all values are NA).
  D2 <- D1_valid[!duplicated(D1_valid[,c("model","scenario","region","variable")]),]

  # List of member data (i.e. Model, Scenario, Region).
  m_list <- levels(D$Member)
  V_list <- list()
  #var_list <- levels(as.factor(D$variable))

  # Compare variables in submission to template for each member.
  # Assume that variables with "NA" are missing; if all values are zero then it is NOT missing.

  #i <- m_list[[1]]    # For testing!

  for(i in m_list){

    D3 <- D2[D2$Member==i,]

    V2 <- V1
    Submitted <-intersect(V1$variable, D3$variable)    # List of variables submitted.
    Additional <- setdiff(D3$variable, V1$variable)    # List of other variables.

    V2$Outcome <- "Missing_var"    # Create column filled with "Missing" by default.
    V2[V2$variable %in% Submitted,"Outcome"] <- "Submitted_var"

    # Information on submitted variables excluded in template list.
    if(length(Additional)>0){
      A1 <- D3[D3$variable %in% Additional, "variable"]    # Create dataframe from list of variables only found in the data submission.
      A1$Outcome <- "Additional_var"
      V2 <- rbind(V2,A1)    # Append list of additional variables.
    }

    # Information on irregular and/or missing variables.
    # Reports periods with NA/Empty/Invalid entries for each variable and member.
    V2$NA_periods <- "None"
    V2$Empty_periods <- "None"
    V2$Invalid_periods <- "None"
    #j <- 1   # For testing purposes!
    #j = var_list[1]
    #next(j)
    var_list <- V2[V2$Outcome=="Submitted_var",]$variable
    for(j in var_list){    # Skip loop for variables submitted.

      NA_periods <- D1_na[D1_na$Member==i & D1_na$variable==j, ]

      if(nrow(NA_periods)>0){

        #V2[V2$variable %in% NA_periods$variable, "NA_periods"] <- paste(NA_periods$period, collapse=", ")
        V2[V2$variable==j, "NA_periods"] <- paste(NA_periods$period, collapse=", ")

      }

      Empty_periods <- D1_empty[D1_empty$Member==i & D1_empty$variable==j, ]

      if(nrow(Empty_periods)>0){

        #V2[V2$variable %in% Empty_periods$variable, "Empty_periods"] <- paste(Empty_periods$period, collapse=", ")
        V2[V2$variable==j,]$Empty_periods <- paste(Empty_periods$period, collapse=", ")

      }

      Invalid_periods <- D1_invalid[D1_invalid$Member==i & D1_invalid$variable==j, ]

      if(nrow(Invalid_periods)>0){

        #V2[V2$variable %in% Invalid_periods$variable, "Invalid_periods"] <- paste(Invalid_periods$period, collapse=", ")
        V2[V2$variable==j, "Invalid_periods"] <- paste(Invalid_periods$period, collapse=", ")

      }

    }

    V2 <- cbind(D3[1,c("model","scenario","region")],V2)    # Add columns with Member information.

    V_list[[length(V_list)+1]] <- V2

  }

  V_out <- do.call(rbind, V_list)

  V_missing <- V_out[V_out$Outcome=="Missing_var", c("model","scenario","region","variable","Outcome")]
  filename <- sprintf("../data_output/Var_missing_%s.csv", format(Sys.time(),"%Y_%m%d"))
  write.csv(V_missing, file=filename, row.names = F)

  V_missing_wide <- dcast(V_missing, ...~model+scenario)
  filename <- sprintf("../data_output/Var_missing_wide_%s.csv", format(Sys.time(),"%Y_%m%d"))
  write.csv(V_missing_wide, file=filename, row.names = F)

  filename <- sprintf("../data_output/Var_submission_check_%s.csv", format(Sys.time(),"%Y_%m%d"))
  write.csv(V_out, file=filename, row.names = F)
  return(V_out)
}

# TEST
# # Select data to be plotted.
# df1 <- data.table::fread("../data/ar5_db_sample05_EMF.csv", header = TRUE)
# D <- melt(df1,
#            id.vars=c("model","scenario","region","variable","unit"))
# names(D) <- c("model","scenario","region","variable","unit","period","value")
# D$period <- as.numeric(gsub("X([0-9]+)","\\1",D$period))
# V <- read.csv("../data/Variable_list_ar5_v01.csv", header = TRUE)
#xD1a <- mipplot_var_submission(D,V,na_name="N/A")
#head(Dx1a)

# END
