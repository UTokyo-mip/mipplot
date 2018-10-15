#--------------------------------------------------------------------
# RULETABLE READ FUNCTION: READ RULETABLE DATA AS MATE DATA
#--------------------------------------------------------------------

#' @title Read file of rule table without ID number
#' @description Read table of additivity rule and adds column with id number.
#' @param R_without_id Path to a file containing data of additivity rule.
#' @return A dataframe of additivity rule ("ID, Left_side, Right_side")
#' @examples
#' mipplot_readquitte(ar5_db_rule_table_v09_wo_id)
#' @export mipplot_read_ruletab

mipplot_read_ruletab <- function(R_without_id) {

  R_without_id <- read.csv(R_without_id, stringsAsFactors = FALSE)

  # Stop when input data is not data.frame.
  if (!("data.frame" %in% class(R_without_id)))
    stop("Input data is not data.frame class")

  # Stop when input data has no rows.
  if (nrow(R_without_id) == 0)
    stop("Input data must contain one or more rows.")

  # Check if input data has required columns.
  if (!all(c("Left_side", "Right_side") %in% colnames(R_without_id)))
    stop("Input data must contains columns Left_side and Right_side.")

  R_without_id$Rule_ID <- rep(0, dim(R_without_id)[1])
  current_ID <- 0

  for (i in 1:dim(R_without_id)[1]) {

    if (R_without_id$Left_side[i] != "") {
      current_ID <- current_ID + 1
    }

    R_without_id$Rule_ID[i] <- current_ID
  }

  # If the dataframe includes column of Color_code,
  # the result of this function includes the column.
  if ("Color_code" %in% colnames(R_without_id)) {
    R_with_id <- R_without_id[, c("Rule_ID", "Left_side", "Right_side", "Color_code")]
  } else {
    R_with_id <- R_without_id[, c("Rule_ID", "Left_side", "Right_side")]
  }

  return(R_with_id)
}


