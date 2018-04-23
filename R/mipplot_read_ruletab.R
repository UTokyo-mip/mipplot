#' @title A function to read rule table without ID number
#' @description Read table of additivity rule and adds column with id number.

mipplot_read_ruletab <- function(R_without_id) {

  R_without_id <- read.csv(R_without_id)

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

  R_with_id <- R_without_id[, c(3, 1, 2)]

  return(R_with_id)
}


