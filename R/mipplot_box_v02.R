#--------------------------------------------------------------------
# PLOTTING FUNCTION: BOX
#--------------------------------------------------------------------

#' @title Box plot from IAMC data
#' @description The function arguments include the input dataframe,
#'              labels for the plot/axes/legend, and faceting dimensions
#' @param D A dataframe of IAMC data in quitte format to produce plots.
#' @return A list of box plots.
#' @example mipplot_box(ar5_db_sample_data)
#' @export p_list1

mipplot_box <- function(
  D, region=levels(D$region),
  variable=levels(D$variable),
  target_year=levels(D$period), PRINT_OUT=F, DEBUG=T) {

  p_list1 <- list()

  for (r in levels(as.factor(region))) {

    for (v in levels(as.factor(variable))) {

      for (ty in levels(as.factor(target_year))) {

        D_sub <- D[D$region == r & D$variable == v & D$period == ty, ]

        ## removing NA ensures lines are connected
        D_sub <- D_sub[!is.na(D_sub$value),]

        # Skip iteration if data is empty
        #  for the selected scope (region/variable).
        if (nrow(D_sub) == 0) { next() }

        ## Title
        tt1 <- paste("region:", r, ",  period:", ty, sep = "")
        tt2 <- paste("variable:", as.character(v), sep = "")
        tt3 <- paste(' [', D_sub$unit[1], ']', sep = "")

         ## Box plots: using scenario
         p_Out1 <- ggplot2::ggplot(
           D_sub, ggplot2::aes(x = scenario, y = value)) +
           ggplot2::geom_boxplot() +
           ggplot2::labs(title = tt1, subtitle = tt2, y = tt3) +
           ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

         p_Out1 <- p_Out1 +
           ggplot2::theme(text = ggplot2::element_text(size = 20))

         ## STORE PLOTS TO LIST
         p_list1[[length(p_list1) + 1]] <- p_Out1
      }
    }
  }

  if (PRINT_OUT == TRUE) {

    mipplot_print_pdf(p_list1, filelabel = "box")

  }

  return(p_list1)
}

