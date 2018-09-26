
#--------------------------------------------------------------------
# PLOTTING FUNCTION: LINE
#--------------------------------------------------------------------

#' @title Line plot from IAMC data
#' @description The function arguments include the input dataframe,
#'              labels for the plot/axes/legend, and faceting dimensions
#' @param D A dataframe of IAMC data in quitte format to produce plots.
#' @return A list of line plots.
#' @example mipplot_box(ar5_db_sample_data)
#' @export p_list1

mipplot_line <- function(
  D, region = levels(D$region), variable = levels(D$variable),
  colorby = "scenario", linetypeby = "model", shapeby = "model",
  scenario = levels(D$scenario), facet_x = NULL,
  facet_y = NULL, PRINT_OUT = F, DEBUG = T) {

  p_list1 <- list()

  # Convert to quitte format (PIK package dataframe).
  D <- quitte::as.quitte(D)

  for (r in levels(as.factor(region))) {

    for (v in levels(as.factor(variable))) {

        D_sub <- D[
          D$region == r &
            D$variable == v &
            D$scenario %in% scenario, ]

        ## removing NA ensures lines are connected
        D_sub <- D_sub[!is.na(D_sub$value), ]

        # Skip iteration if data is empty for
        # the selected scope (scenario/region/variable).
        if (nrow(D_sub) == 0) {
          next()
        }

        ## Title
        tt1 <- paste("region:", r, sep = "")
        tt2 <- paste("variable:", as.character(v), sep = "")
        tt3 <- paste(" [", D_sub$unit[1], "]", sep = "")

        ## Line plots: using values name
        p_Out1 <- ggplot2::ggplot(
          data = D_sub, ggplot2::aes(x = period, y = value))

        p_Out1 <- p_Out1 +
          ggplot2::geom_line(
            ggplot2::aes_(
              color = as.name(colorby), linetype = as.name(linetypeby))) +
          ggplot2::geom_point(
            ggplot2::aes_(
              color = as.name(colorby),
              shape = as.name(shapeby)), size = 2) +
            ggplot2::scale_shape_manual(values = seq(0, 10)) +
            ggplot2::labs(title = tt1, subtitle = tt2, y = tt3)

        ## Facet plots if horizontal and/or vertical dimension provided.
        if (!is.null(facet_x) & !is.null(facet_y)) {

          facet_by <- paste(facet_y, facet_x, sep = "~")
          p_Out1 <- p_Out1 + ggplot2::facet_grid(facet_by)

        } else if (!is.null(facet_x) & is.null(facet_y)) {

          facet_by <- paste(".", facet_x, sep = "~")
          p_Out1 <- p_Out1 + ggplot2::facet_grid(facet_by)

        } else if (is.null(facet_x) & !is.null(facet_y)) {

          facet_by <- paste(facet_y, ".", sep = "~")
          p_Out1 <- p_Out1 + ggplot2::facet_grid(facet_by)
        }

        p_Out1 <- p_Out1 +
          ggplot2::theme(text = ggplot2::element_text(size = 20))

        ## STORE PLOTS TO LIST
        p_list1[[length(p_list1) + 1]] <- p_Out1
    }
  }

  if (PRINT_OUT == TRUE) {

    mipplot_print_pdf(p_list1, filelabel = "line")

  }

  return(p_list1)
}