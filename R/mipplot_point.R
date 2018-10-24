
#--------------------------------------------------------------------
# PLOTTING FUNCTION: Point
#--------------------------------------------------------------------

#' @title Point plot from IAMC data
#' @description The function arguments include the input dataframe,
#'              labels for the plot/axes/legend, and faceting dimensions
#' @param D A dataframe of IAMC data in quitte format to produce plots.
#' @param region A list of regions.
#' @param variable A list of variables.
#' @param target_year A list of target years.
#' @param colorby An axis for color setting.
#' @param shapeby An axis for shape setting.
#' @param xby An axis for x locating setting.
#' @param facetby facetby.
#' @param facet_x facet_x.
#' @param facet_y facet_y.
#' @param fontsize font size.
#' @param PRINT_OUT set TRUE to generate PDF image.
#' @param DEBUG set TRUE to show debug messages.
#' @return A list of point plots.
#' @examples
#' \donttest{
#' mipplot_point(ar5_db_sample_data)
#' }
#' @export

mipplot_point <- function(
  D, region = levels(D$region), variable = levels(D$variable),
  target_year = levels(as.factor(D$period)), colorby = "model",
  shapeby = "model", xby = "scenario",
  facetby = NULL, facet_x = NULL, facet_y=NULL,
  fontsize=20, PRINT_OUT = F, DEBUG = T) {

  p_list1 <- list()

  for (r in levels(as.factor(region))) {

    for (v in levels(as.factor(variable))) {

      for (ty in levels(as.factor(target_year))) {


        D_sub <- D[D$region == r & D$variable == v & D$period == ty, ]

        ## removing NA ensures lines are connected
        D_sub <- D_sub[!is.na(D_sub$value), ]

        # Skip iteration if data is empty for the selected scope (region/variable).
        if (nrow(D_sub) == 0){ next() }

        ## Title
        tt1 <- paste("region:", r, ",  period:", ty, sep = "")
        tt2 <- paste("variable:", as.character(v), sep = "")
        tt3 <- paste(' [', D_sub$unit[1], ']', sep = "")

        ## Box plots: using scenario
        p_Out1 <- ggplot2::ggplot(
          data = D_sub,
          ggplot2::aes(y=value)) +
          ggplot2::geom_point(
            ggplot2::aes_(x = as.name(xby),
                          color = as.name(colorby),
                          shape = as.name(shapeby),
                          fill = as.name(colorby)), size = 5) +
          ggplot2::scale_shape_manual(values = seq(0, 10)) +
          ggplot2::labs(title = tt1, subtitle = tt2, y = tt3) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

        ## Facet plots if horizontal and/or vertical dimension provided.
        if (!is.null(facetby)) {

            p_Out1 <- p_Out1 + ggplot2::facet_wrap(~eval(parse(text = facetby)))

        } else if (!is.null(facet_x) & !is.null(facet_y)) {

            facet_by <- paste(facet_y, facet_x, sep = "~")
            p_Out1 <- p_Out1 + ggplot2::facet_grid(facet_by)

        } else if (!is.null(facet_x) & is.null(facet_y)) {

            facet_by <- paste(".", facet_x, sep = "~")
            p_Out1 <- p_Out1 + ggplot2::facet_grid(facet_by)

        }else if(is.null(facet_x) & !is.null(facet_y)) {

            facet_by <- paste(facet_y, ".", sep = "~")
            p_Out1 <- p_Out1 + ggplot2::facet_grid(facet_by)
        }

        p_Out1 <- p_Out1 + ggplot2::theme(
          text = ggplot2::element_text(size = fontsize))

        ## STORE PLOTS TO LIST
        p_list1[[length(p_list1) + 1]] <- p_Out1
      }
    }
  }

  if (PRINT_OUT == TRUE) {

    mipplot_print_pdf(p_list1, filelabel = "point")

  }

  return(p_list1)
}
