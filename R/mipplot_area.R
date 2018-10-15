#--------------------------------------------------------------------
# PLOTTING FUNCTION: AREA.
#--------------------------------------------------------------------
#====================================================================
# Area plots using right-hand-side values of target additivity rule.
#     The function arguments inlcude the input dataframe, labels
#     for the plot/axes/legend, and faceting dimensions (two in this
#     version).
#====================================================================

#' @title Area plot from IAMC data
#' @description Area plots using right-hand-side values of target
#'              additivity rule. The function arguments include the input dataframe,
#'              labels for the plot/axes/legend, and faceting dimensions
#'              (two in this version).
#' @param D A dataframe of IAMC data in quitte format to produce area plots.
#' @param R A dataframe of data aggregation rules (meta data).
#' @param color_code_specify set FALSE if you apply default color palette.
#' @return A list of area plots.
#' @examples
#' mipplot_area (ar5_db_sample_data, ar5_db_rule_table)
#' @export

mipplot_area <- function(
  D, R, region=levels(D$region), scenario=levels(D$scenario),
  facet_x=NULL, facet_y=NULL, PRINT_OUT=F, DEBUG=T, fontsize=20,
  color_code_specify=T){

  p_list1 <- list()

  for (i in levels(as.factor(R$Rule_ID))){

    for (r in levels(as.factor(region))){

      for (s in levels(as.factor(scenario))){

        Var_set <- R[R$Rule_ID == i, ]

        ## SELECT DATA
        D_LHS <- D[D$region == r & D$scenario == s &
                   D$variable %in% Var_set$Left_side, ]

        D_RHS <- D[D$region == r & D$scenario == s &
                     D$variable %in% Var_set$Right_side, ]

        # Renaming levels of a factor
        # http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/

        # Common Part of Var-name
        var_common_name <- Var_set[1, 2]

        ## Title
        tt1 <- paste("region:", r, ",  scenario:", s, sep = "")
        tt2 <- paste("variable:", as.character(Var_set[1, 2]), sep = "")
        tt3 <- paste(" [", D_RHS$unit[1], "]", sep = "")

        # Change name of variable by removing
        # common part from aggregated vairable (LHS).
        D_RHS$variable <- gsub(
          paste(var_common_name, "|", sep = ""), "", D_RHS$variable, fixed = T)

        ## Generate plots only if data is available for a given scenario.
        if (nrow(na.omit(D_RHS[D_RHS$scenario == s, ])) > 0) {

          ### FACET DOES NOT WORK IN NO DATA EXISTS.
          p_Out1 <-
            ggplot2::ggplot() +
            ggplot2::geom_area(
              data = na.omit(D_RHS),
              ggplot2::aes(x = period, y = value, fill = variable),
              position = "stack")

          p_Out1 <- p_Out1 +
            ggplot2::geom_line(
              data = na.omit(D_LHS),
              ggplot2::aes(x = period, y = value), size = 2)

          # Define plot titles and axes labels.
          p_Out1 <- p_Out1 +
            ggplot2::labs(title = tt1, subtitle = tt2, y = tt3)

          # Remove legend title.
          # p_Out1 <- p_Out1 + ggplot2::theme(legend.title=tt_legend)

          if (!is.null(facet_x) & !is.null(facet_y)) {

              facet_by <- paste(facet_y, facet_x, sep = "~")
              p_Out1 <- p_Out1 + ggplot2::facet_grid(facet_by)

          } else {

              p_Out1 <- p_Out1 + ggplot2::facet_wrap(~model)
          }

          p_Out1 <- p_Out1 + ggplot2::theme(
            text = ggplot2::element_text(size = fontsize))


          # if color palette isn't specified or color_code column isn't included,
          # default color palette is applied.
          if (color_code_specify == FALSE || !("Color_code" %in% colnames(R))) {

            color_mapper <- mipplot_default_color_palette

          } else {

            # otherwise, generate palette.
            color_mapper <- mipplot_generate_color_mapper(R)

          }
          # apply color palette.
          p_Out1 <- p_Out1 + ggplot2::scale_fill_manual(values=color_mapper)


          p_list1[[length(p_list1) + 1]] <- p_Out1

        }
      }
    }
  }

  if (PRINT_OUT == TRUE) {

    mipplot_print_pdf(p_list1, filelabel = "area")

  }

  return(p_list1)
}
