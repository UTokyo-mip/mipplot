#--------------------------------------------------------------------
# PLOTTING FUNCTION: BAR
#--------------------------------------------------------------------
#====================================================================
# Bar plots using right-hand-side values of target additivity rule.
#     The function arguments inlcude the input dataframe, labels
#     for the plot/axes/legend, and faceting dimensions.
#====================================================================

#' @title Bar plot from IAMC data
#' @description Bar plots using right-hand-side values of
#'              target additivity rule. The function arguments include the
#'              input dataframe, labels for the plot/axes/legend, and
#'              faceting dimensions.
#' @param D A dataframe of IAMC data in tibble format to produce plots.
#' @param R A dataframe of data aggregation rules (meta data).
#' @param region A list of region.
#' @param xby name of axis. the default setting is "scenario".
#' @param target_year target year.
#' @param facet_x facet_x
#' @param facet_y facet_y
#' @param PRINT_OUT set TRUE to generate A PDF file.
#' @param DEBUG set TRUE to show debug messages.
#' @param fontsize size of font in the output plot.
#' @param color_code_specify set FALSE if you apply default color palette.
#' @return A list of bar plots.
#' @examples
#' \donttest{
#' mipplot_bar(ar5_db_sample_data, ar5_db_sample_rule_table)
#' }
#' @export

# Faceting approach: allow only for 1 dimension (facet_wrap)
# if one dimension is replicated in horizontal axis.

mipplot_bar <- function(
  D, R,region = levels(D$region), xby = "scenario",
  target_year = levels(as.factor(D$period)),
  facet_x = NULL, facet_y = NULL, PRINT_OUT = F, DEBUG = T, fontsize = 20,
  color_code_specify = T) {

  # REPLACED THIS FUNCTION WITH 1-LINE CODE (SEE LINE 52).
  # wrap_text <- function(x, width=60){
  #   sapply(x,function(x) {paste(strwrap(x,width),collapse="\n")})
  # }

  p_list1 <- list()

  for (i in levels(as.factor(R$Rule_ID))) {

    for (r in levels(as.factor(region))) {

      for (ty in levels(as.factor(target_year))) {

        Var_set <- R[R$Rule_ID == i, ]

        ## SELECT DATA
        D_LHS <-
          D[D$region == r & D$period == ty &
              D$variable %in% Var_set$Left_side, ]
        D_RHS <-
          D[D$region == r & D$period == ty &
              D$variable %in% Var_set$Right_side, ]

        # Renaming levels of a factor
        # http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/

        # Common Part of Var-name
        var_common_name <- Var_set[1, 2]

        # Title
        tt1 <- paste("region:", r, ",  period:", ty, sep = "")
        tt2 <- paste("variable:", as.character(Var_set[1, 2]), sep = "")
        tt3 <- paste(" [", D_RHS$unit[1], "]", sep = "")

        # Change name of variable by removing
        # common part from aggregated vairable (LHS).
        D_RHS$variable <-
          forcats::fct_rev(gsub(paste(var_common_name, "|", sep = ""),
               "", D_RHS$variable, fixed = T))

        # Only generate plots if data is available for a region.
        if (nrow(na.omit(D_RHS[D_RHS$region == r, ]))) {

          ## Bar plots: using right-hand-side values of target rule.
          p_Out1 <- ggplot2::ggplot(
            na.omit(D_RHS),
            ggplot2::aes(y = value, fill = variable)) +
            ggplot2::geom_bar(stat = "identity", ggplot2::aes_(x = as.name(xby))) +
            ggplot2::labs(title = tt1, subtitle = tt2, y = tt3) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

  			  ## Facet plots if horizontal and/or vertical dimension provided.

          if (xby == "model") {

              facet_by <- "scenario"

          } else {

              facet_by <- "model"
          }

          if (!is.null(facet_x) & !is.null(facet_y)) {

            if (facet_x!=xby & facet_y!=xby) {

              facet_by <- paste(facet_y, facet_x, sep="~")
              p_Out1 <- p_Out1 + ggplot2::facet_grid(facet_by)

            } else {

                p_Out1 <- p_Out1 +
                  ggplot2::facet_wrap(~eval(parse(text=facet_by)))
            }

          } else {

            if (!is.null(facet_x) & is.null(facet_y)) {

              if (xby!=facet_x) {
                facet_by <- facet_x
              }

            } else if(is.null(facet_x) & !is.null(facet_y)) {

              if (xby!=facet_y) {
                facet_by <- facet_y
              }
            }

            p_Out1 <- p_Out1 +
              ggplot2::facet_wrap(~eval(parse(text=facet_by)))
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
          if (!is.null(color_mapper[[var_common_name]])) {
            new_mapper <- color_mapper[[var_common_name]]
            p_Out1 <- p_Out1 + ggplot2::scale_fill_manual(values=new_mapper)
          }

          # STORE PLOTS TO LIST
          p_list1[[length(p_list1) + 1]] <- p_Out1
        }
      }
    }
  }

  if (PRINT_OUT == TRUE){

    mipplot_print_pdf(p_list1, filelabel = "bar")

  }

     return(p_list1)
}
