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
#' @param D A dataframe of IAMC data in tibble format to produce area plots.
#' @param R A dataframe of data aggregation rules (meta data).
#' @param region A list of regions.
#' @param scenario A list of scenario.
#' @param facet_x facet_x
#' @param facet_y facet_y
#' @param PRINT_OUT set TRUE to generate PDF file.
#' @param DEBUG set TRUE to show debug messages.
#' @param fontsize font size of text.
#' @param color_code_specify set FALSE if you apply default color palette.
#' @param language A string of language. Possible values are "en", "jp",
#' "es", "zh-cn", "zh-tw". The default value is "en".
#' @return A list of area plots.
#' @examples
#' \donttest{
#' mipplot_area(ar5_db_sample_data, ar5_db_sample_rule_table)
#' }
#' @export

mipplot_area <- function(
  D, R, region=levels(D$region), scenario=levels(D$scenario),
  facet_x=NULL, facet_y=NULL, PRINT_OUT=F, DEBUG=T, fontsize=20,
  color_code_specify=T, language="en"){

  # internationalization

  i18n_header <- shiny.i18n::Translator(
    translation_json_path =
      system.file("mipplot", "translation_header.json", package="mipplot"))
  i18n_header$set_translation_language(language)

  i18n_region <- shiny.i18n::Translator(
    translation_json_path =
      system.file("mipplot", "translation_region.json", package="mipplot"))
  i18n_region$set_translation_language(language)

  i18n_variable <- shiny.i18n::Translator(
    translation_json_path =
      system.file("mipplot", "translation_variable.json", package="mipplot"))
  i18n_variable$set_translation_language(language)

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

        # if color palette isn't specified or color_code column isn't included,
        # default color palette is applied.
        # This color_map is used to sort variable names too.
        if (color_code_specify == FALSE || !("Color_code" %in% colnames(R))) {
          color_mapper <- mipplot_default_color_palette
        } else {
          # otherwise, generate palette.
          color_mapper <- mipplot_generate_color_mapper(R)
        }

        ## Title
        tt1 <- paste("region:", r, ",  scenario:", s, sep = "")
        tt2 <- paste("variable:", as.character(Var_set[1, 2]), sep = "")
        tt3 <- paste(" [", D_RHS$unit[1], "]", sep = "")

        # Change name of variable by removing
        # common part from aggregated vairable (LHS).
        D_RHS$variable <- factor(
          gsub(paste(var_common_name, "|", sep = ""),"", D_RHS$variable, fixed = T),
          levels = rev(names(color_mapper[[var_common_name]])))

        D_RHS <- translate_data_table(D_RHS, language)
        D_LHS <- translate_data_table(D_LHS, language)
        for (i_mapper in 1:length(color_mapper)) {
          color_mapper[[i_mapper]] <- translate_color_mapper(color_mapper[[i_mapper]], language)
          print(color_mapper[[i_mapper]])
        }


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

          # apply color palette.
          if (!is.null(color_mapper[[var_common_name]])) {
            new_mapper <- color_mapper[[var_common_name]]
            p_Out1 <- p_Out1 + ggplot2::scale_fill_manual(values=new_mapper)
          }

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
