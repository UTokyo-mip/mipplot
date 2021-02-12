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
#' @param one_hundred_percent_stacked set TRUE if you want a graph of 100\% stacked, set this to TRUE.
#' @param axis_year_text_angle text angle of x axis
#' @param language A string of language. Possible values are "en", "jp",
#' "es", "zh-cn", "zh-tw". The default value is "en".
#' @return A list of area plots.
#' @importFrom stats na.omit
#' @examples
#' \dontrun{
#' mipplot_area(ar5_db_sample_data, ar5_db_sample_rule_table)
#' }
#' @export

mipplot_area <- function(
  D, R, region=levels(D$region), scenario=levels(D$scenario),
  facet_x=NULL, facet_y=NULL, PRINT_OUT=F, DEBUG=T, fontsize=20,
  color_code_specify=T, one_hundred_percent_stacked=F,
  axis_year_text_angle=0, language="en"){

  period <- value <- variable <- NULL

  # load translations
  i18n_header <- shiny.i18n::Translator$new(
    translation_json_path =
      system.file("mipplot", "translation_header.json", package="mipplot"))
  i18n_header$set_translation_language(language)

  i18n_region <- shiny.i18n::Translator$new(
    translation_json_path =
      system.file("mipplot", "translation_region.json", package="mipplot"))
  i18n_region$set_translation_language(language)

  i18n_variable <- shiny.i18n::Translator$new(
    translation_json_path =
      system.file("mipplot", "translation_variable.json", package="mipplot"))
  i18n_variable$set_translation_language(language)

  # if color palette isn't specified or color_code column isn't included,
  # default color palette is applied.
  # This color_map is used to sort variable names too.
  R_original_english = R
  if (color_code_specify == FALSE || !("Color_code" %in% colnames(R_original_english))) {
    color_mapper <- mipplot::mipplot_default_color_palette
  } else {
    # otherwise, generate palette.
    color_mapper <- mipplot_generate_color_mapper(R_original_english)
  }

  # apply internationalization
  for (i_mapper in 1:length(color_mapper)) {
    color_mapper[[i_mapper]] <- translate_color_mapper(color_mapper[[i_mapper]], i18n_variable)
  }
  color_mapper <- translate_color_mapper(color_mapper, i18n_variable)

  D <- translate_data_table(D, i18n_variable)
  R <- translate_rule_table(R, i18n_variable)

  # font setting (for internationalization of Chinese and Japansese)
  install_font_if_not_available(language = language)
  theme_to_specify_font <- get_theme_to_change_font(language = language)

  # init plot object list
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
        tt1 <- paste(i18n_header$t("region"), ":", i18n_region$t(r), ",  ", i18n_header$t("scenario"), ":", s, sep = "")
        tt2 <- paste(i18n_header$t("variable"), ":", as.character(Var_set[1, 2]), sep = "")
        tt3 <- paste(" [", D_RHS$unit[1], "]", sep = "")

        # Change name of variable by removing
        # common part from aggregated vairable (LHS).
        D_RHS$variable <- factor(
          gsub(paste(var_common_name, "|", sep = ""),"", D_RHS$variable, fixed = T),
          levels = rev(names(color_mapper[[var_common_name]])))

        ## Generate plots only if data is available for a given scenario.
        if (nrow(na.omit(D_RHS[D_RHS$scenario == s, ])) > 0) {

          ### FACET DOES NOT WORK IN NO DATA EXISTS.

          # Note: use if(){}else{} instead of ifelse(),
          # because ifelse() may be not available in shiny context.
          # https://github.com/rstudio/shiny/issues/2143
          if (one_hundred_percent_stacked) {
            position <- ggplot2::position_fill()
          } else {
            position <- "stack"
          }

          D_RHS_negative_values_splitted <-
              split_variable_into_positive_and_negative_parts(
                na.omit(D_RHS), 'period', 'variable', 'value')

          p_Out1 <-
            ggplot2::ggplot() +
            ggplot2::geom_area(
              data = D_RHS_negative_values_splitted,
              ggplot2::aes(x = period, y = value, fill = variable),
              position = position)

          # Plot line plot if one_hundred_percent_stacked is FALSE
          # because the maximum values of y-axes of 100% stacked graph
          # and ordinal graph are not same.
          if (!one_hundred_percent_stacked) {
            p_Out1 <- p_Out1 +
              ggplot2::geom_line(
                data = na.omit(D_LHS),
                ggplot2::aes(x = period, y = value), size = 2)
          }

          # Define plot titles and axes labels.
          p_Out1 <- p_Out1 +
            ggplot2::labs(title = tt1, subtitle = tt2, y = tt3, fill = i18n_header$t("variable"), x = i18n_header$t("period"))

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

          # set angle of x axis
          p_Out1 <- p_Out1 + ggplot2::theme(
            axis.text.x=element_text(angle=axis_year_text_angle, hjust=1))

          # apply color palette.
          if (!is.null(color_mapper[[var_common_name]])) {
            new_mapper <- color_mapper[[var_common_name]]
            new_mapper_with_negative_value_support <- add_negative_only_variable_support_to_color_mapper(new_mapper)
            p_Out1 <- p_Out1 + ggplot2::scale_fill_manual(values=new_mapper_with_negative_value_support, breaks=names(new_mapper))
          }

          # internationalization font setting
          p_Out1 <- p_Out1 + theme_to_specify_font

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
