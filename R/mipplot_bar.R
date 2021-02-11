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
#' @param one_hundred_percent_stacked set TRUE if you want a graph of 100\% stacked, set this to TRUE.
#' @param axis_scenario_text_angle text angle of x axis
#' @param language A string of language. Possible values are "en", "jp",
#' "es", "zh-cn", "zh-tw". The default value is "en".
#' @return A list of bar plots.
#' @examples
#' \dontrun{
#' mipplot_bar(ar5_db_sample_data, ar5_db_sample_rule_table)
#' }
#' @export

# Faceting approach: allow only for 1 dimension (facet_wrap)
# if one dimension is replicated in horizontal axis.

mipplot_bar <- function(
  D, R,region = levels(D$region), xby = "scenario",
  target_year = levels(as.factor(D$period)),
  facet_x = NULL, facet_y = NULL, PRINT_OUT = F, DEBUG = T, fontsize = 20,
  color_code_specify = T, one_hundred_percent_stacked = F,
  axis_scenario_text_angle = 0, language="en") {

  # REPLACED THIS FUNCTION WITH 1-LINE CODE (SEE LINE 52).
  # wrap_text <- function(x, width=60){
  #   sapply(x,function(x) {paste(strwrap(x,width),collapse="\n")})
  # }

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

        # if color palette isn't specified or color_code column isn't included,
        # default color palette is applied.
        # This color_map is used to sort variable names too.
        if (color_code_specify == FALSE || !("Color_code" %in% colnames(R))) {
          color_mapper <- mipplot_default_color_palette
        } else {
          # otherwise, generate palette.
          color_mapper <- mipplot_generate_color_mapper(R)
        }

        # Title
        tt1 <- paste(i18n_header$t("region"), ":", i18n_region$t(r), ",  ",
                     i18n_header$t("period"), ":", ty, sep = "")
        tt2 <- paste(i18n_header$t("variable"), ":", as.character(Var_set[1, 2]), sep = "")
        tt3 <- paste(" [", D_RHS$unit[1], "]", sep = "")

        # Change name of variable by removing
        # common part from aggregated vairable (LHS).
        D_RHS$variable <- factor(
          gsub(paste(var_common_name, "|", sep = ""),"", D_RHS$variable, fixed = T),
          levels = rev(names(color_mapper[[var_common_name]])))

        # Only generate plots if data is available for a region.
        if (nrow(na.omit(D_RHS[D_RHS$region == r, ]))) {

          # Note: use if(){}else{} instead of ifelse(),
          # because ifelse() may be not available in shiny context.
          # https://github.com/rstudio/shiny/issues/2143
          if (one_hundred_percent_stacked) {
            position <- ggplot2::position_fill()
          } else {
            position <- "stack"
          }

          ## Bar plots: using right-hand-side values of target rule.
          p_Out1 <- ggplot2::ggplot(
            na.omit(D_RHS),
            ggplot2::aes(y = value, fill = variable)) +
            ggplot2::geom_bar(stat = "identity", ggplot2::aes_(x = as.name(xby)),
                              position = position) +
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
              ggplot2::facet_wrap(~eval(parse(text=facet_by))) +
              ggplot2::xlab(i18n_header$t(facet_by)) +
              ggplot2::guides(fill=guide_legend(title=i18n_header$t("variable")))
          }

          p_Out1 <- p_Out1 + ggplot2::theme(
            text = ggplot2::element_text(size = fontsize))

          # set angle of x axis
          if (axis_scenario_text_angle != 0) {

            # not to set `hjust`` to 1 when label angle is 0
            # to avoid label to be trimmed
            p_Out1 <- p_Out1 + ggplot2::theme(
              axis.text.x=element_text(angle=axis_scenario_text_angle, hjust=1))
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
