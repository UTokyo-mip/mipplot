
#--------------------------------------------------------------------
# PLOTTING FUNCTION: LINE
#--------------------------------------------------------------------

#' @title Line plot from IAMC data
#' @description The function arguments include the input dataframe,
#'              labels for the plot/axes/legend, and faceting dimensions
#' @param D A dataframe of IAMC data in tibble format to produce plots.
#' @param region A list of regions.
#' @param variable A list of variables.
#' @param colorby an axis for color setting.
#' @param linetypeby an axis for line type setting.
#' @param shapeby an axis for shape setting.
#' @param scenario A list of scenarios.
#' @param facet_x facet_x
#' @param facet_y facet_y
#' @param legend set TRUE to plot legend. default is TRUE.
#' @param PRINT_OUT set TRUE to generate PDF files.
#' @param DEBUG set TRUE to show debug messages.
#' @param axis_year_text_angle text angle of x axis
#' @param language A string of language. Possible values are "en", "jp",
#' "es", "zh-cn", "zh-tw". The default value is "en".
#' @param max_scenarios Maximum number of scenarios to be shown. If legend is FALSE, this option is .
#' @param max_models Maximum number of models to be shown. If legend is FALSE, this option is
#' @return A list of line plots.
#' @importFrom dplyr select filter
#' @importFrom utils head
#' @examples
#' \donttest{
#' mipplot_line(ar5_db_sample_data)
#' }
#' @export

mipplot_line <- function(
  D, region = levels(D$region), variable = levels(D$variable),
  colorby = "scenario", linetypeby = "model", shapeby = "model",
  scenario = levels(D$scenario), facet_x = NULL,
  facet_y = NULL, legend = TRUE, PRINT_OUT = FALSE, DEBUG = TRUE,
  axis_year_text_angle=0, language="en",
  max_scenarios = 15, max_models = 15) {

  model <- period <- value <- NULL

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

  # apply internationalization
  if (language != "en") {
    D <- translate_data_table(D, i18n_variable)
  }

  # font setting (for internationalization of Chinese and Japanese)
  install_font_if_not_available(language = language)
  theme_to_specify_font <- get_theme_to_change_font(language = language)

  p_list1 <- list()

  for (r in levels(as.factor(region))) {

    for (v in levels(as.factor(variable))) {

        if(language != "en") {
          v <- i18n_variable$t(v)
        }


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

        ## Limit number of lines to be plotted

        # get all scenarios
        D_sub %>% select(scenario) %>% unique %>%
          unlist %>% as.character -> scenario_list

        # get all models
        D_sub %>% select(model) %>% unique %>%
          unlist %>% as.character -> model_list

        # reduce data
        if (legend) {
          if (max_scenarios < length(scenario_list)) {
            warning(paste("too many scenarios. first", max_scenarios, "are shown."))
            scenario_list %>% head(max_scenarios) -> scenario_list_part
            D_sub %>% filter(scenario %in% scenario_list_part) -> D_sub
          }
          if (max_models < length(model_list)) {
            warning(paste("too many models. first", max_models, "are shown."))
            model_list %>% head(max_models) -> model_list_part
            D_sub %>% filter(model %in% model_list_part) -> D_sub
          }
        }

        ## Title
        tt1 <- paste(i18n_header$t("region"), ":", i18n_region$t(r), sep = "")
        tt2 <- paste(i18n_header$t("variable"), ":", v, sep = "")
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
            ggplot2::labs(title = tt1, subtitle = tt2, y = tt3, x = i18n_header$t("period"))

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
          ggplot2::theme(text = ggplot2::element_text(size = 20)) +
          ggplot2::labs(color=i18n_header$t(colorby), linetype=i18n_header$t(linetypeby), shape=i18n_header$t(linetypeby))

        # set angle of x axis
        p_Out1 <- p_Out1 + ggplot2::theme(
          axis.text.x=element_text(angle=axis_year_text_angle, hjust=1))

        # Remove the legend if legend option is FALSE
        if (!legend) {
          p_Out1 <- p_Out1 + ggplot2::theme(legend.position = "none")
        }


        ## STORE PLOTS TO LIST
        p_list1[[length(p_list1) + 1]] <- p_Out1
    }
  }

  if (PRINT_OUT == TRUE) {

    mipplot_print_pdf(p_list1, filelabel = "line")

  }

  return(p_list1)
}
