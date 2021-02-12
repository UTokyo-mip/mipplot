#--------------------------------------------------------------------
# PLOTTING FUNCTION: BOX
#--------------------------------------------------------------------

#' @title Box plot from IAMC data
#' @description The function arguments include the input dataframe,
#'              labels for the plot/axes/legend, and faceting dimensions
#' @param D A dataframe of IAMC data in tibble format to produce plots.
#' @param region A list of regions.
#' @param variable A list of variables.
#' @param target_year target year.
#' @param PRINT_OUT set TRUE to generate PDF file.
#' @param DEBUG set TRUE to show debug messages.
#' @param language A string of language. Possible values are "en", "jp",
#' "es", "zh-cn", "zh-tw". The default value is "en".
#' @return A list of box plots.
#' @examples
#' \dontrun{
#' mipplot_box(ar5_db_sample_data)
#' }
#' @export

mipplot_box <- function(
  D, region=levels(D$region),
  variable=levels(D$variable),
  target_year=levels(as.factor(D$period)), PRINT_OUT=F, DEBUG=T,
  language="en") {

  scenario <- value <- NULL

  # load translations
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

  # apply internationalization
  D <- translate_data_table(D, i18n_variable)

  # font setting (for internationalization of Chinese and Japansese)
  install_font_if_not_available(language = language)
  theme_to_specify_font <- get_theme_to_change_font(language = language)

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
        tt1 <- paste(i18n_header$t("region"), ":", i18n_region$t(r),
                     ",  ", i18n_header$t("period"), ":", ty, sep = "")
        tt2 <- paste(i18n_header$t("variable"), ":", as.character(v), sep = "")
        tt3 <- paste(' [', D_sub$unit[1], ']', sep = "")

         ## Box plots: using scenario
         p_Out1 <- ggplot2::ggplot(
           D_sub, ggplot2::aes(x = scenario, y = value)) +
           ggplot2::geom_boxplot() +
           ggplot2::labs(title = tt1, subtitle = tt2, y = tt3, x = i18n_header$t("scenario")) +
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

