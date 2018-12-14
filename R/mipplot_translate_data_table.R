translate_rule_table <- function(english_rule_table, language) {
  if (language == 'en') {
    return(english_rule_table)
  }

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

  translated_rule_table <- english_rule_table
  translated_rule_table$Left_side <- as.character(translated_rule_table$Left_side)
  translated_rule_table$Right_side <- as.character(translated_rule_table$Right_side)
  for (i_row in 1:nrow(english_rule_table)) {
    translated_rule_table[[i_row, 'Left_side']] <-
      i18n_variable$t(english_rule_table[[i_row, 'Left_side']])
    translated_rule_table[[i_row, 'Right_side']] <-
      i18n_variable$t(english_rule_table[[i_row, 'Right_side']])
  }
  #translated_rule_table$Left_side <- as.factor(translated_rule_table$Left_side)
  #translated_rule_table$Right_side <- as.factor(translated_rule_table$Right_side)

  return(translated_rule_table)
}

translate_data_table <- function(english_data_table, language) {

  if (language=='en') {
    return(english_data_table);
  }

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

  translated_data_table <- english_data_table

  if ('variable' %in% colnames(english_data_table)) {
    translated_data_table$variable <- as.character(translated_data_table$variable)

    if (nrow(english_data_table) == 0) return(english_data_table)

    for (i_row in 1:nrow(english_data_table)) {
      translated_data_table[[i_row, 'variable']] <-
        i18n_variable$t(as.character(english_data_table[[i_row, 'variable']]))
    }
    translated_data_table$variable <- as.factor(translated_data_table$variable)
  }

  return(translated_data_table)
}


translate_color_mapper <- function(english_color_mapper, language) {

  if (language == 'en') {
    return(english_color_mapper)
  }

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

  translated_color_mapper <- english_color_mapper

  for (i_elem in 1:length(english_color_mapper)) {
    names(translated_color_mapper)[i_elem] <-
      i18n_variable$t(names(english_color_mapper)[i_elem])
  }

  return(translated_color_mapper)
}

translate_vector <- function(english_vector, language) {

  if (language == 'en') {
    return(english_vector)
  }

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

  translated_vector <- english_vector

  for (i_elem in 1:length(english_vector)) {
    translated_vector[i_elem] <-
      i18n_variable$t(english_vector[i_elem])
  }

  return(translated_vector)
}

