translate_rule_table <- function(english_rule_table, i18n_variable) {

  translated_rule_table <- english_rule_table
  translated_rule_table$Left_side <- as.character(translated_rule_table$Left_side)
  translated_rule_table$Right_side <- as.character(translated_rule_table$Right_side)
  for (i_row in 1:nrow(english_rule_table)) {
    if (english_rule_table[[i_row, 'Left_side']] != "") {
      translated_rule_table[[i_row, 'Left_side']] <-
        i18n_variable$t(english_rule_table[[i_row, 'Left_side']])
    }
    if (english_rule_table[[i_row, 'Right_side']] != "") {
      translated_rule_table[[i_row, 'Right_side']] <-
        i18n_variable$t(english_rule_table[[i_row, 'Right_side']])
    }
  }

  return(translated_rule_table)
}

translate_data_table <- function(english_data_table, i18n_variable) {

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


translate_color_mapper <- function(english_color_mapper, i18n_variable) {

  translated_color_mapper <- english_color_mapper

  for (i_elem in 1:length(english_color_mapper)) {

    names(translated_color_mapper)[i_elem] <-
      i18n_variable$t(names(english_color_mapper)[i_elem])
  }

  # if translation is not injective mapping,
  # duplicated color setting will be generated.
  dup_indices = duplicated(names(translated_color_mapper))

  return(translated_color_mapper[!dup_indices])
}

convert_language_specifier_flavor_from_mipplot_to_showtext <- 
    function (language_in_mipplot) {
  if (language_in_mipplot == "zh-cn") {
    return("CN")
  } else if (language_in_mipplot == "zh-tw") {
    return("TW")
  } else if (language_in_mipplot == "jp") {
    return("JP")
  } else if (language_in_mipplot == "en") {
    return("EN")
  } else if (language_in_mipplot == "es") {
    return("ES")
  } else {
    stop("unsupported language specifier")
  }
}

get_font_name <- function(language) {
  if (language == "zh-cn") {
    return("source-han-sans-cn")
  } else if (language == "zh-tw") {
    return("source-han-sans-tw")
  } else if (language == "jp") {
    return("source-han-sans-jp")
  } else if (language == "en") {
    stop("not jp, cn, tw")
    return("")
  } else if (language == "es") {
    stop("not jp, cn, tw")
    return("")
  } else {
    stop("unsupported language specifier")
  }
}

install_font_if_not_available <- function(language) {
  if (language %in% c("zh-cn", "zh-tw", "jp")) {

    if (!get_font_name(language) %in% showtextdb::font_installed()) {
      print("Installing fonts for internationalization (not system wide)")
      print("It takes some time only the first call with each language settings.")
      language_specifier_for_showtext <- 
        convert_language_specifier_flavor_from_mipplot_to_showtext(language)
      showtextdb::font_install(
        showtextdb::source_han_sans(lang = language_specifier_for_showtext))
    } else {
      # do nothing
    }
  } else {
    # do nothing
  }
}

get_theme_to_change_font <- function(language) {
  if (language %in% c("zh-cn", "zh-tw", "jp")) {
    return(theme(text = element_text(family = get_font_name(language))))
  } else {
    return(theme())
  }
  
  
}
