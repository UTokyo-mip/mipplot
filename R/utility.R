#' @title correct data format of given IAMC data table
#' @description Dataset in IAMC format rule is not rigid.
#' This function corrects data types of columns in the dataset.
#' If necessary columns is missing, it throws exception.
#' Output object of this function is as follows:
#'
#' type: data.table
#' columns:
#'   model:    factor
#'   scenario: factor
#'   region:   factor
#'   variable: factor
#'   unit:     factor
#'   period:   double
#'   value:    double
#' @param iamc_data IAMC dataset described above
#' @return modified dataframe
#' @export
correct_format_of_iamc_dataframe <- function(iamc_data) {

  check_format_of_iamc_dataframe(iamc_data)
  corrected_iamc_data <- change_data_types_of_iamc_dataframe(iamc_data)

  return(corrected_iamc_data);
}

#' @title check if the format of given data is valid
#' as an IAMC dataset.
#' @param iamc_data IAMC dataset in dataframe format
#' @return TRUE if it is valid
check_format_of_iamc_dataframe <- function(iamc_data) {

  check_column_availability(iamc_data)

}

#' @title check if the dataset has required fields of IAMC dataset
#' @description if dataset has all required fields, then returns TRUE
#' @param iamc_data IAMC data frame
#' @return boolean flag
check_column_availability <- function(iamc_data) {

  # list of required field of IAMC data.
  NECESSARY_COLUMNS = c(
    "model", "scenario", "region", "variable", "unit", "period", "value")

  # check availability of columns
  for (necessary_column in NECESSARY_COLUMNS) {
    if (!(necessary_column %in% names(iamc_data))) {
      stop(sprintf("iamc_data doesn't contain %s column", necessary_column))
    }
  }
}

#' @title change column data type in data-set
#' @description change column data type in data-set to be able to
#' be treated as an IAMC data-set.
#' @param iamc_data data frame which has columns `model`, `scenario`, `region`, `variable`, `period`, `unit`
#' @return converted data-frame.
change_data_types_of_iamc_dataframe <- function(iamc_data) {

  # convert data type
  iamc_data$model <- as.factor(iamc_data$model)
  iamc_data$scenario <- as.factor(iamc_data$scenario)
  iamc_data$region <- as.factor(iamc_data$region)
  iamc_data$variable <- as.factor(iamc_data$variable)
  iamc_data$period <- as.double(iamc_data$period)
  iamc_data$unit <- as.factor(iamc_data$unit)

  return(iamc_data)
}

#' @title Add credit text to plots
#' @description Add credit text to a list of ggplot2 plot objects
#' @param list_of_plot list of ggplot2 plot objects
#' @return list of modified ggplot2 plot objects
add_credit_to_list_of_plot <- function(list_of_plot) {
  return(
    lapply(list_of_plot, add_credit_to_plot)
  )
}

#' @title Add credit text to a plot
#' @description Add credit text and project URL to a ggplot2 plot object
#' @param plot_object ggplot2 plot object
#' @return modified ggplot2 plot object
add_credit_to_plot <- function(plot_object) {
  return (
    plot_object +

      # credit text
      ggplot2::labs(
        caption = "Created with https://github.com/UTokyo-mip/mipplot") +

      # formatting of the text
      ggplot2::theme(
        plot.caption = ggplot2::element_text(size=10, colour = "#666666"))
  )
}



#' @title Get expression of vector of string in string format
#' @description To evaluate expression, get string of expression
#' @param vector_of_strings vector of strings, such as c("A", "B")
#' @return An R code representing character vector
get_string_expression_of_vector_of_strings <- function(vector_of_strings) {
  return (paste("c(\"", stringr::str_c(vector_of_strings, collapse = "\", \""), "\")", sep=""))
}

#' @title Get variable-group-name list
#' @description variable-group is a combination of one LHS and one or more RHS.
#' this function outputs the list of names of variable-group in given rule-table.
#' the format of return value is "LHS|RHS1,RHS2,RHS3,...".
#' @param rule_table A rule table
#' @return variable group name
#' @examples
#' get_variable_group_name_list(ar5_db_sample_rule_table)
#' @export
get_variable_group_name_list <- function(rule_table) {

  # initialize result
  group_name_list <- c()

  # initialize temporary variables
  current_left_side_name <- ""
  current_right_side_name_list <- c()

  # scan all rows in rule_table
  for (i_row in 1:nrow(rule_table)) {

    new_row <- rule_table[i_row, ]

    # if find new LHS entry
    if (new_row$Left_side != "") {

      # add current_group_name to result
      if (i_row > 1) {

        # formatting such as
        # current_left_side_name = current_right_side_name_list[1] + current_...[2] + ...
        new_group_name <- generate_variable_group_name_from_lhs_and_rhs_list(
          current_left_side_name, current_right_side_name_list)

        # add new variable group name
        group_name_list <- c(group_name_list, new_group_name)
      }

      # re-set temporary variables
      current_left_side_name <- new_row$Left_side  #paste(, " = ", sep="")
      current_right_side_name_list <- c()
    }

    # if find new RHS entry
    if (new_row$Right_side != "") {

      # remove parent part, then get base name
      # new_right_side_name <- stringr::str_replace(new_row$Right_side, "^.*\\|", "")
      new_right_side_name <- gsub(paste(current_left_side_name, "|", sep = ""), "", new_row$Right_side, fixed = TRUE)

      # add RHS entry to current_group_name with " + " separator.
      current_right_side_name_list <- c(current_right_side_name_list, new_right_side_name)
    }
  }

  # add last entry to result
  ## TODO:
  group_name_list <- c(group_name_list, generate_variable_group_name_from_lhs_and_rhs_list(
    current_left_side_name, current_right_side_name_list))

  # remove "|" or "," if they are placed on the end of current_group_name
  # ex. "Population|" or "Emissions|CO2,Land Use,"
  # group_name_list <- stringr::str_replace_all(group_name_list, "\\|$|,$", "")

  return(group_name_list)
}

generate_variable_group_name_from_lhs_and_rhs_list <- function(lhs, rhs_list) {

  if (length(rhs_list) > 0) {

    return(paste(lhs, stringr::str_flatten(rhs_list, collapse = " + "), sep = " = "))
  } else {
    # if rhs is empty (ex. Population in AR5 rule table)
    # output lhs only.
    return(lhs)
  }

}

#' @title Get variable name list in given variable-group
#' @description Scan rule-table and extract variable names in given variable-group.
#' @param group_name variable-group-name
#' @return A list of strings representing variable names
#' @examples
#' get_variable_name_list_in_variable_group(
#'   "Final Energy|Industry,Residential and Commercial,Transportation")
#' @export
get_variable_name_list_in_variable_group <- function(group_name) {

  # CASE: includes both of LHS and RHS.

  # check if group_name includes "="
  if (stringr::str_detect(group_name, "\\=")) {

    # trim parent level part and get child level part
    # ex.
    # input: "Emissions|CO2|Fossil Fuels and Industry = Energy Demand + Energy Supply"
    # output: "Energy Demand,Energy Supply"
    child_part <- stringr::str_replace_all(group_name, "^.* \\= ", "")

    # separate elements in child level part
    # ex.
    # input: "Energy Demand + Energy Supply"
    # output: c("Energy Demand", "Energy Supply")
    child_part_elements <- stringr::str_split(child_part, " \\+ ")[[1]]

    # trim child level part and get parent level part
    # ex.
    # input: "Emissions|CO2|Fossil Fuels and Industry = Energy Demand + Energy Supply"
    # output: "Energy Demand,Energy Supply"
    parent_part <- gsub("^| \\=.*$", "", group_name)

    if (is.na(parent_part)) {

      return(child_part)

    } else {

      full_path_variable_name_list <- paste(parent_part, child_part_elements, sep="|")

      parent_part_without_last_vertical_line <- stringr::str_replace(parent_part, "\\|$", "")

      return(c(parent_part_without_last_vertical_line, full_path_variable_name_list))
    }

  } else {

    return(group_name)

  }


}

#' @title Split variable into positive and negative parts
#' @description Generally, the range of the input value of stacked chart is greater than or equal to zero.
#' This function splits variable into positive and negative parts in order to include negative values to stacked chart.
#' @param df_all input data frame
#' @param domain_column_name domain column name, such as year
#' @param variable_column_name variable column name, such as 'coal'
#' @param value_column_name value column name, such as 'val'
#' @param variable_name_converter function which convert original variable name into its negative part name
#' @param increment_of_domain_in_interpolation step size for interpolation
#' @return modified data frame
#' @importFrom stats approxfun
#' @importFrom dplyr filter
#' @export
split_variable_into_positive_and_negative_parts <- function(
  df_all, domain_column_name, variable_column_name,
    value_column_name,
    variable_name_converter = function(x){paste(x, '_negative', sep="")},
    increment_of_domain_in_interpolation = 0.1) {

  model <- scenario <- region <-

  # data frame to be returned
  df_new <- df_all[0,]

  conditions <- expand.grid(
    unique(df_all[['model']]),
    unique(df_all[['scenario']]),
    unique(df_all[['region']]))
  colnames(conditions) <- c("model", "scenario", "region")

  for (i_condition in 1:nrow(conditions)) {

    current_model <- conditions[['model']][i_condition]
    current_scenario <- conditions[['scenario']][i_condition]
    current_region <- conditions[['region']][i_condition]

    df_all %>% filter(model == current_model) %>%
      filter(scenario == current_scenario) %>% filter(region == current_region) -> df

    # extract the values of domain axis in df
    # (example) values_of_domain == c(2000, 2050, 2075, 2100)
    values_of_domain <- df[domain_column_name] %>% unique %>% unlist %>% as.vector

    # extract the original variable name list
    # (example) original_variable_name_list == c("coal", "solar")
    original_variable_name_list <- df[variable_column_name] %>% unique %>% unlist %>% as.vector

    # the loop of original variable name
    # (example) original_variable_name == "coal"
    for (original_variable_name in original_variable_name_list) {

      # partial_df is df which includes only `original_variable_name`
      # notes: `!!rlang::sym` is used to get symbol from character
      #         because filter() couldn't accept string expression as a search query
      #
      # (example)
      # > partial_df
      #   y       val variable
      # 1 2000   +100     coal
      # 2 2050   -100     coal
      # 3 2075   0        coal
      #
      partial_df <- df %>% filter((!!rlang::sym(variable_column_name)) == original_variable_name)

      #
      # performs linear interpolation to get following dataframe
      #
      # (example)
      # > partial_df
      #   y       val variable
      # 1 2000   +100     coal
      # 2 2025   0        coal
      # 3 2050   -100     coal
      # 4 2075   0        coal
      #
      new_domain_for_interpolation <- seq(
        from=min(partial_df[domain_column_name]),
        to=max(partial_df[domain_column_name]),
        by=increment_of_domain_in_interpolation)

          # new_domain_for_interpolation <- setdiff(
          # new_domain_for_interpolation,
          #   unlist(partial_df[domain_column_name]))

      func_get_value_at_given_domain <- approxfun(
          unlist(partial_df[domain_column_name]),
          y=unlist(partial_df[value_column_name]))

      # initialize empty dataframe
      new_partial_df <- partial_df[0, ]

      # use first row as a template.
      # we will copy it then modify domain_column, value_column and variable_column.
      template_row <- partial_df[1, ]

      template_row[variable_column_name] <- original_variable_name
      template_row['model'] <- current_model
      template_row['scenario'] <- current_scenario
      template_row['region'] <- current_region


      partial_df <- template_row[rep(seq_len(nrow(template_row)), each = length(new_domain_for_interpolation)),]
      partial_df[domain_column_name] <- new_domain_for_interpolation
      partial_df[value_column_name] <- func_get_value_at_given_domain(new_domain_for_interpolation)

      # command <-
      #   paste("add_row(",
      #   ".data", "=", "partial_df", ",",
      #   domain_column_name, "=", "unlist(new_domain_for_interpolation)", ",",
      #   value_column_name, "=", "func_get_value_at_given_domain(new_domain_for_interpolation)", ",",
      #   variable_column_name, "=", paste('"', original_variable_name,'"', sep=""),
      #   ")", sep=" ")

      # partial_df <- eval(parse(text=command))

      # get positive part
      positive_partial_df <- partial_df %>% filter(0 <= (!!rlang::sym(value_column_name)))
      # positive_partial_df <- interpolate_missing_values(
      #     positive_partial_df, domain_column_name, values_of_domain, value_column_name, 0)

      # append new positive part
      if (0 < nrow(positive_partial_df)) {
        df_new <- rbind(df_new, positive_partial_df)
      }

      # get negative part
      negative_partial_df <- partial_df %>% filter((!!rlang::sym(value_column_name)) < 0)
      # negative_partial_df <- interpolate_missing_values(
      #   negative_partial_df, domain_column_name, values_of_domain, value_column_name, 0)


      # append new negative part
      # append new positive part
      if (0 < nrow(negative_partial_df)) {
        # modify column name of negative part
        negative_partial_df[variable_column_name] <- variable_name_converter(original_variable_name)
        df_new <- rbind(df_new, negative_partial_df)
      }
    }

  }

  return(df_new)
}


interpolate_missing_values <- function(
    df, domain_column_name, domain_values, value_column_name, default_value) {

  # domain values not observed
  # (example)
  # domain_values ................ c(2000, 2100, 2200)
  # domain_values_observed ....... c(2000,       2200)
  # domain_values_not_observed ... c(      2100      )
  domain_values_observed <- df[domain_column_name] %>% unique %>% unlist %>% as.vector
  domain_values_not_observed <- setdiff(domain_values, domain_values_observed)

  # initialize empty dataframe
  additional_df <- df[0, ]

  # use first row as a template.
  # we will copy it then modify its value column with specified constant value,
  # and modify its domain column with unobserved domain value.
  template_row <- df[1, ]

  for (new_domain_value in domain_values_not_observed) {

    # copy template
    new_row <- template_row

    # replace domain and value
    new_row[domain_column_name] <- new_domain_value
    new_row[value_column_name] <- default_value

    # add the row to additoinal_df
    additional_df <- rbind(additional_df, new_row)
  }

  # add additional_df to df then return
  return(rbind(df, additional_df))
}

add_negative_only_variable_support_to_color_mapper <- function(original_color_mapper) {

  additional_color_mapper <- original_color_mapper
  names(additional_color_mapper) <- paste(names(additional_color_mapper), '_negative', sep='')

  return(c(original_color_mapper, additional_color_mapper))
}
