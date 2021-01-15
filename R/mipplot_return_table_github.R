#--------------------------------------------------------------------
# PLOTTING FUNCTION: MUTATED TABLE.
# AUTHORS: SNEH DESHPANDE, KEITARO HANZAWA
#--------------------------------------------------------------------
#====================================================================
# Mutated Table using filtered variable from the rule table
#     The function arguments inlcude the input dataframes:
#     The SR15 dataset and the Rule Table and returns a mutated
#     table with variable, value, model, scenario, region, period
#====================================================================

#' @title Mutated table of SR15 Data
#' @description  Mutated Table using filtered variable from the rule table
#'          The function arguments inlcude the input dataframes:
#'          The SR15 dataset and the Rule Table and returns a mutated
#'          table with variable, value, model, scenario, region, period
#' @param D A dataframe of IAMC data in tibble format to produce mutated table
#' @param R A dataframe of data aggregation rules
#' @return Mutated Table of model,scenario,region,variable,unit,period,value
#' @examples
#' \dontrun{
#' mipplot_return_table(sr15_sample_data, sr15_sample_conversion_rule_table)
#' }
#' @export

mipplot_return_table <- function(D, R) {

  #Assign value NULL to results
  results <- NULL

  for (i in levels(as.factor(R$rule_id))) {

    #Select Data
    # ? omit NA?
    D_x_var <- D %>% dplyr::filter(variable == variable)

    D_y_var <- D %>% dplyr::filter(variable == variable)

    # Common Part of Var-name

    # Rename rule table for left_join
    R2 <- R %>% reshape::rename(
      c("x_variable" = "variable.x",
        "y_variable" = "variable.y")
    )

    # Full join the xvar and yvar data
    D_R <- dplyr::left_join(D_x_var, D_y_var, by = c('model', 'period', 'region', 'scenario'))

    #Left join with rule_table
    D_R_1 <- dplyr::left_join(R2, D_R, by = c("variable.x", "variable.y")) %>%
      dplyr::group_by(model, period, region, scenario) %>% #group to keep columns in table
      dplyr::mutate(unit = paste(unit.x, unit.y, sep = "/")) %>%
      dplyr::mutate(value = value.x / value.y) %>%
      rename(
        variable = new_variable
      )
    #Assigns final table to results
    if (is.null(results)) {
      results <- D_R_1
    }
    #Return final table (ungrouped)

    return(results %>% select(model, scenario, region, variable, unit, period, value) %>% as.data.frame())
  }
}
