#' @title Additivity check using bar plot
#' @description This function is used for debugging a rule table and data-set.
#' An input is a rule table and a data-set, the outputs are some bar plots
#' showing the divergence between the left-side variable and
#' the sum of the right-side variables.
#' @param D A dataframe of IAMC data in tibble format to produce area plots.
#' @param R A dataframe of data aggregation rules (meta data).
#' @param target_scenarios A character vector of scenario names
#' @param target_rule_ids A list of rule id.
#' @param show_all_scenarios Set TRUE to show all scenarios.
#' @param show_all_rule_ids Set TRUE to show all rules.
#' @param debug Set TRUE if show intermediate dataframe using View function.
#' @return A list of bar plots.
#' @examples
#' mipplot_additivity_check_bar(
#'    ar5_db_sample_data, ar5_db_sample_rule_table,
#'    target_scenarios = c("EMF27-450-Conv", "EMF27-Base-NucOff"))
#' @export
#' @importFrom rlang .data
mipplot_additivity_check_bar <- function(
  D,
  R,
  target_scenarios,
  target_rule_ids = 4,
  show_all_scenarios = FALSE,
  show_all_rule_ids = FALSE,
  debug = FALSE) {

  side <- value <- variable <- NULL

  # Initialize plot object list
  # This stores multiple ggplot objects.
  plot_object_list <- list()

  # If `show_all_scenario` is specified,
  # all scenarios will be plotted.
  # Given target_scenarios are ignored.
  if (show_all_scenarios) {
    target_scenarios <- as.character(levels(D$scenario))
  }

  # If `show_all_rule_ids` is specified,
  # all Rule IDs will be plotted.
  # Given target_rule_ids are ignored.
  if (show_all_rule_ids) {
    target_rule_ids <- unique(R$Rule_ID)
  }

  # Create all combinations of target_scenarios and target_rule_ids conditions
  # `expand.grid` is used for creating Cartesian product.
  condition_combinations <- expand.grid(
    scenario = unique(target_scenarios),
    rule_id = unique(target_rule_ids))

  # Make a plot for each condition.
  for (i_combination in 1:nrow(condition_combinations)) {

    # Extract scenario and rule ID from condition_combination
    condition_combination <- condition_combinations[i_combination, ]
    target_scenario <- as.character(condition_combination$scenario)
    target_rule_id <- condition_combination$rule_id


    Var_set <- R[R$Rule_ID == target_rule_id,]

    # Select data
    D[D$variable %in% Var_set$Left_side,] %>%
      dplyr::mutate(side = "L") -> D_temp1

    D[D$variable %in% Var_set$Right_side,] %>%
      dplyr::mutate(side = "R") -> D_temp2

    rbind(D_temp1, D_temp2) -> D_temp0

    D_temp0 %>% dplyr::filter(.data$scenario == target_scenario) -> D_temp

    # Display the table to be plotted
    if (debug) {
      utils::View(D_temp)
    }

    # Check the stacked graph to see if the left and right sides are consistent.
    generated_plot <- D_temp %>%
      ggplot() + geom_bar(aes(x = side, y = value, fill = variable), stat = "identity") +
      facet_grid(. ~ model) +
      theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(str_interp("scenario=${target_scenario}, year=period")) +
      ylab(str_interp("${D_temp$variable[1]} [${D_temp$unit[1]}]"))

    # Append generated plot to plot object list
    plot_object_list[[length(plot_object_list) + 1]] <- generated_plot
  }

  return(plot_object_list)

}
