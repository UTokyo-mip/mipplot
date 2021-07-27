#' @title check additivity of rules and data
#' @description This function is used for debugging a rule table and data-set.
#' An input is a rule table and a data-set, the outputs are some area plots
#' showing the divergence between the left-side variable and
#' the sum of the right-side variables.
#' @param D A dataframe of IAMC data in tibble format to produce area plots.
#' @param R A dataframe of data aggregation rules (meta data).
#' @param max_n_plots The maximum number of output plots.
#' @param plot_all set FALSE to plot only inconsistent combinations
#' @return A list of area plots.
#' @examples
#' \donttest{
#' if (interactive()) {
#'   mipplot_additivity_check(
#'     ar5_db_sample_data, ar5_db_sample_rule_table, max_n_plots = 10)
#'   }
#' }
#' @export
#' @importFrom rlang .data
mipplot_additivity_check <- function(
  D, R, max_n_plots = Inf, plot_all = FALSE) {

  # Define `inconsistent` that maximum of absolute relative
  # error is larger than 1%.
  THRESHOLD_OF_INCONSISTENCY = 0.01

  # Compute maximum of absolute relative deviation for all combination.
  max_abs_relative_deviation_table <- get_max_abs_relative_deviation_table(D, R)

  # Sort combinations in descending order of maximum of absolute relative deviation.
  sorted_max_abs_relative_deviation_table <-
    max_abs_relative_deviation_table %>% dplyr::arrange(dplyr::desc(.data$max_abs_relative_deviation))

  # Limited conditions will be plotted.
  n_plots <- min(max_n_plots, nrow(sorted_max_abs_relative_deviation_table))

  # A container of ggplot2 objects
  p_list = list()

  cat("Found inconsistencies in the following conditions:\n")

  # Draw area plots for each conditions
  for (i_condition in 1:n_plots) {

    # Extract ith condition
    this_condition <-
      sorted_max_abs_relative_deviation_table[i_condition, ]

    # Skip if maximum of absolute relative error is less than 1%.
    if (!plot_all) {
      if (this_condition$max_abs_relative_deviation < THRESHOLD_OF_INCONSISTENCY) {
        next
      }
    }

    # Draw plot
    this_plot <- area_plot_of_specific_rule_id(D, R,
                              region = as.character(this_condition$region),
                              scenario = as.character(this_condition$scenario),
                              rule_ids = as.character(this_condition$rule_id))

    # Append new plot to the container
    p_list[[length(p_list) + 1]] <- this_plot[[1]]

    # Print the condition if absolute relative error is larger than or equal to 1%
    if (this_condition$max_abs_relative_deviation >= THRESHOLD_OF_INCONSISTENCY) {
      print_condition(this_condition)
    }


  }

  return(p_list)
}

print_condition <- function(condition) {
  cat(paste(paste(
    paste("variable:", condition$variable),
    paste("region:", condition$region),
    paste("scenario:", condition$scenario),
    paste("model:", condition$model),
    sep="\t"), "\n", sep=""))
}


get_max_abs_relative_deviation_table <- function(D, R) {

  model <- period <- value <- rhs_sum_value <-
    rule_id <- variable <- region <- scenario <- abs_relative_deviation <- NULL

  result <- NULL

  for (i in levels(as.factor(R$Rule_ID))){

    for (r in levels(as.factor(D$region))){

      for (s in levels(as.factor(D$scenario))){

        Var_set <- R[R$Rule_ID == i, ]

        ## Select data
        D_LHS <- D[D$region == r & D$scenario == s &
                     D$variable %in% Var_set$Left_side, ]

        D_RHS <- D[D$region == r & D$scenario == s &
                     D$variable %in% Var_set$Right_side, ]

        # Common Part of Var-name
        var_common_name <- Var_set[1, 2]

        # Change name of variable by removing
        # common part from aggregated vairable (LHS).
        D_RHS$variable <- factor(
          gsub(paste(var_common_name, "|", sep = ""),"", D_RHS$variable, fixed = T))

        ## Generate plots only if data is available for a given scenario.
        if (nrow(na.omit(D_RHS[D_RHS$scenario == s, ])) > 0) {

          this_abs_relative_deviation_table <- dplyr::full_join(
            D_RHS %>% dplyr::group_by(model, period) %>% dplyr::summarize(rhs_sum_value = sum(value)),
              D_LHS, by = c('model', 'period')) %>% dplyr::mutate(
                abs_relative_deviation =
                  abs((rhs_sum_value - value) / value)) %>%
            dplyr::mutate(rule_id = i) # add rule id column

          if (is.null(result)) {
            result <- this_abs_relative_deviation_table
          } else {
            result <- dplyr::bind_rows(result, this_abs_relative_deviation_table)
          }

          ## Title
          tt1 <- paste("region:", r, ",  scenario:", s, sep = "")
          tt2 <- paste("variable:", as.character(Var_set[1, 2]), sep = "")
          tt3 <- paste(" [", D_RHS$unit[1], "]", sep = "")
          # print(paste(tt1, tt2, tt3))

        }
      }
    }
  }

  return(
    result %>% dplyr::group_by(rule_id, variable, region, scenario, model) %>%
      dplyr::summarize(max_abs_relative_deviation = max(abs_relative_deviation, na.rm = TRUE)) %>%
      dplyr::ungroup())
}

#' @importFrom rlang .data
#' @importFrom stats na.omit
area_plot_of_specific_rule_id <- function(
  D, R, region=levels(D$region), scenario=levels(D$scenario),
  facet_x=NULL, facet_y=NULL, PRINT_OUT=F, DEBUG=T, fontsize=20,
  color_code_specify=T, rule_ids=NULL) {

  p_list1 <- list()

  for (i in rule_ids){

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
          color_mapper <- mipplot::mipplot_default_color_palette
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

        ## Generate plots only if data is available for a given scenario.
        if (nrow(na.omit(D_RHS[D_RHS$scenario == s, ])) > 0) {

          ### FACET DOES NOT WORK IN NO DATA EXISTS.
          p_Out1 <-
            ggplot2::ggplot() +
            ggplot2::geom_area(
              data = na.omit(D_RHS),
              ggplot2::aes(x = .data$period, y = .data$value, fill = .data$variable),
              position = "stack")

          p_Out1 <- p_Out1 +
            ggplot2::geom_line(
              data = na.omit(D_LHS),
              ggplot2::aes(x = .data$period, y = .data$value), size = 2)

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

