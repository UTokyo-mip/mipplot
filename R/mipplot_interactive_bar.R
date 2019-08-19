#' @title A function to launch interactive plot using Shiny
#' @description A function to launch interactive bar plot using
#'              right-hand-side values of  target additivity rule.
#'              The function arguments include the input dataframe,
#'              labels for the plot/axes/legend, and faceting dimensions
#' @param D A quitte format dataframe of IAMC data to produce garph.
#' @param R A table with additivity rules.
#' @examples
#' \donttest{
#' mipplot_interactive_bar(ar5_db_sample_data, ar5_db_sample_rule_table)
#' }
#' @export

mipplot_interactive_bar <- function(D, R) {

  # name_of_input_data_variable is a string such as "ar5_db_sample_data"
  # this variable is used for generating R code to reproduce plot
  name_of_input_data_variable <- as.character(substitute(D))
  name_of_input_rule_table_variable <- as.character(substitute(R))

  # check and correct data format if necessary
  D <- correct_format_of_iamc_dataframe(D)

  region_list <- levels(D$region)
  var_list <- levels(D$variable)
  model_list <- levels(D$model)
  scenario_list <- levels(D$scenario)
  period_list <- levels(as.factor(D$period))

  # get variable-group-name list
  variable_group_name_list <- get_variable_group_name_list(R)

  ui <- fluidPage(

    titlePanel("mipplot"),

    sidebarLayout(
      sidebarPanel(

        selectInput("region", "region:",
                    choices = c("Choose region" = "", region_list)
        ),

        shinyWidgets::pickerInput("variable_group",
                                  label = "variable:",
                                  choices = variable_group_name_list,
                                  multiple = FALSE,
                                  options = list(
                                    `actions-box` = TRUE,
                                    `title` = "Choose variable"
                                  )),


        shinyWidgets::pickerInput("model",
                                  label = "model:",
                                  choices = get_model_name_list(D),
                                  multiple = TRUE,
                                  options = list(
                                    `actions-box` = TRUE,
                                    `title` = "Choose model"
                                  )),

        shinyWidgets::pickerInput("scenario",
                    label = "scenario:",
                    choices = get_scenario_name_list(D),
                    # selected = get_scenario_name_list(D)[1],
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      `title` = "Choose scenario"
                    )),

        shinyWidgets::sliderTextInput(
          inputId = "target_year",
          label = "target_year:",
          choices = period_list,
          # selected = c(head(period_list, n = 1),
          #              tail(period_list, n = 1))
        ),

        shiny::checkboxInput(
          inputId = "printCredit",
          label = "print credit",
          value = TRUE),

        checkboxInput(
          inputId = "aHundredPercentStacked",
          label = "100% stacked",
          value = FALSE),

        # Show container which shows R code
        # to reproduce current plot.
        shiny::div(
          class = "form-group shiny-input-container",
          shiny::tags$label(class="control-label", "code:"),
          shiny::tags$pre(
            style = "overflow: scroll; max-height: 10em; white-space: pre-line;",
            shiny::textOutput(
              "code_to_reproduce_plot", inline = TRUE
            )
          )
        )

      ),




      mainPanel(plotOutput("bar_plot"))

    )
  )

  server <- function(input, output) {

    output$code_to_reproduce_plot <- shiny::reactive({
      generate_code_to_plot_bar(
        input, name_of_input_data_variable,
        name_of_input_rule_table_variable)
    })

    output$bar_plot <- renderPlot({

      # get variable name list in specified variable group
      input_variable_list <- get_variable_name_list_in_variable_group(input$variable_group)

      # We cannot filter the rows of the data
      # with function mipplot_bar() alone.
      # So prior to calling mipplot_bar(),
      # we filter the data.

      data_subset = D %>%
        dplyr::filter(variable %in% input_variable_list) %>%
        dplyr::filter(model %in% input$model) %>%
        dplyr::filter(scenario %in% input$scenario)

      # Generates an image that does not contain a copyright notice.
      plotted_image <- mipplot_bar(
        data_subset, R, region = input$region,
                  target_year = input$target_year,
        one_hundred_percent_stacked = input$aHundredPercentStacked)

      # If specified, a copyright notice will be added to the image.
      if (input$printCredit) {
        plotted_image <- add_credit_to_list_of_plot(plotted_image)
      }

      # print error message if condition is not given.
      validate(
        need(
          length(input$variable_group) > 0 & input$variable_group != "" &
            length(input$region) > 0 & input$region != "" &
            length(input$model) > 0 & input$model != "" &
            length(input$scenario) > 0 & input$scenario != "",
          "please set condition")
      )

      # print error message if no plot is plotted.
      validate(
        need(length(plotted_image) > 0, "can't find any data in this condition")
      )

      # Output the image
      plotted_image
    },
    height = 400, width = 600
    )


  }

  shinyApp(ui, server);
}

#' @title generate code to reproduce bar plot
#' @description This function is called in the mipplot_interactive_bar()
#' and provides R code to reproduce the currently drawn plot.
#' This function cannot be used out of reactive expression in Shiny.
#' @param input This is the same as the input argument in the shiny:ui().
#' @param name_of_input_data_variable A string such as "ar5_sample_data".
#' @param name_of_input_rule_table_variable A string such as "ar5_sample_rule".
generate_code_to_plot_bar <- function(
  input,
  name_of_input_data_variable,
  name_of_input_rule_table_variable) {

  # get variable name list in specified variable group
  input_variable_list <- get_variable_name_list_in_variable_group(input$variable_group)

    return(stringr::str_interp(
      "data_subset <- ${name_of_input_data_variable} %>%
  filter(variable %in% ${get_string_expression_of_vector_of_strings(input_variable_list)}) %>%
  filter(model %in% ${get_string_expression_of_vector_of_strings(input$model)}) %>%
  filter(scenario %in% ${get_string_expression_of_vector_of_strings(input$scenario)})

mipplot_bar(data_subset, ${name_of_input_rule_table_variable},
  region = ${get_string_expression_of_vector_of_strings(input$region)},
  target_year = ${input$target_year},
      one_hundred_percent_stacked = ${input$aHundredPercentStacked})"
    ))
  }
