#' @title A function to launch interactive plot using Shiny
#' @description A function to launch interactive plot for additivity check.
#' @param D A quitte format dataframe of IAMC data to produce graph.
#' @param R A table with additivity rules.
#' @param debug Set TRUE if table view is required.
#' @return No return value, called for side effects
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel mainPanel plotOutput renderPlot validate need shinyApp
#' @examples
#' \dontrun{
#' mipplot_interactive_additivity_check_bar(ar5_db_sample_data, ar5_db_sample_rule_table)
#' }
#' @export

mipplot_interactive_additivity_check_bar <- function(D, R, debug=FALSE) {

  # name_of_input_data_variable is a string such as "ar5_db_sample_data"
  # this variable is used for generating R code to reproduce plot
  name_of_input_data_variable <- as.character(substitute(D))
  name_of_input_rule_table_variable <- as.character(substitute(R))

  # check and correct data format if necessary
  D <- correct_format_of_iamc_dataframe(D)

  scenario_list <- levels(D$scenario)
  rule_id_list <- unique(R$Rule_ID)

  ui <- fluidPage(

    titlePanel("mipplot"),

    sidebarLayout(
      sidebarPanel(

        shinyWidgets::pickerInput("rule_id",
                                  label = "rule ID:",
                                  choices = rule_id_list,
                                  selected = rule_id_list[1],
                                  multiple = FALSE,
                                  options = list(
                                    `actions-box` = TRUE,
                                    `title` = "Choose rule ID"
                                  )),

        shinyWidgets::pickerInput("scenario",
                    label = "scenario:",
                    choices = get_scenario_name_list(D),
                    selected = scenario_list[1],
                    multiple = FALSE,
                    options = list(
                      `actions-box` = TRUE,
                      `title` = "Choose scenario"
                    )),

        shiny::checkboxInput(
          inputId = "printCredit",
          label = "print credit",
          value = TRUE),

        # # To disable automatic re-draw,
        # # we only have to includes submitButton.
        # shiny::div(
        #   class = "form-group shiny-input-container",
        #   submitButton(text = "Apply Changes", icon = NULL, width = NULL)
        # ),
      ),

      mainPanel(plotOutput("bar_plot"))

    )
  )

  server <- function(input, output) {

    output$bar_plot <- renderPlot({

      # print error message if condition is not given.
      validate(
        need(
          length(input$scenario) > 0 && input$scenario != "",
          length(input$rule_id) > 0 && input$rule_id != "",
          "Please specify plotting options.")
      )

      # Generates an image that does not contain a copyright notice.
      plotted_image <- mipplot_additivity_check_bar(
        D, R,
        target_scenarios = input$scenario,
        target_rule_ids = input$rule_id,
        debug=debug)

      # If specified, a copyright notice will be added to the image.
      if (input$printCredit) {
        plotted_image <- add_credit_to_list_of_plot(plotted_image)
      }

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
