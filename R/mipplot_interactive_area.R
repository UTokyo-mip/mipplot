#' @title A function to launch interactive plotting session on Shiny
#' @description Provides gui to set plotting parameter for area plot.
#' @param D A dataframe of IAMC data in tibble format to produce area plots.
#' @param R A dataframe of data aggregation rules (meta data).
#' @param language A string of language for initial plot.
#' Possible values are "en", "jp",
#' "es", "zh-cn", "zh-tw". The default value is "en".
#' @return No return value, called for side effects
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel selectInput checkboxInput submitButton div tags textOutput mainPanel plotOutput reactive renderPlot validate need shinyApp
#' @importFrom utils head tail
#' @examples
#' \donttest{
#' if (interactive()) {
#' mipplot_interactive_area(ar5_db_sample_data, ar5_db_sample_rule_table)
#' }
#' }
#' @export

mipplot_interactive_area <- function(D, R, language = "en") {

  variable <- model <- period <- NULL


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

        selectInput("scenario", "scenario:",
                    choices = c("Choose scenario" = "", scenario_list)
        ),

        shinyWidgets::sliderTextInput(
          inputId = "period",
          label = "period:",
          choices = period_list,
          selected = c(head(period_list, n = 1),
                      tail(period_list, n = 1))
        ),

        checkboxInput(
          inputId = "printCredit",
          label = "print credit",
          value = TRUE),

        checkboxInput(
          inputId = "aHundredPercentStacked",
          label = "100% stacked",
          value = FALSE),

        checkboxInput(
          inputId = "rotateYearLabel45Degrees",
          label = "roate year label 45 degrees",
          value = FALSE),

        selectInput("language", "language:",
                    choices = c(
                      "Chinese(Simplified)" = "zh-cn",
                      "Chinese(Traditional)" = "zh-tw",
                      "English" = "en",
                      "Japanese" = "jp",
                      "Spanish" = "es"),
                    selected = language),


        # To disable automatic re-draw,
        # we only have to includes submitButton.
        shiny::div(
          class = "form-group shiny-input-container",
          submitButton(text = "Apply Changes", icon = NULL, width = NULL)
        ),

        # Show container which shows R code
        # to reproduce current plot.
        div(
          class = "form-group shiny-input-container",
          tags$label(class="control-label", "code:"),
          tags$pre(
            style = "overflow: scroll; max-height: 10em; white-space: pre-line;",
            textOutput(
              "code_to_reproduce_plot", inline = TRUE
            )
          )
        )#,
      ),

      mainPanel(plotOutput("area_plot"))

    )
  )

  server <- function(input, output) {

    output$code_to_reproduce_plot <- reactive({
      generate_code_to_plot_area(
        input, name_of_input_data_variable,
        name_of_input_rule_table_variable)
    })

    output$area_plot <- renderPlot({

      # print error message if condition is not given.
      validate(
        need(
          length(input$variable_group) > 0 && input$variable_group != "" &&
            length(input$region) > 0 && input$region != "" &&
            length(input$model) > 0 && input$model != "" &&
            length(input$scenario) > 0 && input$scenario != "",
          "Please specify plotting options.")
      )

      # get variable name list in specified variable group
      input_variable_list <- get_variable_name_list_in_variable_group(input$variable_group)

      # We cannot filter the rows of the data
      # with function mipplot_bar() alone.
      # So prior to calling mipplot_bar(),
      # we filter the data.
      data_subset = D %>%
        dplyr::filter(variable %in% input_variable_list) %>%
        dplyr::filter(model %in% input$model) %>%
        # filter period
        dplyr::filter(input$period[1] <= period) %>%
        dplyr::filter(period <= input$period[2])

      # Generates an image that does not contain a copyright notice.
      plotted_image <- mipplot_area(data_subset, R,
                                    region = input$region,
                                    scenario = input$scenario,
                                    one_hundred_percent_stacked = input$aHundredPercentStacked,
                                    axis_year_text_angle = ifelse(input$rotateYearLabel45Degrees, 45, 0),
                                    language = input$language)

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
    height = 400, width = 600)
  }
  shinyApp(ui, server)
}


#' @title generate code to reproduce area plot
#' @description This function is called in the mipplot_interactive_area()
#' and provides R code to reproduce the currently drawn plot.
#' This function cannot be used out of reactive expression in Shiny.
#' @param input This is the same as the input argument in the shiny:ui().
#' @param name_of_input_data_variable A string such as "ar5_sample_data".
#' @param name_of_input_rule_table_variable A string such as "ar5_sample_rule".
#' @return A string representing the R code for rerun.
generate_code_to_plot_area <- function(
  input,
  name_of_input_data_variable,
  name_of_input_rule_table_variable) {

  # get variable name list in specified variable group
  input_variable_list <- get_variable_name_list_in_variable_group(input$variable_group)

  return(stringr::str_interp(
    "data_subset <- ${name_of_input_data_variable} %>%
  filter(variable %in% ${get_string_expression_of_vector_of_strings(input_variable_list)}) %>%
  filter(model %in% ${get_string_expression_of_vector_of_strings(input$model)}) %>%
  filter(${input$period[1]} <= period) %>%
  filter(period <= ${input$period[2]})


mipplot_area(data_subset, ${name_of_input_rule_table_variable},
  region = ${get_string_expression_of_vector_of_strings(input$region)},
  scenario = ${get_string_expression_of_vector_of_strings(input$scenario)},
  one_hundred_percent_stacked = ${input$aHundredPercentStacked},
  axis_year_text_angle = ${ifelse(input$rotateYearLabel45Degrees, 45, 0)},
  language = '${input$language}')"))
}

