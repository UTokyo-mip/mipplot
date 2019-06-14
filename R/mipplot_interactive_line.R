#' @title A function to launch interactive plot using Shiny
#' @description A function to launch interactive line plot.
#'              The function arguments include the input dataframe,
#'              labels for the plot/axes/legend, and faceting dimensions
#' @param D A quitte format dataframe of IAMC data to produce garph.
#' @examples
#' \donttest{
#' mipplot_interactive_line(ar5_db_sample_data)
#' }
#' @export

mipplot_interactive_line <- function(D) {

  region_list <- levels(D$region)
  var_list <- levels(D$variable)
  model_list <- levels(D$model)
  scenario_list <- levels(D$scenario)
  period_list <- levels(as.factor(D$period))


  ui <- fluidPage(

    titlePanel("mipplot"),

    sidebarLayout(
      sidebarPanel(

        selectInput("region", "region:",
                    choices = region_list
                    ),

        selectInput("variable", "variable:",
                    list(`variable` = var_list)
                    ),

        shinyWidgets::pickerInput("model",
                    label = "model:",
                    choices = get_model_name_list(D),
                    selected = get_model_name_list(D)[1],
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE
                    )),

        shinyWidgets::pickerInput("scenario",
                    label = "scenario:",
                    choices = get_scenario_name_list(D),
                    selected = get_scenario_name_list(D)[1],
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE
                    )),

        # TODO:
        # previous user interfaces are commented out.
        # if new experimental user interfaces are employed,
        # please delete these comments.

        # selectInput("period_start", "period_start:",
        #             list(`period` = period_list),
        #             selected = 2005
        #             ),

        # selectInput("period_end", "period_end:",
        #             list(`period` = period_list),
        #             selected = 2050
        #             ),

        shinyWidgets::sliderTextInput(
          inputId = "period",
          label = "period:",
          choices = period_list,
          selected = c(head(period_list, n = 1),
                       tail(period_list, n = 1))
        ),

        # To disable automatic re-draw,
        # we only have to includes submitButton.
        submitButton(text = "Apply Changes", icon = NULL, width = NULL)
        ),


      mainPanel(plotOutput("line_plot"))

    )
  )

  server <- function(input, output) {

    output$line_plot <- renderPlot({

      code <- stringr::str_interp("
      D_subset = D %>%
        dplyr::filter( model %in% ${get_string_expression_of_vector_of_strings(input$model)} ) %>%
        dplyr::filter(${input$period[1]} <= period) %>%
        dplyr::filter(period <= ${input$period[2]}) %>%
      mipplot_line(  #D_subset,
                   variable = ${get_string_expression_of_vector_of_strings(input$variable)},
                   scenario = ${get_string_expression_of_vector_of_strings(input$scenario)},
                   region = ${get_string_expression_of_vector_of_strings(input$region)})
      ")

      cat(code)

      # Since mipplot_line() function has no arguments to filter
      # models and periods to be plotted,
      # manually narrow the records in D
      # before entering the mipplot_line() function.
      D_subset = D %>%

        # filter model
        dplyr::filter( model %in% input$model ) %>%

        # filter period
        dplyr::filter(input$period[1] <= period) %>%
        dplyr::filter(period <= input$period[2])
        # dplyr::filter(input$period_start <= period) %>%
        # dplyr::filter(period <= input$period_end)

      # plot D_subset instead of D.
      mipplot_line(D_subset,
                   variable = input$variable,
                   scenario = input$scenario,
                   region = input$region)
    },
    height = 400, width = 600
    )


  }



  shinyApp(ui, server);
}

#' @title Get name list of models in IAMC formatted data frame
#' @description select name of models from the column "model" then make unique it.
#' output is character vector such as,
#' c("AIM-Enduse 12.1", "GCAM 3.0", "IMAGE 2.4" )
#' @param D A quitte format dataframe of IAMC data to produce garph.
#' @examples
#' \donttest{
#' get_model_name_list(ar5_db_sample_data)
#' }
get_model_name_list <- function(D) {
  return (D %>% dplyr::pull(model) %>% unique() %>% levels())
}

#' @title Get name list of scenarios in IAMC formatted data frame
#' @description select name of scenarios from the column "scenario" then make unique it.
#' output is character vector such as,
#' c("EMF27-450-Conv", "EMF27-450-FullTech", "EMF27-450-NoCCS", "EMF27-450-NucOff")
#' @param D A quitte format dataframe of IAMC data to produce garph.
#' @examples
#' \donttest{
#' get_scenario_name_list(ar5_db_sample_data)
#' }
get_scenario_name_list <- function(D) {
  return (D %>% dplyr::pull(scenario) %>% unique() %>% levels())
}

#' @title Get expression of vector of string in string format
#' @description To evaluate expression, get string of expression
#' @param vector_of_strings vector of strings, such as c("A", "B")
#' @examples
#' \donttest{
#' noquote(
#'   get_string_expression_of_vector_of_strings(c("A", "B"))
#' )
#' }
get_string_expression_of_vector_of_strings <- function(vector_of_strings) {
  return (paste("c(\"", stringr::str_c(vector_of_strings, collapse = "\", \""), "\")", sep=""))
}
