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

  library(shiny)

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
                    list(`region` = region_list)
                    ),

        selectInput("variable", "variable:",
                    list(`variable` = var_list)
                    ),

        checkboxGroupInput("model", "model:",
                           choiceNames = get_model_name_list(D),
                           choiceValues = get_model_name_list(D),
                           # the default model is a first appeared model in D
                           selected = get_model_name_list(D)[1]
                           ),

        checkboxGroupInput("scenario", "scenario:",
                           choiceNames = get_scenario_name_list(D),
                           choiceValues = get_scenario_name_list(D),
                           # the default scenario is a first appeared scenario in D
                           selected = get_scenario_name_list(D)[1]
                           ),

        selectInput("period_start", "period_start:",
                    list(`period` = period_list),
                    selected = 2005
                    ),

        selectInput("period_end", "period_end:",
                    list(`period` = period_list),
                    selected = 2050
                    )
        ),


      mainPanel(plotOutput("line_plot"))

    )
  )

  server <- function(input, output) {

    output$line_plot <- renderPlot({
      mipplot_line(D,variable = input$variable, #model = input$model,
                        scenario = input$scenario
                        #period = seq(input$period_start,input$period_end,5)
                   )
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
#' @dontrun{
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
#' @dontrun{
#' get_scenario_name_list(ar5_db_sample_data)
#' }
get_scenario_name_list <- function(D) {
  return (D %>% dplyr::pull(scenario) %>% unique() %>% levels())
}
