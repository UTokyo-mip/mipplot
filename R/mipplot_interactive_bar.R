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

mipplot_interactive_bar <- function(D,R) {

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
                           choices =
                             list("AIM-Enduse 12.1" = "AIM-Enduse 12.1",
                                  "GCAM 3.0" = "GCAM 3.0",
                                  "IMAGE 2.4" = "IMAGE 2.4"
                             ),
                           selected = "AIM-Enduse 12.1"
                           ),


        checkboxGroupInput("scenario", "scenario:",
                           choices =
                             list("EMF27-450-Conv" = "EMF27-450-Conv",
                                  "EMF27-450-FullTech" = "EMF27-450-FullTech",
                                  "EMF27-450-NoCCS" = "EMF27-450-NoCCS",
                                  "EMF27-450-NucOff" = "EMF27-450-NucOff",
                                  "EMF27-Base-Conv" = "EMF27-Base-Conv",
                                  "EMF27-Base-FullTech" = "EMF27-Base-FullTech",
                                  "EMF27-Base-LimBio" = "EMF27-Base-LimBio",
                                  "EMF27-Base-NucOff" = "EMF27-Base-NucOff"
                             ),
                           selected = "EMF27-450-Conv"
                           ),

        selectInput("target_year", "target_year:",
                    list(`period` = period_list),
                    selected = 2005
                    )
        ),


      mainPanel(plotOutput("bar_plot"))

    )
  )

  server <- function(input, output) {

    output$bar_plot <- renderPlot({
      mipplot_bar(D,R,region = input$region, variable = input$variable,
                  model = input$model,
                  scenario = input$scenario,
                  target_year = input$target_year
                  )
    },
    height = 400, width = 600
    )


  }



  shinyApp(ui, server);
}
