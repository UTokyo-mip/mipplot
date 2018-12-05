#' @title A function to launch interactive plot using Shiny
#' @description A function to launch interactive plot using Shiny
#' @param D A quitte format dataframe of IAMC data to produce garph.
#' @param R A table with additivity rules.
#' @examples
#' \dontrun{
#' mipplot_interactive_plot_line(ar5_db_sample_data, ar5_db_sample_rule_table)
#' }
#' @export

mipplot_interactive_plot_line <- function(D, R) {

  library(ggplot2)
  library(shiny)

  var_list <- levels(D$variable)
  region_list <- levels(D$region)

  ui <- fluidPage(

    selectInput("variable", "Choose a variable:",
                list(`variable` = var_list)),

    selectInput("region", "Choose a region:",
                list(`region` = region_list)),

    textOutput("result"),

    mainPanel(
      plotOutput("plot2")));


  server <- function(input, output) {

    output$plot2 <- renderPlot({

        mipplot_line(
          D %>% dplyr::filter(
              variable == input$variable & region == input$region))
      },

      height = 400, width = 600)
    }

  shinyApp(ui, server);
}
