#' @title Interactive line plot from IAMC data using Shiny
#' @description A function to launch interactive plot using Shiny
#' @param D A quitte format dataframe of IAMC data to produce plot.
#' @param R A table with additivity rules.
#' @example mipplot_interactive_plot_line(mipplot::ar5_db_sample09_Wang, mipplot::ar5_db_rule_table_v09_Wang)
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
