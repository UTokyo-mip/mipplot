#' @title A function to launch interactive plot using Shiny
#' @description A function to launch interactive plot using Shiny
#' @param D A quitte format dataframe of IAMC data to produce garph.
#' @param R A table with additivity rules.
#' @example mipplot_interactive_plot_line(mipplot::ar5_db_sample09_Wang, mipplot::ar5_db_rule_table_v09_Wang)
#' @export  interactive polt window


mipplot_interactive_line <- function(D) {

  library(shiny)

  var_list <- levels(D$variable)
  model_list <- levels(D$model)
  scenario_list <- levels(D$scenario)
  period_list <- levels(as.factor(D$period))


  ui <- fluidPage(

    titlePanel("mipplot"),

    sidebarLayout(
      sidebarPanel(

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

        selectInput("period_start", "period_start:",
                    list(`period` = period_list),
                    selected = 2005
        ),

        selectInput("period_end", "period_end:",
                    list(`period` = period_list),
                    selected = 2050)
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
