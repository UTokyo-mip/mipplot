

mipplot_interactive_plot_line <- function(D,R){
    library(ggplot2)
    library(shiny)

	var_list <- levels(D$variable)
    region_list <- levels(D$region)
	ui = fluidPage(
	         selectInput("variable", "Choose a variable:",
	                     list(`variable` = var_list)),
	         selectInput("region", "Choose a region:",
	                     list(`region` = region_list)),
	         textOutput("result"),
	         mainPanel(plotOutput("plot2"))
	     );

	server = function(input, output) {
	    #output$result <- renderText({
	    #  paste("variable: ", input$variable)
	    #})
	    output$plot2<-renderPlot(
	         {
	           mipplot_line(D%>%filter(variable==input$variable & region==input$region))[[1]]
	           # ggplot2::ggplot(data.frame(x=c(1,2),y=c(3,4)),ggplot2::aes(x=x,y=y)) + ggtitle(input$result)
	         },
	         height = 400,width = 600)
	  }

	shinyApp(ui,server);
}
