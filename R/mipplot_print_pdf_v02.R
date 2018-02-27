#--------------------------------------------------------------------
# PLOTTING FUNCTION: AREA.
#--------------------------------------------------------------------
#====================================================================
# Print list of plots to pdf file.
#====================================================================
mipplot_print_pdf <- function(p_list1, filelabel=""){
    #filelabel<-""
    ## Open printing device.
    #filename <-sprintf("../data_output/MIP_plots_area_%s.pdf", format(Sys.time(), "%Y_%m%d"))
    filename <-sprintf(paste("../data_output/MIP_plots_",filelabel,"_%s.pdf",sep=""), format(Sys.time(), "%Y_%m%d"))
    pdf(filename, onefile = TRUE, width = 11.69, height = 8.27)
    #plot(p_list1)
    #p_list1
    # Plot for each variable set.
    for(p in p_list1){
      plot(p)
    }
    dev.off()

}
