
#--------------------------------------------------------------------
# PLOTTING FUNCTION: LINE
#--------------------------------------------------------------------

#' @title A function to plot line graph
#' @description Line plots
#' @param D A dataframe of IAMC data to produce garph.
#' @return A list of graph
#' @example mipplot_line(AR5_Sample_data)
#' @export p_list1


mipplot_line <- function(D,region=levels(D$region),variable=levels(D$variable),
                              scenario=levels(D$scenario),PRINT_OUT=F,DEBUG=T){


  p_list1 <- list()

  for(r in levels(as.factor(region))){
    for (v in levels(as.factor(variable))){

        #FILTERING THE DATAFRAME (D) TO MULTIPLE scenarioS BASED ON "levels" OF A FACTOR WAS NOT WORKING AS EXPECTED!
        #FILTERING TO A LIST OF scenario USING "%in%" FIXED THE PROBLEM.
        D_sub <- D[D$region==r & D$variable==v & D$scenario %in% scenario, ]
        D_sub <- D_sub[!is.na(D_sub$value),]  ## removing NA ensures lines are connected

        if(nrow(D_sub)==0){next()}    # Skip iteration if data is empty for the selected scope (scenario/region/variable).

        ## Title
        tt1 <- paste("region:",r, sep="")
        tt2 <- paste("variable:",as.character(v), sep="")
        tt3 <- paste(' [',D_sub$unit[1],']', sep="")

        ## Line plots: using values name
        p_Out1 <- ggplot2::ggplot(data=D_sub, ggplot2::aes(x=period, y=value))+
          ggplot2::geom_line(ggplot2::aes(color=scenario,linetype=model))+
          ggplot2::geom_point(ggplot2::aes(color=scenario,shape=model),size=2)+
          ggplot2::labs(title=tt1, subtitle=tt2, y=tt3)

          #theme(legend.position = "bottom") +
          #axis.text.x = element_text(angle = 90))

        p_Out1 <- p_Out1 + ggplot2::theme(text=ggplot2::element_text(size = 20))

        ## STORE PLOTS TO LIST
        p_list1[[length(p_list1)+1]] <- p_Out1

    }
  }

  if (PRINT_OUT == TRUE){
    ## Open printing device.
    filename <-sprintf("../data_output/JpMIP_plots_line_%s.pdf", format(Sys.time(), "%Y_%m%d"))
    pdf(filename, onefile = TRUE, width = 11.69, height = 8.27)

    ## Plot for each variable set.
    for(p in p_list1){
      plot(p)
    }
    dev.off()
  }

  return(p_list1)

}


