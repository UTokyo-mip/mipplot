
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
#                         xby="period", yby="value", groupby=NULL,
                              colorby="scenario", linetypeby="model", shapeby="model",
                              scenario=levels(D$scenario),facet_x=NULL, facet_y=NULL, PRINT_OUT=F,DEBUG=T){

  p_list1 <- list()

  D <- quitte::as.quitte(D)  # Convert to quitte format (PIK package dataframe).
 
  for(r in levels(as.factor(region))){
    for (v in levels(as.factor(variable))){

        D_sub <- D[D$region==r & D$variable==v & D$scenario %in% scenario, ]
        D_sub <- D_sub[!is.na(D_sub$value),]  ## removing NA ensures lines are connected

        if(nrow(D_sub)==0){next()}    # Skip iteration if data is empty for the selected scope (scenario/region/variable).

        ## Title
        tt1 <- paste("region:",r, sep="")
        tt2 <- paste("variable:",as.character(v), sep="")
        tt3 <- paste(' [',D_sub$unit[1],']', sep="")

        #TESTS WITH EVAL(PARSE) BUT GAVE ERROR (UNEXPECTED ",")!
        #CHANGING "aes" TO "aes_" AND USING "as.name" TO REFER TO ARGUMENTS WERE SUCCESSFUL!
        
        #eval_arg2 <- sprintf("(color=%s, linetype=%s)", colorby, linetypeby) 
        #eval_arg2a <- sprintf("color=%s", colorby) 
        #eval_arg2b <- sprintf("linetype=%s", linetypeby) 
        
        ## Line plots: using values name
        p_Out1 <- ggplot2::ggplot(data=D_sub, ggplot2::aes(x=period, y=value))
        #ggplot2::geom_line(ggplot2::aes(color=scenario,linetype=model))
        #ggplot2::geom_line(ggplot2::aes(eval(parse(text=eval_arg2a)), eval(parse(text=eval_arg2b))))
        
#***TRIED TO ALLOW FOR "PERIOD" AS NON-X-VALUE AND AS COLOR/LINETYPE ARGUMENT:
        #***NEED TO GROUP DATA BY INTERACTION AND TRANSFORM PERIOD DATA INTO FACTOR.
        #***THIS CODE WORKS!
        #ggplot2::ggplot(D2,ggplot2::aes(scenario,value, group=interaction(period, model)))+ggplot2::geom_line(ggplot2::aes(color=as.factor(period), linetype=model))
        
        #***BUT THE FUNCTION WITH EXPLICIT ARGUMENTS DOES NOT WORK!
        #mipplot_line(D2, xby="scenario", colorby = "period", linetypeby = "model")
        # Error in unique.default(x, nmax = nmax) : 
        #   unique() applies only to vectors 
        
        # if(colorby=="period" | linetypeby=="period"){
        #   D_sub$period <- as.factor(D_sub$period)
        #   #groupby=paste(colorby,linetypeby, sep=",")
        #   #p_Out1 <- ggplot2::ggplot(data=D_sub, ggplot2::aes_(x=as.name(xby), y=as.name(yby),group=(interaction(as.name(colorby),as.name(linetypeby)))))
        #   p_Out1 <- ggplot2::ggplot(data=D_sub, ggplot2::aes_(x=as.name(xby), y=as.name(yby),group=(interaction(as.name(groupby)))))
        # } else {
        #   p_Out1 <- ggplot2::ggplot(data=D_sub, ggplot2::aes_(x=as.name(xby), y=as.name(yby)))
        # }
        p_Out1 <- p_Out1 + 
          ggplot2::geom_line(ggplot2::aes_(color=as.name(colorby),linetype=as.name(linetypeby)))+
          #ggplot2::geom_point(ggplot2::aes(color=scenario,shape=model),size=2)+
          ggplot2::geom_point(ggplot2::aes_(color=as.name(colorby),shape=as.name(shapeby)),size=2)+
          ggplot2::labs(title=tt1, subtitle=tt2, y=tt3)
        
          #ggplot2::theme(legend.position = "bottom") +
          #axis.text.x = ggplot2::element_text(angle = 90))

        ## Facet plots if horizontal and/or vertical dimension provided.
        if(!is.null(facet_x) & !is.null(facet_y)){
          facet_by <- paste(facet_y, facet_x, sep="~")
          p_Out1 <- p_Out1 + ggplot2::facet_grid(facet_by)
        }else if(!is.null(facet_x) & is.null(facet_y)){
          facet_by <- paste(".", facet_x, sep="~")
          p_Out1 <- p_Out1 + ggplot2::facet_grid(facet_by)
        }else if(is.null(facet_x) & !is.null(facet_y)){
          facet_by <- paste(facet_y, ".", sep="~")
          p_Out1 <- p_Out1 + ggplot2::facet_grid(facet_by)
        }
        
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


