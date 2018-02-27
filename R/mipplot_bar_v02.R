#--------------------------------------------------------------------
# PLOTTING FUNCTION: BAR
#--------------------------------------------------------------------
#====================================================================
# Bar plots using right-hand-side values of target additivity rule.
#     The function arguments inlcude the input dataframe, labels
#     for the plot/axes/legend, and faceting dimensions.
#====================================================================

#' @title A function to plot bar graph
#' @description Bar plots using right-hand-side values of target additivity rule.
#' @param D A dataframe of IAMC data to produce garph.
#' @param R A dataframe of data aggregation rules (meta data).
#' @return A list of graph
#' @example mipplot_bar(AR5_Sample_data, AR5_Rule_table)
#' @export p_list1


mipplot_bar <- function(D,R,region=levels(D$region),
                              target_year=levels(as.factor(D$period)),PRINT_OUT=F,DEBUG=T,fontsize=20){
  #REPLACED THIS FUNCTION WITH 1-LINE CODE (SEE LINE 52).
  # wrap_text <- function(x, width=60){
  #   sapply(x,function(x) {paste(strwrap(x,width),collapse="\n")})
  # }

  p_list1 <- list()

  for (i in levels(as.factor(R$Rule_ID))){

    for(r in levels(as.factor(region))){

      for(ty in levels(as.factor(target_year))){

        Var_set <- R[R$Rule_ID==i,]

        ## SELECT DATA
        D_LHS <- D[D$region==r & D$period==ty & D$variable %in% Var_set$Left_side, ]
        D_RHS <- D[D$region==r & D$period==ty & D$variable %in% Var_set$Right_side, ]


        #Renaming levels of a factor
        #http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/

        # Common Part of Var-name
        var_common_name <- Var_set[1,2]

        # # Remove the Common Part
        # D_RHS$variable <- mapvalues(D_RHS$variable, levels(as.factor(D_RHS$variable)),
        #                             wrap_text(
        #                               stri_replace_all_fixed(levels(as.factor(D_RHS$variable)),var_common_name,""),
        #                               width=20))


        #Title
        tt1 <- paste("region:",r,",  period:",ty, sep="")
        tt2 <- paste("variable:",as.character(Var_set[1,2]), sep="")
        tt3 <- paste(' [',D_RHS$unit[1],']', sep="")

        # Change name of variable by removing common part from aggregated vairable (LHS).
        D_RHS$variable <- gsub(paste(var_common_name,"|",sep=""),"",D_RHS$variable, fixed=T)

        # Only generate plots if data is available for a region.
        if(nrow(na.omit(D_RHS[D_RHS$region==r,]))){
          ## Line plots: using left-hand-side values of target rule.
          p_Out1 <- ggplot2::ggplot(na.omit(D_RHS), ggplot2::aes(x=scenario, y=value, fill=variable))+
            ggplot2::geom_bar(stat = "identity") +
            facet_wrap(~model)+
            ggplot2::labs(title=tt1, subtitle=tt2, y=tt3)+
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
            #ggplot2::theme(legend.position = "bottom")

          p_Out1 <- p_Out1 + ggplot2::theme(text=ggplot2::element_text(size = fontsize))

          # STORE PLOTS TO LIST
          p_list1[[length(p_list1)+1]] <- p_Out1
        }
      }
    }
  }


  if (PRINT_OUT == TRUE){
   ## Open printing device.
   filename <-sprintf("../data_output/JpMIP_plots_bar_%s.pdf", format(Sys.time(), "%Y_%m%d"))
   pdf(filename, onefile = TRUE, width = 11.69, height = 8.27)

   ## Plot for each variable set.
   for(p in p_list1){
    plot(p)
   }
   dev.off()
  }


   return(p_list1)

}




