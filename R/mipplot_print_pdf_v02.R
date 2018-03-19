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

    DATA_OUTPUT_DIR = "./data_output"
    create_dir_with_user_permission(DATA_OUTPUT_DIR)

    filename <-sprintf(file.path(DATA_OUTPUT_DIR, paste("MIP_plots_",filelabel,"_%s.pdf",sep="")), format(Sys.time(), "%Y_%m%d"))

    tryCatch(

      expr = {
        pdf(filename, onefile = TRUE, width = 11.69, height = 8.27)
        #plot(p_list1)
        #p_list1
        # Plot for each variable set.
        for(p in p_list1){
          plot(p)
        }
      },

      error = function(e) {
        stop(sprintf("in mipplot_print_pdf, following error happend. \n%s", e))
      },

      finally = {
        dev.off()
      }
    )
}

create_dir_with_user_permission <- function(dir_path) {

    # If the output folder does not exist, create it.
    if (!file.exists(dir_path)) {

      # when R is on interactive mode, the prompt is appeared.
      if (interactive()) {

        if (menu(c("Yes", "No"),
                  title=sprintf(
                    "The output directiory doesn't exist. Do you want to create folder to %s ?",
                    dir_path)) != 2) {

          stop(sprintf("You need to create output folder to %s.", dir_path))
        }
      }
      dir.create(dir_path)
    }
}
