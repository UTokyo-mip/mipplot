#--------------------------------------------------------------------
# Print list of plots to pdf file.
#--------------------------------------------------------------------

#' @title Print list of plots to pdf file
#' @description This function plots a ggplot plots to PDF file.
#' @param p_list1 A list of ggplot plot.
#' @param filelabel A string of prefix of output filename.
#' @param filename A string of filename. If it is given,
#' filelabel is ignored.
#' @return No return value, called for side effects
#' @examples
#' \donttest{
#' p <- mipplot_area(ar5_db_sample_data, ar5_db_sample_rule_table,
#'              region = "World", scenario = "EMF27-450-FullTech")
#' mipplot_print_pdf(p)
#' }
#' @importFrom  grDevices dev.off pdf
#' @export

mipplot_print_pdf <- function(p_list1, filelabel = "", filename=tryCatch(file.choose(new = TRUE), error=function(e) {NA})){

    if (is.na(filename)) {
      # If file is not specified,
      # output filename is automatically determined.
      DATA_OUTPUT_DIR <- "../data_output"
      create_dir_with_user_permission(DATA_OUTPUT_DIR)

      filename <- sprintf(
        file.path(DATA_OUTPUT_DIR,
                  paste("MIP_plots_", filelabel, "_%s.pdf", sep = "")),
        format(Sys.time(), "%Y_%m%d"))
    }

    tryCatch(

      expr = {

        # Enable showtext package
        showtext::showtext_auto()

        pdf(filename, onefile = TRUE, width = 11.69, height = 8.27)

        for (p in p_list1) {

          plot(p)

        }

      },

      error = function(e) {

        stop(sprintf("in mipplot_print_pdf, following error happend. \n%s", e))

      },

      finally = {

        # Turn off showtext package
        showtext::showtext_auto(FALSE)

        dev.off()

      }
    )
}

create_dir_with_user_permission <- function(dir_path) {

    # If the output folder does not exist, create it.
    if (!file.exists(dir_path)) {

      # when R is on interactive mode, the prompt is appeared.
      if (interactive()) {

        if (
          utils::menu(c("Yes", "No"),
            title = sprintf(
              "The output directiory doesn't exist. Do you want to create folder to %s ?",
              dir_path)) != 1) {

          stop(sprintf("You need to create output folder to %s.", dir_path))

        }
      }
      dir.create(dir_path)
    }
}
