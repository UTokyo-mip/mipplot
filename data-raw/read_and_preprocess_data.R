# Run this script in `mipplot/data-raw` directory.

ar5_db_sample_data <- mipplot_read_iamc("ar5_db_sample_data.csv")
devtools::use_data(ar5_db_sample_data, overwrite = TRUE)

ar5_db_sample_rule_table <- mipplot_read_ruletab("ar5_db_sample_rule_table.csv")
devtools::use_data(ar5_db_sample_rule_table, overwrite = TRUE)

# Generate default color pallete
mipplot_default_color_palette <-
  mipplot_generate_color_mapper(ar5_db_sample_rule_table)
devtools::use_data(mipplot_default_color_palette, overwrite = TRUE)

s <- read.table("data-raw/translation_header.csv", fileEncoding = "utf8", sep=",")
library(shiny.i18n)
