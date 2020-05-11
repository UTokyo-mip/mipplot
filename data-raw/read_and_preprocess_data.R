# Run this script in `mipplot/data-raw` directory.

ar5_db_sample_data <- mipplot_read_iamc("ar5_db_sample_data.csv")
usethis::use_data(ar5_db_sample_data, overwrite = TRUE)

ar5_db_sample_rule_table <- mipplot_read_ruletab("ar5_db_sample_rule_table.csv")
usethis::use_data(ar5_db_sample_rule_table, overwrite = TRUE)

sr15_sample_data <- mipplot_read_iamc("iamc15_sample_data.csv")
usethis::use_data(sr15_sample_data, overwrite = TRUE)

# Generate default color pallete
mipplot_default_color_palette <-
  mipplot_generate_color_mapper(ar5_db_sample_rule_table)
usethis::use_data(mipplot_default_color_palette, overwrite = TRUE)
