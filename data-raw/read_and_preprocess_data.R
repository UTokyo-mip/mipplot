# Run this script in `mipplot/data-raw` directory.

ar5_db_sample_data <- mipplot_readquitte("ar5_db_sample09_EMF_v01.csv")
devtools::use_data(ar5_db_sample_data, overwrite = TRUE)

ar5_db_rule_table <- read.csv("ar5_db_rule_table_v09.csv")
devtools::use_data(ar5_db_rule_table, overwrite = TRUE)

ar5_db_rule_table_v09_wo_id <- read.csv(
  "ar5_db_rule_table_v09-wo_id.csv", header = TRUE)
devtools::use_data(ar5_db_rule_table_v09_wo_id, overwrite = TRUE)
