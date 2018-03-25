# Run this script in `mipplot/data-raw` directory.

ar5_db_sample09_Wang <- mipplot_readquitte("ar5_db_sample09_Wang.csv")
devtools::use_data(ar5_db_sample09_Wang, overwrite = TRUE)

ar5_db_rule_table_v09_Wang <- read.csv("ar5_db_rule_table_v09_Wang.csv")
devtools::use_data(ar5_db_rule_table_v09_Wang, overwrite = TRUE)
