context("test-utility.R")

test_that("get group-name-list", {

  group_name_list <- get_variable_group_name_list(ar5_db_sample_rule_table)
  print(group_name_list)
  testthat::expect_equal(group_name_list[1], "Emissions|CO2|Fossil Fuels and Industry,Land Use")
  testthat::expect_equal(length(group_name_list), 10)
})

