context("test-mipplot_autofill_color.R")

test_that("Fill colors using mipplot_autofill_color", {

  # generate an incomplete rule table
  incomplete_rule_table <- mipplot::ar5_db_sample_rule_table
  incomplete_rule_table[3, 3] <- "Emissions|CO2|Electric"
  incomplete_rule_table[3, 4] <- ""
  expect_true(nchar(incomplete_rule_table[3, 4]) == 0)

  # run autofill function
  complete_rule_table <- mipplot_autofill_color(incomplete_rule_table)
  expect_true(nchar(complete_rule_table[3, 4]) > 0)

})
