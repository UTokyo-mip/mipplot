context("test-generate_color_mapper.R")

test_that("Read rule table with normal colors", {

  # generate mapper object from sample rule table
  mapper <- mipplot_generate_color_mapper(raw_table = mipplot::ar5_db_sample_rule_table)

  # check one of the result color mapping
  expect_true(mapper['Ocean'] == "#82175a")

})
