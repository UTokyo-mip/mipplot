context("test-generate_color_mapper.R")

test_that("Read rule table with normal colors", {

  # generate mapper object from sample rule table
  mapper <- mipplot_generate_color_mapper(raw_table = mipplot::ar5_db_sample_rule_table)

  # check one of the result color mapping
  # expect_true(mapper['Ocean'] == "#82175a")

  # check the number of common names
  expect_true(length(mapper) == 6)

  # check the results of color mapping
  expect_true(names(mapper[['Emissions|CO2']][1]) == 'Fossil Fuels and Industry')
  expect_true(mapper[['Emissions|CO2']][1] == '#17202a')
  expect_true(names(mapper[['Emissions|CO2']][2]) == 'Land Use')
  expect_true(mapper[['Emissions|CO2']][2] == '#008000')

})
