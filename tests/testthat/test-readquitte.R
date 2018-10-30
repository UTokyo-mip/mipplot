context("test-read_iamc.R")

test_that("Read the file that meets the specification", {

  # Read sample iamc format file
  iamc_file_path = system.file("mipplot", "ar5_db_sample_data.csv", package = "mipplot", mustWork = TRUE)

  # Load iamc format file.
  loaded_data <- mipplot_read_iamc(iamc_file_path, sep = ",", interactive = FALSE)

  # Check if loaded data is not empty or not.
  expect_gt(nrow(loaded_data), 0)

})

