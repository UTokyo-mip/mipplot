context("test-read_ruletab.R")

test_that("Load normal data without exception.", {

  # Convert rule table without id to one with id.
  converted <- mipplot_read_ruletab(mipplot::ar5_db_rule_table_v09_wo_id)

  # Check if converted rule table contains rule id header.
  expect_true("Rule_ID" %in% colnames(converted))

  # Check if converted rule table has the same number of rows as original.
  expect_equal(nrow(converted), nrow(mipplot::ar5_db_rule_table_v09_wo_id))

})

test_that("Stop when empty data is given", {

  # Create empty data.
  empty_rules_without_id <- mipplot::ar5_db_rule_table_v09_wo_id[0, ]

  # Process empty data.
  expect_error({

    # Raise error
    mipplot_read_ruletab(empty_rules_without_id)

  })
})

test_that("Stop when input data is not a data.frame", {

  # Create not data.frame data.
  not_data_frame = list(Left_side = 1, Right_side = 1)

  # Process the data.
  expect_error({

    # Raise error
    mipplot_read_ruletab(not_data_frame)

  })
})

test_that("Stop when input data dosen't have all required columns.", {

  # Create data which has Left_side but not Right_side.
  half_data = mipplot::ar5_db_rule_table_v09_wo_id[, 1]

  # Process the data.
  expect_error({

    # Raise error
    mipplot_read_ruletab(half_data)

  })
})
