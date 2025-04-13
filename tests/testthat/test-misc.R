testthat::test_that("Testing misc functions", {

  # Check for a valid ORCID
  testthat::expect_true(
    is_valid_orcid("0000-0002-7569-1390")
  )
})
