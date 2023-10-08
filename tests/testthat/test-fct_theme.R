test_that("Check that the custom theme works", {
  testthat::skip_if_not_installed(pkg = "fresh")
  # Load the theme
  tt <- odpscp_theme()
  expect_s3_class(tt, "css")
})
