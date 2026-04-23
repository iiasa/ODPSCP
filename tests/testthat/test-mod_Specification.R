testthat::test_that("uploaded feature tables accept valid csv and tsv files", {
  feature_data <- data.frame(
    name = c("Feature A", "Feature B"),
    group = c("Species (distributions)", "Connectivity"),
    number = c(1, 2)
  )

  csv_file <- tempfile(fileext = ".csv")
  utils::write.csv(feature_data, csv_file, row.names = FALSE)

  csv_result <- read_uploaded_specification_table(
    datapath = csv_file,
    filename = "features.csv",
    expected_names = c("name", "group", "number")
  )

  testthat::expect_null(csv_result$message)
  testthat::expect_equal(csv_result$data, feature_data)

  tsv_file <- tempfile(fileext = ".tsv")
  utils::write.table(feature_data, tsv_file,
                     sep = "\t", row.names = FALSE, quote = FALSE)

  tsv_result <- read_uploaded_specification_table(
    datapath = tsv_file,
    filename = "features.tsv",
    expected_names = c("name", "group", "number")
  )

  testthat::expect_null(tsv_result$message)
  testthat::expect_equal(tsv_result$data, feature_data)
})

testthat::test_that("uploaded feature tables reject invalid shape or names", {
  invalid_shape <- data.frame(
    name = "Feature A",
    group = "Species (distributions)"
  )

  shape_file <- tempfile(fileext = ".csv")
  utils::write.csv(invalid_shape, shape_file, row.names = FALSE)

  shape_result <- read_uploaded_specification_table(
    datapath = shape_file,
    filename = "bad-shape.csv",
    expected_names = c("name", "group", "number")
  )

  testthat::expect_null(shape_result$data)
  testthat::expect_match(shape_result$message, "exactly 3 columns")

  invalid_names <- data.frame(
    feature = "Feature A",
    category = "Species (distributions)",
    number = 1
  )

  names_file <- tempfile(fileext = ".csv")
  utils::write.csv(invalid_names, names_file, row.names = FALSE)

  names_result <- read_uploaded_specification_table(
    datapath = names_file,
    filename = "bad-names.csv",
    expected_names = c("name", "group", "number")
  )

  testthat::expect_null(names_result$data)
  testthat::expect_match(names_result$message, "must be named name, group, number")
})