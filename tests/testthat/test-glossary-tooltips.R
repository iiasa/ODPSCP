testthat::test_that("Glossary tooltip terms are prepared consistently", {
  glossary_path <- system.file("glossary_table.csv", package = "ODPSCP", mustWork = TRUE)

  entries <- get_glossary_tooltip_terms(path_glossary = glossary_path)

  testthat::expect_s3_class(entries, "data.frame")
  testthat::expect_true(all(c("match", "definition") %in% names(entries)))
  testthat::expect_true(any(entries$match == "Planning unit"))
  testthat::expect_true(any(entries$match == "SCP"))
  testthat::expect_false(any(entries$match == "s"))
  testthat::expect_false(any(grepl("\\s$", entries$match)))

  tooltip_json <- glossary_tooltip_json(path_glossary = glossary_path)
  parsed <- jsonlite::fromJSON(tooltip_json)

  testthat::expect_true(any(parsed$match == "Planning unit"))
  testthat::expect_true(any(parsed$match == "Representativeness"))
})
