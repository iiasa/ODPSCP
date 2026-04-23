testthat::test_that("Testing misc functions", {

  square_a <- sf::st_polygon(list(matrix(
    c(0, 0,
      1, 0,
      1, 1,
      0, 1,
      0, 0),
    ncol = 2,
    byrow = TRUE
  )))
  square_b <- sf::st_polygon(list(matrix(
    c(1, 1,
      2, 1,
      2, 2,
      1, 2,
      1, 1),
    ncol = 2,
    byrow = TRUE
  )))
  studyregion <- sf::st_sf(
    geometry = sf::st_sfc(square_a, square_b, crs = 4326)
  )

  serialized_region <- format_sf_to_studyregion_text(studyregion)
  roundtrip_region <- format_text_to_studyregion(serialized_region)

  testthat::expect_s3_class(roundtrip_region, "sf")
  testthat::expect_equal(as.character(sf::st_geometry_type(roundtrip_region)[1]), "MULTIPOLYGON")
  testthat::expect_equal(sf::st_crs(roundtrip_region)$epsg, 4326)

  # Check for a valid ORCID
  testthat::expect_true(
    is_valid_orcid("0000-0002-7569-1390")
  )
})
