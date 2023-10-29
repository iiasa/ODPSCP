test_that("Check that the protocol can be parsed", {
  library(yaml)

  # Path to protocol
  # FIXME: Ideally version nr agnostic
  path_protocol <- system.file("01_protocol.yaml", package = "ODPSCP",mustWork = TRUE)

  # Check that file exists
  expect_true( file.exists(path_protocol) )

  # Check that it can be parsed
  expect_no_error(pp <- load_protocol(path_protocol) )
  expect_type(pp, "list")
  expect_gte(length(pp), 2)

  # Check protocol information
  expect_equal(pp$protocol$name, "ODPSCP")

  # check that each overview option has a title and description and mandatory flag
  # at least
  ll <- sapply(pp$overview, function(z) names(z))
  expect_gte( length(ll), 6)
  expect_true( all( sapply(ll, function(z) "question" %in% z) ) )
  expect_true( all( sapply(ll, function(z) "description" %in% z) ) )
  expect_true( all( sapply(ll, function(z) "mandatory" %in% z) ) )

  # Query that we have ids for all entries
  expect_no_error( get_protocol_ids(path_protocol = path_protocol,group = "overview") )
  ids <- get_protocol_ids(group = "overview")
  expect_gte( length(ids), 10)
  expect_true( anyDuplicated(ids)==0 )
  ids <- get_protocol_ids(group = "design")
  expect_gte( length(ids), 10)
  expect_true( anyDuplicated(ids)==0 )
  ids <- get_protocol_ids(group = "specification")
  expect_gte( length(ids), 10)
  expect_true( anyDuplicated(ids)==0 )
  ids <- get_protocol_ids(group = "context")
  expect_gte( length(ids), 5)
  expect_true( anyDuplicated(ids)==0 )
  ids <- get_protocol_ids(group = "prioritization")
  expect_gte( length(ids), 5)
  expect_true( anyDuplicated(ids)==0 )

  # TODO:
  # Check that all options with pre-defined entries have such options
  # Check that fieldtypes are among recognized types
})
