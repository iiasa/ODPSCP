#' Parse spatial data and convert to [`sf`] format
#'
#' @description
#' This is a small helper function that converts any type of spatial
#' format to a [`sf`] object.
#' @param file A [`character`] file path
#' @param make_valid A [`logical`] on whether \code{'file'} should be ensured to
#' be a valid geometry (Default: \code{TRUE}).
#' @keywords internal
#' @noRd
spatial_to_sf <- function(file, make_valid = TRUE){
  assertthat::assert_that(is.character(file),
                          is.logical(make_valid))

  # Get file extension
  ext <- tolower( tools::file_ext(file) )

  # Found vector
  if(ext %in% c("shp","gpkg")){
    out <- sf::st_read(file, quiet = TRUE)
  } else if(ext %in% c("zip")) {
    # Unzip first
    tmpdir <- paste0(base::tempdir(),"_fileupload")
    dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
    # Extract
    utils::unzip(zipfile = file, exdir = tmpdir)
    # List files
    ll <- list.files(tmpdir, full.names = TRUE)
    ll <- ll[assertthat::has_extension(ll, 'shp')]
    if(length(ll)==0) return(NULL)
    if(length(ll)>1) ll <- ll[1] # Take first

    out <- sf::st_read(ll, quiet = TRUE)
    # Remove files
    base::unlink(tmpdir, recursive = TRUE)

  } else if( ext %in% c("tif","geotiff")){
    out <- terra::rast(file)
    out[out>0] <- 1 # Replace all with 1
    # If there are 0 assume those should be NA
    out[out==0] <- NA
    out <- out |> terra::as.polygons() |> sf::st_as_sf()
  } else {
    return( NULL )
  }
  # --- #
  # Check for empty crs
  if(is.na(sf::st_crs(out))){
    # Assume long-lat
    out <- sf::st_set_crs(out, value = sf::st_crs(4326))
  }

  # Check for empty geometries
  if(all( sf::st_is_empty(out) )) return( NULL )

  if(make_valid){
    if(!all( sf::st_is_valid(out) )){
      out <- sf::st_make_valid(out)
    }
  }

  # Convert to MULTIPOLYGON
  out <- out |> sf::st_cast("MULTIPOLYGON")

  # Transform
  out <- out |> sf::st_transform(crs = sf::st_crs(4326))

  # Rename geometry name to be sure
  sf::st_geometry(out) <- "geometry" # rename

  return(out)
}

#' Small helper for spatial conversion to wkt
#' @param val A [`list`] with the datapath for the spatial file
#' @return A [`character`] with a WKT.
#' @noRd
format_studyregion_to_text <- function(val){
  assertthat::assert_that(utils::hasName(val, "datapath"))
  # Check that if file exists
  if(!file.exists(val$datapath)) return("Studyregion could not be loaded?")
  # Load from data path
  val <- try({
    spatial_to_sf(val$datapath, make_valid = FALSE)
  }, silent = TRUE)
  if(inherits(val,"try-error")){
    val <- "Studyregion could not be loaded?"
  } else {
    # Convert to sfc
    val <- val |> sf::st_as_sfc()
    val <- paste0(
      # Also append SRID in front
      sf::st_crs(val) |> sf::st_as_text(),";", sf::st_as_text(val)
    )
  }
  return(val)
}

#' Small helper for conversion wkt to spatial
#' @param val A [`character`] with studyregion string
#' @return A [`sf`] object.
#' @noRd
format_text_to_studyregion <- function(val){
  assertthat::assert_that(is.character(val))

  # Split by semicolon
  ss <- strsplit(val,";")[[1]]

  # Convert to spatial and set crs
  pol <- sf::st_as_sfc(ss[[2]])
  pol <- sf::st_set_crs(pol, value = sf::st_crs(ss[1]) )

  # Convert to sf
  if(!inherits(pol, "sf")){
    pol <- sf::st_as_sf(pol)
  }

  # Rename geometry name to be sure
  sf::st_geometry(pol) <- "geometry" # rename

  return(pol)
}

#' Search for a ORCID id online
#'
#' @description
#' This function searches for a ORCID id online and returns the
#' first and surname of any found resarchers
#'
#' @param val A [`character`] with the ORCID identifier.
#'
#' @return A [`vector`] with the first and surname of the researcher. If it fails,
#' return `NULL`.
#' @keywords internal
#' @noRd
get_names_from_orcid <- function(val){
  assertthat::assert_that(is.character(val))

  # Get the request from online
  url <- paste0("https://pub.orcid.org/v3.0/", val, "/person")
  res <- try({ httr::GET(url, httr::accept("application/json")) },silent = TRUE)

  # If the request failed, return NULL
  if(inherits(res, "try-error")) {
    return(NULL)
  }

  # Check if the request was successful
  if(httr::status_code(res) == 200) {
    data <- jsonlite::fromJSON(httr::content(res, as = "text"))
    name <- data$name
    given <- name$`given-names`$value
    family <- name$`family-name`$value
  } else {
    return(NULL)
  }
  # Check if the name is a character
  if(!is.character(given) || !is.character(family)){
    return(NULL)
  }

  # Check for other issues
  if(is.null(given) || is.null(family)){
    return(NULL)
  }

  # Return name
  return(c(
    forname = given,
    surename = family
  ))
}

#' Validate a ORCID identifier
#'
#' @description
#' This function checks if the ORCID identifier is valid.
#' @param val A [`character`] with the ORCID identifier.
#' @keywords internal
#' @noRd
is_valid_orcid <- function(val) {
  assertthat::assert_that(is.character(val))

  grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{3}", val)
}
