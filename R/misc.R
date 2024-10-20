#' Parse spatial data and convert to [`sf`] format
#'
#' @description
#' This is a small helper function that converts any type of spatial
#' format to a [`sf`] object.
#' @param file A [`character`] file path
#' @param make_valid A [`logical`] on whether \code{'file'} should be ensured to
#' be a valid geometry (Default: \code{FALSE}).
#' @keywords internal
#' @noRd
spatial_to_sf <- function(file, make_valid = FALSE){
  assertthat::assert_that(is.character(file),
                          is.logical(make_valid))

  # Get file extension
  ext <- tolower( tools::file_ext(file) )

  # Found vector
  if(ext %in% c("shp","gpkg")){
    out <- sf::st_read(file, quiet = TRUE)
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
