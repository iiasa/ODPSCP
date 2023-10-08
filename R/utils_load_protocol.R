#' This function loads the protocol
#'
#' @description
#' Utility function to load the ODPSCP protocol.
#' @param path_protocol A [`character`] pointing to the destination of the protocol.
#' @return A [`list`] object.
#'
#' @import yaml
#' @noRd
load_protocol <- function(path_protocol = NULL){
  assertthat::assert_that(
    is.null(path_protocol) || is.character(path_protocol)
  )
  if(is.null(path_protocol)){
    path_protocol <- system.file("01_protocol.yaml",
                                 package = "ODPSCP",
                                 mustWork = TRUE)
  }
  assertthat::assert_that(file.exists(path_protocol))

  # Load the protocol
  pp <- yaml::read_yaml(path_protocol)
  return(pp)
}
