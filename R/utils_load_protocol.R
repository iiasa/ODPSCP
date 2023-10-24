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

#' Get render ids from the protocol
#'
#' @description
#' Utility function to get all ids from a protocol
#' @param path_protocol A [`character`] pointing to the destination of the protocol.
#' @return A [`character`] object with all ids.
#'
#' @import yaml
#' @noRd
get_protocol_ids <- function(path_protocol = NULL, group = NULL){
  # Load the protocol
  protocol <- load_protocol(path_protocol)

  if(!is.null(group)){
    protocol <- protocol[[group]]
  }

  # Get all render ids
  pp <- unlist(protocol)
  pp <- pp[grep("render-id", names(pp))] |> as.character()
  assertthat::assert_that(length(pp)>0, is.character(pp))
  return(pp)
}

#' Get protocol version
#'
#' @description
#' Utility function to get the protocol version number
#' @param path_protocol A [`character`] pointing to the destination of the protocol.
#' @return A [`character`] with the current version number
#'
#' @import yaml
#' @noRd
get_protocol_version <- function(path_protocol = NULL){
  # Load the protocol
  protocol <- load_protocol(path_protocol)
  version <- protocol$protocol$version
  return(version)
}
