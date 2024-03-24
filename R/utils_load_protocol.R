#' This function loads the protocol
#'
#' @description
#' Utility function to load the ODPSCP protocol.
#' @param path_protocol A [`character`] pointing to the destination of the protocol.
#' @return A [`list`] object.
#'
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
#' @noRd
get_protocol_ids <- function(path_protocol = NULL, group = NULL){
  # Load the protocol
  protocol <- load_protocol(path_protocol)

  if(!is.null(group)){
    group <- match.arg(tolower(group), c("protocol","overview",
                                       "design","specification",
                                       "context","prioritization"), several.ok = FALSE)
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
#' @noRd
get_protocol_version <- function(path_protocol = NULL){
  # Load the protocol
  protocol <- load_protocol(path_protocol)
  version <- protocol$protocol$version
  return(version)
}

#' Get protocol group for render-id
#'
#' @description
#' Utility function to get the group for a render-id
#' @param path_protocol A [`character`] pointing to the destination of the protocol.
#' @return A [`data.frame`] with the group and element
#'
#' @noRd
get_protocol_elementgroup <- function(id, path_protocol = NULL){
  assertthat::assert_that(
    is.character(id),
    is.null(path_protocol) || is.character(path_protocol)
  )
  # Load the protocol
  template <- load_protocol(path_protocol)

  # FIXME: Hacky, lazy coding from a train
  out <- data.frame()
  for(gr in names(template)[-1]){
    pp <- template[[gr]]
    for(element in names(pp)){
      ppp <- pp[[element]]
      if(ppp[['render-id']] == id){
        out <- dplyr::bind_rows(
          out,
          data.frame(group = gr, element = element, id = id)
        )
      }
    }
  }
  if(nrow(out)==0) return( NULL )
  return(out)
}


#' Check for mandatory fields to be filled
#'
#' @description
#' Small convenience function that checks for mandatory fields
#'
#' @param protocol A filled out protocol in [`list`] format.
#' @returns A [`vector`] of character entries that are mandatory in the protocol.
#' @noRd
get_protocol_mandatory <- function(path_protocol = NULL){
  assertthat::assert_that(is.character(path_protocol) || is.null(path_protocol))

  # If is null, load protocol
  template <- load_protocol(path_protocol)

  results <- vector()
  for(gr in names(template)[-1]){
    pp <- template[[gr]]
    for(element in names(pp)){
      ppp <- pp[[element]]
      if(ppp$mandatory) results <- append(results,values = ppp[['render-id']] )
    }
  }
  return(results)
}
