#' Prepare glossary tooltip terms
#'
#' @description
#' Internal helpers for attaching glossary definitions to protocol copy.
#'
#' @param path_glossary Optional path to the glossary csv.
#'
#' @return A data frame with matched text and definitions.
#' @noRd
get_glossary_tooltip_terms <- function(path_glossary = NULL) {
  assertthat::assert_that(
    is.null(path_glossary) || is.character(path_glossary)
  )

  if (is.null(path_glossary)) {
    path_glossary <- system.file(
      "glossary_table.csv",
      package = "ODPSCP",
      mustWork = TRUE
    )
  }

  glossary <- utils::read.csv(
    path_glossary,
    sep = ",",
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  glossary$Term <- trimws(glossary$Term)
  glossary$Abbreviation <- trimws(glossary$Abbreviation)
  glossary$Definition <- trimws(glossary$Definition)

  keep_definition <- !is.na(glossary$Definition) & nzchar(glossary$Definition)
  keep_term <- !is.na(glossary$Term) & nzchar(glossary$Term)
  keep_abbreviation <- !is.na(glossary$Abbreviation) &
    nzchar(glossary$Abbreviation) &
    nchar(glossary$Abbreviation) > 1

  term_rows <- data.frame(
    match = glossary$Term[keep_definition & keep_term],
    definition = glossary$Definition[keep_definition & keep_term],
    stringsAsFactors = FALSE
  )

  abbreviation_rows <- data.frame(
    match = glossary$Abbreviation[keep_definition & keep_abbreviation],
    definition = glossary$Definition[keep_definition & keep_abbreviation],
    stringsAsFactors = FALSE
  )

  entries <- rbind(term_rows, abbreviation_rows)
  if (nrow(entries) == 0) {
    return(entries)
  }

  entries$key <- tolower(entries$match)
  entries <- entries[!duplicated(entries$key), , drop = FALSE]
  entries$match_width <- nchar(entries$match)
  entries <- entries[order(-entries$match_width, entries$match), c("match", "definition")]
  rownames(entries) <- NULL

  entries
}

#' Serialize glossary tooltip terms
#'
#' @param path_glossary Optional path to the glossary csv.
#'
#' @return A JSON string.
#' @noRd
glossary_tooltip_json <- function(path_glossary = NULL) {
  jsonlite::toJSON(
    get_glossary_tooltip_terms(path_glossary = path_glossary),
    auto_unbox = TRUE,
    dataframe = "rows"
  )
}

#' Add glossary tooltip resources
#'
#' @param path_glossary Optional path to the glossary csv.
#'
#' @return A tag list containing css, data and javascript.
#' @noRd
glossary_tooltip_resources <- function(path_glossary = NULL) {
  shiny::tagList(
    shiny::tags$style(
      shiny::HTML(
        paste(
          ".odpscp-glossary-term {",
          "border-bottom: 1px dotted rgba(60, 141, 188, 0.9);",
          "cursor: help;",
          "}",
          ".odpscp-glossary-disabled .odpscp-glossary-term {",
          "border-bottom-color: transparent;",
          "cursor: inherit;",
          "}",
          ".odpscp-glossary-term:hover, .odpscp-glossary-term:focus {",
          "background-color: rgba(60, 141, 188, 0.16);",
          "outline: none;",
          "}",
          ".odpscp-glossary-tooltip .tooltip-inner {",
          "background-color: rgba(33, 37, 41, 0.97);",
          "}",
          ".odpscp-glossary-tooltip.bs-tooltip-top .arrow::before,",
          ".odpscp-glossary-tooltip.bs-tooltip-auto[x-placement^='top'] .arrow::before {",
          "border-top-color: rgba(33, 37, 41, 0.97);",
          "}",
          ".odpscp-glossary-tooltip.bs-tooltip-bottom .arrow::before,",
          ".odpscp-glossary-tooltip.bs-tooltip-auto[x-placement^='bottom'] .arrow::before {",
          "border-bottom-color: rgba(33, 37, 41, 0.97);",
          "}",
          ".odpscp-glossary-tooltip.bs-tooltip-left .arrow::before,",
          ".odpscp-glossary-tooltip.bs-tooltip-auto[x-placement^='left'] .arrow::before {",
          "border-left-color: rgba(33, 37, 41, 0.97);",
          "}",
          ".odpscp-glossary-tooltip.bs-tooltip-right .arrow::before,",
          ".odpscp-glossary-tooltip.bs-tooltip-auto[x-placement^='right'] .arrow::before {",
          "border-right-color: rgba(33, 37, 41, 0.97);",
          "}",
          sep = " "
        )
      )
    ),
    shiny::tags$script(
      id = "odpscp-glossary-data",
      type = "application/json",
      shiny::HTML(glossary_tooltip_json(path_glossary = path_glossary))
    ),
    shiny::tags$script(src = "www/glossary-tooltips.js")
  )
}