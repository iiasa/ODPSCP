silence_built_under_warning <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(condition) {
      if (grepl("was built under R version", conditionMessage(condition), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

if (!"ODPSCP" %in% loadedNamespaces()) {
  silence_built_under_warning(
    pkgload::load_all(
      path = normalizePath(testthat::test_path("..", ".."), mustWork = TRUE),
      export_all = FALSE,
      helpers = FALSE,
      quiet = TRUE
    )
  )
}

odpscp_test_symbols <- c(
  "%|NA|%",
  "%||%",
  "%not_in%",
  "app_server",
  "app_sys",
  "app_ui",
  "col_1",
  "col_10",
  "col_12",
  "col_2",
  "col_3",
  "col_4",
  "col_6",
  "col_8",
  "display",
  "drop_nulls",
  "enurl",
  "format_sf_to_studyregion_text",
  "format_text_to_studyregion",
  "get_golem_config",
  "get_glossary_tooltip_terms",
  "get_protocol_elementgroup",
  "get_protocol_ids",
  "is_valid_orcid",
  "jq_hide",
  "list_to_li",
  "list_to_p",
  "load_protocol",
  "glossary_tooltip_json",
  "make_action_button",
  "named_to_li",
  "not_na",
  "not_null",
  "odpscp_theme",
  "read_uploaded_specification_table",
  "rep_br",
  "rv",
  "rvtl",
  "tagRemoveAttributes",
  "undisplay",
  "with_red_star"
)

odpscp_namespace <- asNamespace("ODPSCP")
odpscp_test_env <- environment()

for (symbol in odpscp_test_symbols) {
  assign(
    x = symbol,
    value = get(symbol, envir = odpscp_namespace, inherits = FALSE),
    envir = odpscp_test_env
  )
}
testServer <- getFromNamespace("testServer", "shiny")