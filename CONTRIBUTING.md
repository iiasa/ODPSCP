# Contributing to ODPSCP

Thank you for your interest in contributing! This guide covers the essentials
for code contributors, protocol maintainers, and anyone hosting a mirror.

## Code of Conduct
All contributors must follow the [Contributor Code of Conduct](CODE_OF_CONDUCT.md).

---

## Getting started locally

```r
# Install development dependencies
install.packages("devtools")

# Clone the repository, then from the root:
devtools::install_deps()   # install all Imports / Suggests
devtools::load_all()       # load package without installing
ODPSCP::run_app()          # launch the app
```

The package is built with the [golem](https://thinkr-open.github.io/golem/)
framework. Entry points: `R/app_ui.R` (layout) and `R/app_server.R` (logic).

---

## Repository layout

| Path | Purpose |
|---|---|
| `R/mod_*.R` | Shiny modules — one per protocol section |
| `R/utils_load_protocol.R` | Protocol schema queries (`load_protocol`, `get_protocol_ids`, …) |
| `R/utils_format_protocol.R` | Export formatting (`format_protocol`, `protocol_to_document`) |
| `R/misc.R` | Spatial helpers, ORCID API |
| `inst/01_protocol.yaml` | Single source of truth for all form fields |
| `inst/glossary_table.csv` | Domain terminology rendered in the Glossary tab |
| `tests/testthat/` | Automated tests |

---

## Adding or changing protocol fields

All form fields are defined in `inst/01_protocol.yaml`. Each entry must have at
minimum:

```yaml
- render-id: my_new_field        # unique identifier used as the reactive key
  question:  "Question text?"
  description: "Helper text shown in UI"
  fieldtype: textbox             # textbox | radio | checkbox | slider | …
  mandatory: false
```

Conditional "other" free-text siblings use:

```yaml
  fieldtype_conditional_render-id: my_new_field_other
  fieldtype_conditional: textbox
```

After editing the YAML:
1. Run `devtools::test()` — the protocol schema tests in
   `tests/testthat/test-yaml_protocol_correct.R` will catch duplicated or
   missing ids.
2. Verify the new field appears in the correct module tab and that the export
   (CSV, YAML, docx) includes it.

---

## Running the tests

```r
devtools::test()
# or
testthat::test_check("ODPSCP")
```

The test suite currently covers:
- Protocol YAML structure and uniqueness of render-ids
- Golem framework compliance (`app_ui` / `app_server` signatures)
- Utility functions (`drop_nulls`, operators, theme generation)
- ORCID format validation

New contributions that touch import/export or spatial handling should add
corresponding tests under `tests/testthat/`.

---

## Pull request checklist

- [ ] `devtools::check()` passes without errors or warnings
- [ ] `devtools::test()` passes
- [ ] Protocol changes include a YAML schema test update
- [ ] `NEWS.md` entry added under the appropriate version heading
- [ ] No new hard-coded file paths or credentials

---

## Hosting a mirror

See `deploy/README` for Docker build and run instructions. Once your mirror is
live, open an issue or PR to be added to the mirrors table in `README.md`.
Institutional logos and branding are welcome; keep the protocol fields
consistent with the ODPSCP standard.
