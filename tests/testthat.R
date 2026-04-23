# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

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

silence_built_under_warning(library(testthat))
silence_built_under_warning(library(ODPSCP))

test_check("ODPSCP")
