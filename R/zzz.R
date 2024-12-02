#' @importFrom data.table getDTthreads setDTthreads
#' @importFrom utils globalVariables packageVersion
#' @importFrom crayon bold magenta
#' @importFrom toOrdinal toOrdinalDate

utils::globalVariables(c(
    ".", ".N", ":=", "CJ", "COMPLETED", "CONTENT_AREA", "GRADE",
    "ID", "INDEX", "PERCENTILE_TRAJECTORY", "SCALE_SCORE",
    "SCALE_SCORE_CSEM", "SCALE_SCORE_PRIOR_1", "SGP_BASELINE",
    "SGP_NORM_GROUP_BASELINE", "SGP_NORM_GROUP_BASELINE_SCALE_SCORES",
    "TEMP_1", "VALID_CASE", "VALUE", "YEAR", "variable"
))

.onLoad <- function(libname, pkgname) {
    function(libname, pkgname) {
        available_threads <- data.table::getDTthreads()
        data.table::setDTthreads(available_threads)
        utils::globalVariables(c("."))
    }
}

.onAttach <- function(libname, pkgname) {
    if (interactive()) {
        # Extract version information
        version <- utils::packageVersion("sgpFlow")

        # Define a friendly startup message
	message_text <- paste0(
	    magenta(bold("\uD83C\uDF89 sgpFlow v", version)), " - ", toOrdinal::toOrdinalDate("2024-11-22"), "\n",
	    "\U1F4A1 Tip: ", magenta(bold("> help(\"sgpFlow\")")), "\n",
	    "\U1F310 Docs: ", magenta(bold("https://centerforassessment.github.io/sgpFlow")), "\n",
	    "\u2728 Happy sgpFlowing!"
	)

        # Display the startup message
        packageStartupMessage(message_text)
    }
}
