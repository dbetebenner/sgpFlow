#' @importFrom data.table getDTthreads setDTthreads
#' @importFrom utils globalVariables packageDescription
#' @importFrom crayon bold cyan green magenta red
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

`.onAttach` <- function(libname, pkgname) {
    if (interactive()) {
		# Utility function 
        get_dev_version <- function(package) {
            url <- paste0("https://raw.githubusercontent.com/CenterForAssessment/", package, "/refs/heads/main/DESCRIPTION")
            tryCatch({
                lines <- readLines(url, warn = FALSE)
                version_line <- grep("^Version:", lines, value = TRUE)
                if (length(version_line) > 0) {
                    return(cyan("v", strsplit(version_line, ": ")[[1]][2], sep=""))
                } else {
                    return(red("Not Available"))
                }
            }, error = function(e) {
                return(red("Not Available"))
            }, warning = function(w) {
                return(red("Not Available"))
            })
        }

        # Extract version information
        installed.version <- utils::packageDescription("sgpFlow")[['Version']]
        cran.version <- tryCatch(
            green("v", pkgsearch::cran_package("sgpFlow")[['Version']], sep=""),
            error = function(e) red("Not Available"),
            warning = function(w) red("Not Available"))
        dev.version <- get_dev_version("sgpFlow")

        # Define a friendly startup message
		message_text <- paste0(
		    magenta(bold("\uD83C\uDF89 sgpFlow v", installed.version, sep="")), " - ", toOrdinal::toOrdinalDate("2024-12-18"), "\n",
			strrep("\u2501", 40), "\n",
    	    bold("\U1F4E6 CRAN: "), cran.version, "\n",
    	    bold("\U1F527 Dev: "), dev.version, "\n",
			strrep("\u2501", 40), "\n",
		    "\U1F4A1 Tip: ", magenta(bold("> help(package=\"sgpFlow\")")), "\n",
		    "\U1F310 Docs: ", magenta(bold("https://centerforassessment.github.io/sgpFlow")), "\n",
			strrep("\u2501", 40), "\n",
		    "\u2728 Happy sgpFlowing!")

        # Display the startup message
        packageStartupMessage(message_text)
    }
}
