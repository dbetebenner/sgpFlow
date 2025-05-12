#' @importFrom data.table getDTthreads setDTthreads
#' @importFrom utils globalVariables packageDescription
#' @importFrom crayon bold cyan green magenta red
#' @importFrom toOrdinal toOrdinalDate
#' @importFrom curl curl_fetch_memory new_handle

utils::globalVariables(c(
    ".", ".N", ":=", "CJ", "COMPLETED", "CONTENT_AREA", "GRADE",
    "ID", "INDEX", "PERCENTILE_TRAJECTORY", "SCALE_SCORE",
    "SCALE_SCORE_CSEM", "SCALE_SCORE_PRIOR_1", "SGP_BASELINE",
    "SGP_NORM_GROUP_BASELINE", "SGP_NORM_GROUP_BASELINE_SCALE_SCORES",
    "TEMP_1", "VALID_CASE", "VALUE", "YEAR", "variable", "scale_score.names",
    "..cols_to_select", "..scale_score.names", "cols_to_select", ".SD",
    "SCALE_SCORE_STANDARDIZED"
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
		# Utility function with timeout
        get_dev_version <- function(package, timeout = 2) {
            url <- paste0("https://raw.githubusercontent.com/CenterForAssessment/", package, "/refs/heads/main/DESCRIPTION")
            tryCatch({
                # Use curl with timeout
                response <- curl::curl_fetch_memory(url, handle = curl::new_handle(timeout = timeout))
                if (response$status_code == 200) {
                    lines <- strsplit(rawToChar(response$content), "\n")[[1]]
                    version_line <- grep("^Version:", lines, value = TRUE)
                    if (length(version_line) > 0) {
                        return(cyan("v", strsplit(version_line, ": ")[[1]][2], sep=""))
                    }
                }
                return(red("Not Available"))
            }, error = function(e) {
                return(red("Not Available"))
            }, warning = function(w) {
                return(red("Not Available"))
            })
        }

        # Extract version information with timeout
        installed.version <- utils::packageDescription("sgpFlow")[['Version']]
        cran.version <- tryCatch({
            # Add timeout to CRAN check
            curl::curl_fetch_memory("https://cran.r-project.org/web/packages/sgpFlow/index.html", 
                                  handle = curl::new_handle(timeout = 2))
            green("v", pkgsearch::cran_package("sgpFlow")[['Version']], sep="")
        }, error = function(e) red("Not Available"),
           warning = function(w) red("Not Available"))
        dev.version <- get_dev_version("sgpFlow")

        # Define a friendly startup message
		message_text <- paste0(
		    magenta(bold("\uD83C\uDF89 sgpFlow v", installed.version, sep="")), " - ", toOrdinal::toOrdinalDate("2025-5-10"), "\n",
			strrep("\u2501", 40), "\n",
    	    bold("\U1F4E6 CRAN: "), cran.version, "\n",
    	    bold("\U1F527 Dev: "), dev.version, "\n",
			strrep("\u2501", 40), "\n",
		    "\U1F4A1 Tip: ", magenta(bold("> help(package=\"sgpFlow\")")), "\n",
		    "\U1F310 Docs: ", magenta(bold("https://centerforassessment.github.io/sgpFlow")), "\n",
			strrep("\u2501", 40), "\n",
		    "\u2728 Happy sgpFlowing!", "\n")

        # Display the startup message
        packageStartupMessage(message_text)
    }
}
