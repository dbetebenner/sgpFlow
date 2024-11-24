#' @importFrom data.table getDTthreads setDTthreads
#' @importFrom utils globalVariables packageVersion
#' @importFrom crayon bold magenta
#' @importFrom toOrdinal toOrdinalDate

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
	    magenta(bold("\uD83C\uDF89 sgpFlow v", version)), " - ", toOrdinal::toOrdinalDate(as.character(Sys.Date())), "\n",
	    "\U1F4A1 Tip: ", magenta(bold("> help(\"sgpFlow\")")), "\n",
	    "\U1F310 Docs: ", magenta(bold("https://centerforassessment.github.io/sgpFlow")), "\n",
	    "\u2728 Happy sgpFlowing!"
	)

        # Display the startup message
        packageStartupMessage(message_text)
    }
}
