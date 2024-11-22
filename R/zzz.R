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
            magenta(bold("sgpFlow")), " v", version,
            toOrdinal::toOrdinalDate(2024-11-22), "\n",
            "For help, type: ", magenta(bold("> help(\"sgpFlow\")")), " or visit:\n",
            magenta(bold("https://centerforassessment.github.io/sgpFlow"))
        )

        # Display the startup message
        packageStartupMessage(message_text)
    }
}
