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
            magenta(bold("🎉 sgpFlow v", version)), " - ", toOrdinal::toOrdinalDate(as.character(Sys.Date())), "\n",
            "💡 Tip: ", magenta(bold("> help(\"sgpFlow\")")), "\n",
            "🌐 Docs: ", magenta(bold("https://centerforassessment.github.io/sgpFlow")), "\n",
            "✨ Happy sgpFlowing!"
        )

        # Display the startup message
        packageStartupMessage(message_text)
    }
}
