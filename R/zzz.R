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
    version <- packageVersion("sgpFlow")
    
    # Define a friendly startup message
    message_text <- paste0(
      magenta$bold("sgpFlow"), " v", version, 
      " (11-3-2024)\n",
      "For help, type: ", magenta$bold("> help(\"sgpFlow\")"), " or visit:\n",
      magenta$bold("https://centerforassessment.github.io/sgpFlow")
    )
    
    # Display the startup message
    packageStartupMessage(message_text)
  }
}
