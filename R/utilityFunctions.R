###############################################################################
### Utility functions for sgpFlow package
###############################################################################

### capWords
#' @title Convert All Caps to Mixed Case
#' @description Converts a character string from all uppercase to mixed case, intelligently handling capitalization and preserving specific words or acronyms in uppercase.
#' 
#' This function is particularly useful for cleaning data where names, titles, or other text fields are stored in all caps. It intelligently converts text to mixed case while preserving the format of numbers, punctuation, and specific words or acronyms that should remain in uppercase.
#' 
#' @param x A character string or vector to be converted to mixed case.
#' @param special.words A character vector specifying words or acronyms that should not be converted to mixed case (e.g., "ELA", "I", "III"). Default includes commonly used acronyms in education and assessment contexts.
#' @return A character string in mixed case, with specific formatting applied.
#' @details 
#' - Words specified in `special.words` remain in uppercase.
#' - Numbers and their formats (e.g., "2.0") are preserved.
#' - Handles punctuation like hyphens, apostrophes, and parentheses appropriately.
#' - Trims leading/trailing spaces and reduces multiple spaces to a single space.
#' 
#' This function can process individual character strings or vectors of strings (using `sapply` for vectorized operations). For factors, it is recommended to apply the function to the levels of the factor.
#' 
#' @examples 
#' \dontrun{
#' if(interactive()) {
#'   # Basic examples
#'   capWords("TEST")             # "Test"
#'   capWords("ELA TEST1 TEST2")  # "ELA Test1 Test2"
#'   capWords("O'NEIL")           # "O'Neil"
#'   capWords("JOHN'S")           # "John's"
#'
#'   # Processing a character vector
#'   test.vector <- paste("TEST", 1:10, sep = "")
#'   sapply(test.vector, capWords)
#'
#'   # Handling factors by converting levels
#'   test.factor <- factor(paste("TEST", rep(letters[1:10], each = 50)))
#'   levels(test.factor) <- sapply(levels(test.factor), capWords)
#'   levels(test.factor)
#' }
#' }
#' 
#' @seealso 
#'  \code{\link[base]{toupper}}, \code{\link[base]{tolower}}
#' @rdname capWords
#' @export capWords
capWords <-
    function(
        x,
        special.words = c("ELA", "I", "II", "III", "IV", "CCSD", "CUSD", "CUD", "USD", "PSD", "UD", "ESD", "DCYF", "EMH", "HS", "MS", "ES", "SES", "IEP", "ELL", "MAD", "PARCC", "SBAC", "SD", "SWD", "US", "SGP", "SIMEX", "SS", "SAT", "PSAT", "WIDA", "ACCESS", "WIDA-ACCESS")
    ) {

        if (is.null(x)) {
            return(NULL)
        }
        if (is.na(x)) {
            return(NA)
        }
        if (identical(x, " ")) {
            return(" ")
        }

        # Basic cleaning: replace underscores, periods, and extra spaces
        x <- trimws(gsub("[_.]", " ", x))

        # Helper function to capitalize while respecting special words and numbers
        capitalize_words <- function(word) {
            # Handle special words
            if (word %in% special.words) {
                return(word)
            } 
            if (grepl("^[0-9]+(\\.[0-9]+)?$", word)) { # Check for numbers
                return(word)
            } 
            if (grepl("'", word)) {# Handle apostrophes explicitly
                parts <- strsplit(word, "'", fixed = TRUE)[[1]]
                if (length(parts) > 1) {
                    if (nchar(parts[2]) == 1) {  # Case where the second part is a single letter
                        parts <- paste0(
                            toupper(substring(parts[1], 1, 1)), tolower(substring(parts[1], 2)),
                            "'", tolower(parts[2])
                        )
                    } else {  # Case where there are multiple characters after the apostrophe
                        parts <- paste0(
                            toupper(substring(parts[1], 1, 1)), tolower(substring(parts[1], 2)),
                            "'", paste0(toupper(substring(parts[-1], 1, 1)), tolower(substring(parts[-1], 2)), collapse = "'")
                        )
                    }
                }
                return(parts)
            }
            return(paste0(toupper(substring(word, 1, 1)), tolower(substring(word, 2))))
        }

        # Split words and process each, handling punctuation (hyphens, apostrophes, parentheses)
        words <- unlist(strsplit(x, "(?=[\\s()-])|(?<=[\\s()-])", perl = TRUE))
        result <- sapply(words, capitalize_words)

        # Combine processed words and fix spacing around punctuation
        s.new <- paste(result, collapse = "")
        s.new <- gsub("\\s+([)-])", "\\1", gsub("([(])\\s+", "\\1", s.new))

        # Final step: Remove extra spaces between words
        s.new <- gsub("\\s+", " ", s.new)
        
        return(s.new)
    } ### END capWords

### ddcast
#' @importFrom data.table data.table dcast
ddcast <-
    function(tmp.dt, ...) {
        if (dim(tmp.dt)[1L] == 0L) {
            return(data.table::data.table(NULL))
        } else {
            data.table::dcast(tmp.dt, ...)
        }
    } ### END ddcast Function

### yearIncrement
yearIncrement <-
    function(
        base_year,
        year_lags
    ) {
        if (is.null(base_year)) {
            return(NULL)
        }

        if (grepl("_", base_year[1L])) {
            base_year_pieces_lagged <- outer(as.numeric(unlist(strsplit(base_year, "_"))), c(0, cumsum(year_lags)), "-")
            sort(apply(base_year_pieces_lagged, 2, function(x) paste(x, collapse = "_")))
        } else {
            as.character(as.numeric(base_year) - rev(c(0, cumsum(year_lags))))
        }
    } ### End yearIncrement

### get.loss.hoss
get.loss.hoss <-
    function(state, content_area, grade) {
        return(sgpFlow::sgpFlowStateData[[state]][["Achievement"]][["Knots_Boundaries"]][[content_area]][[paste("boundaries", grade, sep = "_")]])
    }


### myBeta
#' @importFrom stats rbeta
sgpBeta <-
    function(n, mean.sgp = 50, sd.sgp = 15, sgp.min.value = NULL, sgp.max.value = NULL) {
        # Validate parameters
        if (mean.sgp < 1 || mean.sgp > 99) stop("mean.sgp must be between 1 and 99.")
        if (sd.sgp <= 0) stop("sd.sgp must be a positive number.")

        # Define sgp quantile cuts
        sgp.cuts <- c(0, seq(0.005, 0.995, length=100L), 1)

        # Convert to [0,1] scale
        mu <- mean.sgp/100
        sigma <- sd.sgp/100

        # Calculate maximum possible standard deviation for given mean
        max_sigma <- sqrt(mu * (1 - mu))
        if (sigma >= max_sigma) {
            warning("Requested standard deviation too large. Using maximum possible value.")
            sigma <- max_sigma * 0.99
        }
        
        # Calculate shape parameters
        tmp.shape1 <- mu * (mu * (1 - mu) / sigma^2 - 1)
        tmp.shape2 <- (1 - mu) * (mu * (1 - mu) / sigma^2 - 1)
        
        # Generate beta samples
        tmp.sample <- rbeta(n, shape1=tmp.shape1, shape2=tmp.shape2)
        
        if (!is.null(sgp.min.value) | !is.null(sgp.max.value)) {
            # Set defaults if only one value is provided
            if (is.null(sgp.min.value)) sgp.min.value <- 0 else sgp.min.value <- sgp.min.value/100
            if (is.null(sgp.max.value)) sgp.max.value <- 1 else sgp.max.value <- sgp.max.value/100
            
            # Linear interpolation from [0,1] to [min,max]
            tmp.result <- sgp.min.value + tmp.sample * (sgp.max.value - sgp.min.value)
            mean.sgp.new <- sgp.min.value + mean.sgp * (sgp.max.value - sgp.min.value)
            sd.sgp.new <- sd.sgp * (sgp.max.value - sgp.min.value)
            return(pmin(pmax(findInterval(tmp.result, sgp.cuts)-1L, 1L), 99L))
        } else {
            return(pmin(pmax(findInterval(tmp.sample, sgp.cuts)-1L, 1L), 99L))
        }
    } ### END sgpBeta

### betaCopula
#' @importFrom stats rbeta
betaCopula <- function(number, quantile, multiplier) {
    return(quantile + (1 - quantile) * rbeta(number, multiplier*quantile, multiplier*(1-quantile)))
}

### convertTime
#' @title Convert Time Duration to Human-Readable String
#' @description Converts a time duration in the format "HH:MM:SS" to a human-readable string indicating days, hours, minutes, and seconds.
#' 
#' This function takes a time duration in the format "HH:MM:SS" and converts it to a human-readable string that indicates the number of days, hours, minutes, and seconds. It is particularly useful for displaying time durations in a more understandable format.
#' 
#' @param tmp.time A character string in the format "HH:MM:SS" representing a time duration.
convertTime <-
    function(tmp.time) {
        tmp <- tail(c(0, 0, 0, as.numeric(unlist(strsplit(tmp.time, ":")))), 4)
        tmp.label <- c("Day", "Hour", "Minute", "Second")
        tmp.label[which(tmp!=1)] <- paste0(tmp.label, "s")[which(tmp!=1)]
        return(paste(paste(tmp[tmp!=0], tmp.label[tmp!=0]), collapse=", "))
    } ### END convertTime

### timetakensgpFlow
#' @title Calculate Time Taken for sgpFlow Function
#' @description Calculates the time taken for an sgpFlow function to execute.
#' 
#' This function takes the starting time of an sgpFlow function and calculates the time taken for the function to execute. It is particularly useful for timing the execution of sgpFlow functions.
#' 
#' @param started.at The starting time of the sgpFlow function.
#' @return A character string in the format "HH:MM:SS" indicating the time taken for the function to execute.
timetakensgpFlow <-
    function(started.at) {
        format = function(secs) {
            secs.integer = as.integer(secs)
            sprintf("%02d:%02d:%02d:%.3f", secs.integer%/%86400L, (secs.integer%/%3600L)%%24L, (secs.integer%/%60L)%%60L, secs%%60L)
        }
        tt = proc.time() - started.at
        format(tt[3L])
    } ### END timetakensgpFlow

### messagesgpFlow
#' @title Print Messages to console and log file
#' @description Prints messages to console and a log file.
#' 
#' @param tmp.message The message to print.
messagesgpFlow <-
    function(tmp.message,
	    domain=NULL,
	    appendLF=TRUE) {

	    PrintLogMessage <- function(tmp.message, domain=NULL) {
		    # print log message to file
		    dir.create("Logs", showWarnings = FALSE)
		    logfile <- paste0("Logs/sgpFlow-", utils::packageDescription("sgpFlow")[['Version']], "_", Sys.Date(), ".txt")

		    if (is.call(tmp.message)) {
			    tmp.message2 <- c(paste0("\n\n\t", as.character(tmp.message)[1L], "(\n\t\t"), paste(names(tmp.message)[-1L], as.character(tmp.message)[-1L], sep=" = ", collapse="\n\t\t"), ")\n\n")
			    cat(tmp.message2, file = logfile, append=appendLF)
		    } else cat(tmp.message, "\n", file=logfile, sep="", append=appendLF)
	    }

	    if (!is.call(tmp.message)) {
		    base::message(tmp.message)
	    }
	    PrintLogMessage(tmp.message)
	    invisible()
    } ### END messagesgpFlow
