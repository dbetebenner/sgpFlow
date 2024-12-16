###############################################################################
### Utility functions for sgpFlow package
###############################################################################

##  Use backticks for util function names (e.g., `myUtil`) so not recognized by sinew package for export

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
`ddcast` <-
    function(tmp.dt, ...) {
        if (dim(tmp.dt)[1L] == 0L) {
            return(data.table::data.table(NULL))
        } else {
            data.table::dcast(tmp.dt, ...)
        }
    } ### END ddcast Function

### yearIncrement
`yearIncrement` <-
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
`get.loss.hoss` <-
    function(state, content_area, grade) {
        return(sgpFlow::sgpFlowStateData[[state]][["Achievement"]][["Knots_Boundaries"]][[content_area]][[paste("boundaries", grade, sep = "_")]])
    }


### my.beta
`my.beta` <- 
    function(quantiles, shape1.min.max=c(3,7), shape2.min.max=c(3,7), interpolated.min.value=NULL) {
        my.shape1 <- seq(shape1.min.max[2L], shape1.min.max[1L] , length=100L)
        my.shape2 <- seq(shape2.min.max[1L], shape2.min.max[2L] , length=100L)
        tmp.sample <- stats::rbeta(length(quantiles), shape1=my.shape1[quantiles], shape2=my.shape2[quantiles])
        if (is.null(interpolated.min.value)) {
            return(100*tmp.sample)
        } else {
            tmp.interpolate <- seq(0, interpolated.min.value, length=100L)/100
            return(100*(tmp.sample * (1-tmp.interpolate[quantiles]) + tmp.interpolate[quantiles]))
        }
    }

### beta.copula
#' @importFrom stats rbeta
`beta.copula` <- function(number, quantile, multiplier) {
    return(quantile + (1 - quantile) * rbeta(number, multiplier*quantile, multiplier*(1-quantile)))
}