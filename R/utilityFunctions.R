###############################################################################
### Utility functions for sgpFlow package
###############################################################################

##  Use backticks for util function names (e.g., `myUtil`) so not recognized by sinew package for export

### capWords


#' Function for converting all caps to mixed case. Useful in data cleaning.
#' 
#' The function capWords converts characters to mixed case character as
#' intelligently as possible and leading/trailing spaces.
#' 
#' 
#' @usage capWords(x, special.words = c("ELA","I", "II", "III", "IV", "CCSD",
#' "CUSD", "CUD", "USD", "PSD", "UD", "ESD", "DCYF", "EMH", "HS", "MS", "ES",
#' "SES", "IEP", "ELL", "MAD", "PARCC", "SBAC", "SD", "SWD", "US", "SGP",
#' "SIMEX", "SS", "SAT", "PSAT", "WIDA", "ACCESS", "WIDA-ACCESS"))
#' @param x A character string to be converted to mixed case.
#' @param special.words A character vector (see default above), specifying
#' words to not convert to mixed case.
#' @return Returns a mixed case character string.
#' @author Damian W. Betebenner \email{dbetebenner@@nciea.org}
#' @keywords documentation
#' @examples
#'
#' \dontrun{
#' if(interactive()){
#'   capWords("TEST") ## Test
#'   capWords("TEST1 TEST2") ## Test1 Test2
#'   capWords("O'NEIL") ## O'Neil
#'   capWords("JOHN'S") ## John's
#'
#'   ## Use sapply for converting character vectors
#'
#'   test.vector <- paste("TEST", 1:10, sep="")
#'   sapply(test.vector, capWords)
#'
#'
#' ## With factors, convert levels instead of the entire vector
#'
#'   test.factor <- factor(paste("TEST", rep(letters[1:10], each=50)))
#'   levels(test.factor) <- sapply(levels(test.factor), capWords)
#'   levels(test.factor)
#'  }
#' }
#' @rdname sgpFlowTr
#' @export capWords
capWords <-
    function(
        x,
        special.words = c("ELA", "I", "II", "III", "IV", "CCSD", "CUSD", "CUD", "USD", "PSD", "UD", "ESD", "DCYF", "EMH", "HS", "MS", "ES", "SES", "IEP", "ELL", "MAD", "PARCC", "SBAC", "SD", "SWD", "US", "SGP", "SIMEX", "SS", "SAT", "PSAT", "WIDA", "ACCESS", "WIDA-ACCESS")) {
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
        x <- gsub("[_.]", " ", x)
        x <- gsub("\\s+", " ", trimws(x)) # Trim and reduce multiple spaces

        # Helper function to capitalize while respecting special words and numbers
        capitalize_words <- function(word) {
            # Handle special words
            if (word %in% special.words) {
                return(word)
            } else if (grepl("^[0-9]+(\\.[0-9]+)?$", word)) { # Check for numbers
                return(word)
            } else {
                # Capitalize the first letter only
                return(paste0(toupper(substring(word, 1, 1)), tolower(substring(word, 2))))
            }
        }

        # Split words and process each, handling punctuation (hyphens, apostrophes)
        words <- unlist(strsplit(x, "(?=[\\s-'])|(?<=[\\s-'])", perl = TRUE))
        result <- sapply(words, capitalize_words)

        # Combine processed words and fix spacing around punctuation
        s.new <- paste(result, collapse = "")
        s.new <- gsub("\\s+([)-])", "\\1", gsub("([(])\\s+", "\\1", s.new))

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

`get.loss.hoss` <-
    function(state, content_area, grade) {
        return(sgpFlow::sgpFlowStateData[[state]][["Achievement"]][["Knots_Boundaries"]][[content_area]][[paste("boundaries", grade, sep = "_")]])
    }

`my.beta` <- 
    function(quantiles, shape1.min.max=c(3,7), shape2.min.max=c(3,7), interpolated.bottom=NULL) {
        my.shape1 <- seq(shape1.min.max[2L], shape1.min.max[1L] , length=100L)
        my.shape2 <- seq(shape2.min.max[1L], shape2.min.max[2L] , length=100L)
        tmp.sample <- stats::rbeta(length(quantiles), shape1=my.shape1[quantiles], shape2=my.shape2[quantiles])
        if (is.null(interpolated.bottom)) {
            return(100*tmp.sample)
        } else {
            tmp.interpolate <- seq(0, interpolated.bottom, length=100L)/100
            return(100*(tmp.sample * (1-tmp.interpolate[quantiles]) + tmp.interpolate[quantiles]))
        }
    }
