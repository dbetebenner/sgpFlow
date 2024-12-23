#' Generate Growth Distribution Indices
#'
#' This internal function calculates indices based on the specified growth distribution. 
#' It supports "UNIFORM_RANDOM" for generating random indices within a range, numeric 
#' growth distributions (e.g., "1" to "99") for fixed percentiles, and includes a 
#' placeholder for "BETA" distribution (currently not implemented).
#'
#' @param wide_data A data.table containing student or entity data. The number of rows 
#'   determines how many indices to generate.
#' @param growth.distribution A list containing the growth distribution parameters:
#'   \itemize{
#'     \item \code{Distribution}: The distribution type (required):
#'       \itemize{
#'         \item "UNIFORM_RANDOM" for random indices within a range
#'         \item A string "1" to "99" for fixed percentile values
#'         \item "BETA" (not yet implemented)
#'       }
#'     \item \code{Parameters}: Distribution-specific parameters (required):
#'       \itemize{
#'         \item For "UNIFORM_RANDOM": \code{min} and \code{max} integers defining the range
#'         \item For numeric distributions: A single integer value to repeat
#'       }
#'   }
#'
#' @return An integer vector with length equal to \code{nrow(wide_data)}:
#'   \itemize{
#'     \item For "UNIFORM_RANDOM": Random integers between \code{min} and \code{max} inclusive
#'     \item For numeric distributions: The specified integer repeated
#'     \item For "BETA": Error (not implemented)
#'   }
#'
#' @note This function is not exported and is intended for internal use only.
#' 
#' @importFrom dqrng dqsample.int
#' @keywords internal

`getGrowthDistributionIndices` <-
function(
    wide_data,
    growth.distribution
    ) {
        
        if (growth.distribution[["Distribution"]] == "UNIFORM_RANDOM") {
            if (!all(c("min", "max") %in% names(growth.distribution[["Parameters"]]))) {
                stop("UNIFORM_RANDOM distribution requires 'min' and 'max' parameters")
            }
            if (growth.distribution[["Parameters"]][["min"]] >= growth.distribution[["Parameters"]][["max"]]) {
                stop("'min' must be less than 'max' for UNIFORM_RANDOM distribution")
            }
            return(dqrng::dqsample.int(growth.distribution[["Parameters"]][["max"]] - growth.distribution[["Parameters"]][["min"]] + 1L, nrow(wide_data), replace = TRUE) + growth.distribution[["Parameters"]][["min"]] - 1L)
        }

        if (growth.distribution[["Distribution"]] %in% c(paste0("P", 1:99), as.character(1:99))) {

            return(rep(as.integer(gsub("P", "", growth.distribution[["Distribution"]])), nrow(wide_data)))
        }

        if (growth.distribution[["Distribution"]] == "BETA") {
            stop("Beta Distribution functionality not yet implemented.")
        }
    } ### END getGrowthDistributionIndices
