#' @title Generate Growth Distribution Trajectory Indices
#'
#' @description
#' This internal function calculates indices based on the specified growth distribution. 
#' It supports "SGP_UNIFORM_RANDOM" for generating random indices within a range, numeric 
#' growth distributions (e.g., "SGP_1" to "SGP_99") for fixed percentiles, and includes a 
#' placeholder for "SGP_BETA" distribution (currently not implemented).
#'
#' @param wide_data A data.table containing student or entity data. The number of rows 
#'   determines how many indices to generate.
#' @param growth.distribution A list containing the growth distribution parameters:
#'   \itemize{
#'     \item \code{Distribution}: The distribution type (required):
#'       \itemize{
#'         \item "SGP_UNIFORM_RANDOM" for random indices within a range
#'         \item A string "SGP_1" to "SGP_99" for fixed percentile values
#'         \item "SGP_BETA" (not yet implemented)
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
#'     \item For "SGP_UNIFORM_RANDOM": Random integers between \code{min} and \code{max} inclusive
#'     \item For numeric distributions: The specified integer repeated
#'     \item For "SGP_BETA": Error (not implemented)
#'   }
#'
#' @importFrom dqrng dqsample.int
#' @rdname getGrowthDistributionIndices
#' @keywords internal

getGrowthDistributionIndices <-
    function(
        wide_data,
        growth.distribution
    ) {
        
        if (growth.distribution[["Distribution"]] == "SGP_UNIFORM_RANDOM") {
            if (!all(c("min", "max") %in% names(growth.distribution[["Parameters"]]))) {
                stop("SGP_UNIFORM_RANDOM distribution requires 'min' and 'max' parameters")
            }
            if (growth.distribution[["Parameters"]][["min"]] >= growth.distribution[["Parameters"]][["max"]]) {
                stop("'min' must be less than 'max' for SGP_UNIFORM_RANDOM distribution")
            }
            return(dqrng::dqsample.int(growth.distribution[["Parameters"]][["max"]] - growth.distribution[["Parameters"]][["min"]] + 1L, nrow(wide_data), replace = TRUE) + growth.distribution[["Parameters"]][["min"]] - 1L)
        }

        if (growth.distribution[["Distribution"]] %in% c(paste0("SGP_", 1:99))) {

            return(rep.int(as.integer(gsub("SGP_", "", growth.distribution[["Distribution"]])), collapse::fnrow(wide_data)))
        }

        if (growth.distribution[["Distribution"]] == "SGP_BETA") {
            stop("Beta Distribution functionality not yet implemented.")
        }
    } ### END getGrowthDistributionIndices
