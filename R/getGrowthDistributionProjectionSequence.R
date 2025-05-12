#' Get Growth Distribution Projection Sequence
#' 
#' The function get.growth.distribution.projection.sequence takes the growth.distribution, validates it, and
#' produces a list which exactly specifies the way in which growth is assigned to student trajectories
#' The argument growth.distribution is specified either as a single argument for use with ALL years of the projection sequence or each year specifically.
#' If only one year is specified in the growth distribution, then that distribution is utilized across each year of the projection. 
#' Each element of the growth.distribution argument is a character string indicating the distribution from which to sample (using default parameters for the distribution)
#' or a list containing the following components:
#'      Distribution: (Required) The distribution associated with the growth distribution (either a theoretical distribution (e.g., UNIFORM_RANDOM) or a variable name (e.g., SGP_BASELINE) indicating the values to sample from.)
#'      Parameters: (Optional) Distribution specific parameters, (formatted as a list) to pass to the distribution being used (e.g., min, max of the UNIFORM_RANDOM distribution)
#'      Subgroups: (Optional) Dataset variable indicating subgroup to apply growth distribution by (e.g., SCHOOL_NUMBER)
#'
#' @param growth.distribution A list or character string specifying the growth distribution(s) to use:
#'   \itemize{
#'     \item If a single distribution is provided, it will be used for all projection years
#'     \item If multiple distributions are provided, they should match the number of projection years
#'     \item Each distribution element can be either:
#'       \itemize{
#'         \item A character string indicating the distribution (e.g., "UNIFORM_RANDOM", "SGP_BASELINE")
#'         \item A list with components:
#'           \itemize{
#'             \item Distribution: (Required) The distribution type or variable name
#'             \item Parameters: (Optional) Distribution-specific parameters as a list
#'             \item Subgroups: (Optional) Variable name for subgroup analysis
#'           }
#'       }
#'   }
#' @param years.projected Integer indicating the number of years to project growth distributions
#'
#' @return A list containing validated growth distribution specifications for each projection year:
#'   \itemize{
#'     \item Distribution: The specified distribution type
#'     \item Parameters: Distribution parameters (with defaults if not specified)
#'     \item Subgroups: Subgroup specifications (if provided)
#'   }
#'
#' @examples
#' \dontrun{
#' # Single distribution for all years
#' dist1 <- getGrowthDistributionProjectionSequence("UNIFORM_RANDOM", 3)
#'
#' # Custom distribution with parameters
#' dist2 <- getGrowthDistributionProjectionSequence(
#'   list(Distribution = "UNIFORM_RANDOM",
#'        Parameters = list(min = 10, max = 90)),
#'   2)
#'
#' # Multiple distributions by year
#' dist3 <- getGrowthDistributionProjectionSequence(
#'   list(
#'     list(Distribution = "UNIFORM_RANDOM"),
#'     list(Distribution = "SGP_BASELINE",
#'           Subgroups = "SCHOOL_NUMBER")
#'   ),
#'   2)
#' }
#' 
#' @note This function is not exported and is intended for internal use only.
#' @keywords internal

getGrowthDistributionProjectionSequence <-
    function(
        growth.distribution,
        years.projected
    ) {

        # Utility functions
        check.growth.distribution.distribution <- function(growth.distribution.list) {
            # Validate Distribution component of growth.distribution.list
            distribution.names <- sapply(growth.distribution.list, function(x) x[["Distribution"]])
            if (!all(unique(distribution.names) %in% supported_distributions)) {
                stop(sprintf(
                    "Unsupported 'growth.distribution' supplied: Currently supported distributions are: %s",
                    paste(sapply(supported_distributions.short, capWords), collapse = ", ")
                ))
            }
            return(growth.distribution.list)
        } ### END check.growth.distribution.distribution

        check.growth.distribution.parameters <- function(parameters) {
            if (parameters[["Distribution"]] == "UNIFORM_RANDOM") {
                if (is.null(parameters[["Parameters"]])) {
                    parameters[["Parameters"]] <- list(min = 1L, max = 99L)
                } else {
                    if (all(c("min", "max") %in% names(parameters[["Parameters"]]))) stop("Please supply integer min/max Value between 1 & 99 for UNIFORM_RANDOM growth.distribution.")
                }
            }
            if (parameters[["Distribution"]] == "BETA") {
                if (is.null(parameters[["Parameters"]])) {
                    parameters[["Parameters"]] <- list(shape1.min.max = c(3, 7), shape2.min.max = c(3, 7), interpolated.min.value = 0)
                } else {
                    if (all(c("shape1.min.max", "shape2.min.max", "interpolated.min.value") %in% names(parameters[["Parameters"]]))) {
                        stop("Please supply 'shape1.min.max', 'shape2.min.max', and 'interpolated.min.value' for BETA growth.distribution.")
                    }
                }
            }
            return(parameters)
        } ### END check.growth.distribution.parameters

        check.growth.distribution.subgroups <- function(subgroups) {
            ### NULL subgroups[["Subgroups"]] implies ALL_STUDENTS
            ### No other checks on subgroups currently implemented
            return(subgroups)
        }

        ## Parameters
        supported_distributions <- c("UNIFORM_RANDOM", "BETA", paste0("P", 1:99), as.character(1:99))
        supported_distributions.short <- c("UNIFORM_RANDOM", "BETA", "P**", "**")

        ## Validate growth.distribution argument
        growth.distribution <- toupper(ifelse(is.null(growth.distribution), "UNIFORM_RANDOM", gsub("[ -]", "", growth.distribution)))
        if (!is.list(growth.distribution) && !is.character(growth.distribution)) stop("Supplied growth.distribution must be either of class 'character' or 'list'.")
        
        ## Validate length of growth.distribution
        if (!length(growth.distribution) %in% c(1L, years.projected)) {
            stop(sprintf(
                "Length of supplied 'growth.distribution' must be either 1 or %d.",
                years.projected
            ))
        }

        # Convert growth.distribution to growth.distribution.list
        if (length(growth.distribution)==1 && is.list(growth.distribution)) growth.distribution.list <- rep(growth.distribution, years.projected)
        if (length(growth.distribution)==1 && is.character(growth.distribution)) {
            growth.distribution.list <- vector("list", length = years.projected)
            growth.distribution.list <- lapply(rep(growth.distribution, years.projected), function(name) list(Distribution = name))
        }

        # Validate Distribution component of growth.distribution.list 
        growth.distribution.list <- check.growth.distribution.distribution(growth.distribution.list)

        # Validate Parameters component of growth.distribution.list
        growth.distribution.list <- lapply(growth.distribution.list, function(x) check.growth.distribution.parameters(x))

        # Validate Subgroups component of growth.distribution.list
        growth.distribution.list <- lapply(growth.distribution.list, function(x) check.growth.distribution.subgroups(x))

        return(growth.distribution.list)
    } ### END getGrowthDistributionProjectionSequence