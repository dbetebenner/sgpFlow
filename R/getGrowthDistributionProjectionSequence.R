#' The function get.growth.distribution.projection.sequence takes the growth.distribution, validates it, and
#' produces a list which exactly specifies the way in which growth is assigned to student trajectories
#' The argument growth.distribution is specified either as a single argument for use with ALL years of the projection sequence or each year specifically.
#' If only one year is specified in the growth distribution, then that distribution is utilized across each year of the projection. 
#' Each element of the growth.distribution argument is a character string indicating the distribution from which to sample (using default parameters for the distribution)
#' or a list containing the following components:
#'      Distribution: (Required) The distribution associated with the growth distribution (either a theoretical distribution (e.g., UNIFORM_RANDOM) or a variable name (e.g., SGP_BASELINE) indicating the values to sample from.)
#'      Parameters: (Optional) Distribution specific parameters, (formatted as a list) to pass to the distribution being used (e.g., min, max of the UNIFORM_RANDOM distribution)
#'      Groupings: (Optional) Dataset variable indicating subgroup to apply growth distribution by (e.g., SCHOOL_NUMBER)

`getGrowthDistributionProjectionSequence` <- 
function(
    growth.distribution,
    years.projected)
{

    # Utility functions
    check.growth.distribution.distribution <- function(growth.distribution.list) {
        # Validate Distribution component of growth.distribution.list
        distribution.names <- sapply(growth.distribution.list, function(x) x[['Distribution']])
        if (!all(unique(distribution.names) %in% supported_distributions)) {
            stop(sprintf(
                "Unsupported 'growth.distribution' supplied: Currently supported distributions are: %s",
                paste(sapply(supported_distributions.short, capWords), collapse = ", ")
            ))
        }

        # Convert Distribution P** to ** and return
        lapply(growth.distribution.list, function(x) {
            x[['Distribution']] <- gsub("^P(\\d+)$", "\\1", x[['Distribution']])
            x
        })
    }

    check.growth.distribution.parameters <- function(parameters) {
        if (parameters[['Distribution']]=="UNIFORM_RANDOM") {
            if (is.null(parameters[['Parameters']])) {
                parameters[['Parameters']] <- list(min=1L, max=99L)
            } else {
                if (all(c("min", "max") %in% names(parameters[['Parameters']]))) stop("Please supply integer min/max Value between 1 & 99 for UNIFORM_RANDOM growth.distribution.")
            }
        }
        if (parameters[['Distribution']]=="BETA") {
            if (is.null(parameters[['Parameters']])) {
                parameters[['Parameters']] <- list(shape1.min.max=c(3,7), shape2.min.max=c(3,7), interpolated.min.value=0)
            } else {
                if (all(c("shape1.min.max", "shape2.min.max", "interpolated.min.value") %in% names(parameters[['Parameters']]))) {
                    stop("Please supply 'shape1.min.max', 'shape2.min.max', and 'interpolated.min.value' for BETA growth.distribution.")
                }
            }
        }
        return(parameters)
    }

    check.growth.distribution.subgroups <- function(subgroups) {
        ### NULL subgroups[['Subgroups']] implies ALL_STUDENTS
        ### No other checks on subgroups currently implemented
        return(subgroups)
    }

    ## Parameters
    supported_distributions <- c("UNIFORM_RANDOM", "BETA", paste0("P", 1:99), as.character(1:99))
    supported_distributions.short <- c("UNIFORM_RANDOM", "BETA", "P**", "**")

    ## Validate growth.distribution argument
    growth.distribution <- toupper(ifelse(is.null(growth.distribution), "UNIFORM_RANDOM", growth.distribution))
    if (!is.list(growth.distribution) && !is.character(growth.distribution)) stop("Supplied growth.distribution must be either of class 'character' or 'list'.")

    ## Validate length of growth.distribution
    if (!length(growth.distribution) %in% c(1, years.projected)) {
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