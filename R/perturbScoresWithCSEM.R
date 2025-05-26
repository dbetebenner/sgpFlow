#' @title Perturb Scores with CSEM
#' 
#' @description
#' This function is used to perturb scale scores with associated scale score conditional standard errors of measurement (CSEM) values.
#'
#' @param wide_data A data.table of wide data.
#' @param state A character value of the state.
#' @param sgpFlow.config A list of configuration parameters for the sgpFlow package.
#' @importFrom data.table data.table
#' @importFrom stats rnorm
#' 
#' @rdname perturbScoresWithCSEM
#' @keywords internal

perturbScoresWithCSEM <-
    function(
        wide_data,
        state,
        sgpFlow.config,
        csem.distribution = "Normal",
        csem.perturbation.iterations = 100L,
        trajectory.type
    ) {
        ## Utility functions
        perturb.rnorm <- function(mean = 0, sd) {
            dt <-
                data.table::data.table(sd = sd, VALUE = as.numeric(NA))[!is.na(sd), VALUE := stats::rnorm(.N, mean, sd)]
            return(dt[["VALUE"]])
        }

        ## Parameters
        csem.distribution <- toupper(csem.distribution)
        supported.distributions <- c("NORMAL")

        ## Create data.table to hold all scores 
        tmp.dt <- data.table::data.table()

        ## Define relevant variables
        if (!csem.distribution %in% supported.distributions) {
            stop(paste0("Distribution supplied (", csem.distribution, ") not currently supported."))
        }

        ## Loop over grades in grade.progression
        for (grade.iter in seq_along(sgpFlow.config[["grade.progression"]])) {
            tmp.grade <- sgpFlow.config[["grade.progression"]][grade.iter]
            tmp.content_area <- sgpFlow.config[["content_area.progression"]][grade.iter]
            tmp.column.name <- paste0("SCALE_SCORE_GRADE_", tmp.grade)

            if (!is.null(state)) {
                loss.hoss <- get.loss.hoss(state, tmp.content_area, tmp.grade)
            } else {
                loss.hoss <- range(wide_data[[tmp.column.name]], na.rm = TRUE)
            }

            if (csem.distribution == "NORMAL") {
                tmp.dt[, (tmp.column.name) :=
                    c(wide_data[[tmp.column.name]],
                    replicate(csem.perturbation.iterations, wide_data[[tmp.column.name]] + perturb.rnorm(sd = sgpFlow::sgpFlowStateData[[state]][["Achievement"]][["CSEM"]][[tmp.content_area]][[paste("GRADE", tmp.grade, sep = "_")]](wide_data[[tmp.column.name]]))))]
            }

            ## Pull in scores to loss and hoss
            tmp.dt[
                get(tmp.column.name) < loss.hoss[1L], (tmp.column.name) := loss.hoss[1L]
            ][get(tmp.column.name) > loss.hoss[2L], (tmp.column.name) := loss.hoss[2L]]
        }

        ## Return unperturbed and perturbed values
        ## Depending upon trajectory.type 
        if (trajectory.type == "EXACT_VALUE") {
            return(tmp.dt)
        }

        if (trajectory.type == "NEAREST_INTEGER_VALUE") {
            return(round(tmp.dt))
        }

        if (trajectory.type == "NEAREST_OBSERVED_VALUE") {
            stop("NEAREST_OBSERVED_VALUE not currently supported.")
        }
    } ### END perturbScoresWithCSEM
