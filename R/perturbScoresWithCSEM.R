#' @title Perturb Scores with CSEM
#' 
#' @description
#' This function is used to perturb scale scores with associated scale score conditional standard errors of measurement (CSEM) values.
#'
#' @param wide_data A data.table of wide data.
#' @param state A character value of the state.
#' @param sgpFlow.config A list of configuration parameters for the sgpFlow package.
#' @param csem.perturbation.iterations Integer. Number of iterations for perturbing scores and calculating trajectories.
#' @param csem.perturbation.distribution A character string specifying the distribution to use for CSEM perturbation. Options include `"NORMAL"`.
#' @param trajectory.type A character vector specifying the type of trajectory "rounding" to perform when calculating growth trajectories. Options include \code{"EXACT_VALUE"}, \code{"NEAREST_INTEGER_VALUE"}, and \code{"NEAREST_OBSERVED_VALUE"}. Default is \code{"EXACT_VALUE"}.
#' @importFrom data.table data.table
#' @importFrom dqrng dqrnorm
#' @importFrom Rfast Round
#'
#' @rdname perturbScoresWithCSEM
#' @keywords internal

perturbScoresWithCSEM <-
    function(
        wide_data,
        state,
        sgpFlow.config,
        csem.perturbation.iterations,
        csem.perturbation.distribution,
        trajectory.type
    ) {

        ## If no iterations, return unperturbed scores
        if (csem.perturbation.iterations == 0L) return(wide_data)

        ## Utility functions
        perturb.scale.scores <- function(scale.scores, csem.perturbation.distribution, csem.perturbation.iterations, csem.perturbation.function, trajectory.type) {
            if (csem.perturbation.distribution == "NORMAL") {
                if (trajectory.type == "EXACT_VALUE") {
                    return(c(rep(scale.scores, csem.perturbation.iterations) + dqrng::dqrnorm(csem.perturbation.iterations) * rep(csem.perturbation.function(scale.scores), csem.perturbation.iterations)))
                }
                if (trajectory.type == "NEAREST_INTEGER_VALUE") {
                    return(Rfast::Round(c(rep(scale.scores, csem.perturbation.iterations) + dqrng::dqrnorm(csem.perturbation.iterations) * rep(csem.perturbation.function(scale.scores), csem.perturbation.iterations))))
                }
                if (trajectory.type == "NEAREST_OBSERVED_VALUE") {
                    ## TODO: Implement nearest observed value
                }
            }
        }

        ## Create data.table to hold all scores
        tmp.dt <- data.table::data.table(ID = rep(wide_data[["ID"]], csem.perturbation.iterations))

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

            ## Add perturbed scores to data.table
            tmp.dt[, (tmp.column.name) :=
                c(wide_data[[tmp.column.name]],
                perturb.scale.scores(
                    scale.scores = wide_data[[tmp.column.name]],
                    csem.perturbation.distribution = csem.perturbation.distribution,
                    csem.perturbation.iterations = csem.perturbation.iterations,
                    csem.perturbation.function = sgpFlow::sgpFlowStateData[[state]][["Achievement"]][["CSEM"]][[tmp.content_area]][[paste("GRADE", tmp.grade, sep = "_")]],
                    trajectory.type = trajectory.type)
                )]

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
