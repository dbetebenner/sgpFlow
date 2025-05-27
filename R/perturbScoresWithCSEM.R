#' @title Perturb Scores with CSEM
#' 
#' @description
#' This function is used to perturb scale scores with associated scale score conditional standard errors of measurement (CSEM) values.
#'
#' @param wide_data A data.table of wide data.
#' @param state A character value of the state.
#' @param sgpFlow.config A list of configuration parameters for the sgpFlow analysis being performed.
#' @param csem.perturbation.iterations Integer. Number of iterations of perturbed scores for the calculation of percentile trajectories.
#' @param csem.perturbation.distribution A character string specifying the distribution to use for CSEM perturbation. Options include `"NORMAL"`.
#' @param trajectory.type A character vector specifying the type of trajectory "rounding" to perform when calculating growth trajectories. Options include \code{"EXACT_VALUE"}, \code{"NEAREST_INTEGER_VALUE"}, and \code{"NEAREST_OBSERVED_VALUE"}. Default is \code{"EXACT_VALUE"}.
#' @returns A data.table extending the input wide_data with additional perturbed copies as additional rows.
#' 
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
        perturb.scale.scores <- function(scale.scores, csem.perturbation.distribution, csem.perturbation.iterations, csem.perturbation.function, trajectory.type, loss.hoss) {
            if (csem.perturbation.distribution == "NORMAL") {
                if (trajectory.type == "EXACT_VALUE") {
                    return(bound.scores(c(rep(scale.scores, csem.perturbation.iterations) + dqrng::dqrnorm(csem.perturbation.iterations * length(scale.scores)) * rep(csem.perturbation.function(scale.scores), csem.perturbation.iterations)), loss.hoss))
                }
                if (trajectory.type == "NEAREST_INTEGER_VALUE") {
                    return(bound.scores(Rfast::Round(c(rep(scale.scores, csem.perturbation.iterations) + dqrng::dqrnorm(csem.perturbation.iterations * length(scale.scores)) * rep(csem.perturbation.function(scale.scores), csem.perturbation.iterations))), loss.hoss))
                }
                if (trajectory.type == "NEAREST_OBSERVED_VALUE") {
                    ## TODO: Implement nearest observed value
                }
            }
        }

        ## Create data.table to hold all scores
        ## Add one iteration for unperturbed scores
        tmp.dt <- data.table::data.table(ID = rep(wide_data[["ID"]], csem.perturbation.iterations + 1L))

        ## Loop over grades in grade.progression
        for (grade.iter in seq_along(sgpFlow.config[["grade.progression"]])) {
            tmp.grade <- sgpFlow.config[["grade.progression"]][grade.iter]
            tmp.content_area <- sgpFlow.config[["content_area.progression"]][grade.iter]
            tmp.column.name <- paste0("SCALE_SCORE_GRADE_", tmp.grade)
            loss.hoss <- get.loss.hoss(state, tmp.content_area, tmp.grade)

            ## Add perturbed scores to data.table
            tmp.dt[, (tmp.column.name) :=
                c(wide_data[[tmp.column.name]],
                perturb.scale.scores(
                    scale.scores = wide_data[[tmp.column.name]],
                    csem.perturbation.distribution = csem.perturbation.distribution,
                    csem.perturbation.iterations = csem.perturbation.iterations,
                    csem.perturbation.function = sgpFlow::sgpFlowStateData[[state]][["Achievement"]][["CSEM"]][[tmp.content_area]][[paste("GRADE", tmp.grade, sep = "_")]],
                    trajectory.type = trajectory.type,
                    loss.hoss = loss.hoss)
                )]
        } ## END grade.iter loop

        ## Return unperturbed and perturbed values
        return(tmp.dt)
    } ### END perturbScoresWithCSEM
