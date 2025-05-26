#' @title Generate SGP Trajectories for Cohorts
#' @description Computes student growth percentile (SGP) trajectories for cohorts of students using longitudinal data,
#' state-specific configurations, and projection spline matrices.
#' 
#' This function processes longitudinal student data to generate percentile trajectories over time,
#' integrating cohort-specific configurations, growth distributions, and optional perturbations based
#' on conditional standard error of measurement (CSEM).
#' 
#' @param long_data A `data.table` in long format containing student data. Required columns typically include `YEAR`, `GRADE`, `CONTENT_AREA`, `ID`, and scale scores.
#' @param state A character string indicating the state for which trajectories are computed, used for state-specific configurations in the `sgpFlow` package.
#' @param sgpFlow.config A list of configurations for SGP analysis, including grade progressions, content areas, and metadata.
#' @param cohort.end.year Integer or `NULL`. The end year of the cohort for which percentile trajectories are generated. If `NULL`, the latest year in the data is used. Default: `NULL`.
#' @param growth.distribution A character vector specifying the growth distribution for projecting scores. Options include `"UNIFORM_RANDOM"`, `"BETA"`, or percentile values (`"1"` through `"99"`). Default: `NULL`.
#' @param trajectory.type A character vector specifying the type of trajectory "rounding" to perform when calculating growth trajectories. Options include \code{"EXACT_VALUE"}, \code{"NEAREST_INTEGER_VALUE"}, and \code{"NEAREST_OBSERVED_VALUE"}. Default is \code{"EXACT_VALUE"}.
#' @param csem.perturbation.of.initial.scores Logical. If `TRUE`, perturbs initial scale scores using CSEM to introduce variability in simulations. Default: `TRUE`.
#' @param csem.perturbation.iterations Integer. Number of iterations for perturbing scores and calculating trajectories. Default: `100`.
#' @param achievement.percentiles.tables Logical. If `TRUE`, creates tables of 99 trajectories tables based upon initial achievement percentiles.
#' @param projection.splineMatrices A list of projection spline matrices used to model growth percentiles over time.
#' @returns A list of `data.table` objects, where each element represents the results of one simulation iteration. Each `data.table` contains student IDs and their projected scale scores at different percentiles.
#' @details 
#' - Converts `long_data` to wide format using \code{\link[sgpFlow]{getWideData}}, tailored to the specified `cohort.end.year` and `sgpFlow.config`.
#' - Integrates projection spline matrices using \code{getGradeProjectionSequenceMatrices}.
#' - Delegates percentile trajectory calculations to \code{\link[sgpFlow]{getPercentileTrajectories}}, which handles CSEM perturbations and growth distribution sequences.
#' 
#' The function ensures seamless computation of percentile trajectories by chaining key steps, enabling detailed cohort-specific growth analysis.
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'   # Example usage
#'   trajectories <- sgpFlowTrajectories(
#'     long_data = student_long_data,
#'     state = "NY",
#'     sgpFlow.config = sgp_config_list,
#'     cohort.end.year = 2023,
#'     growth.distribution = "UNIFORM_RANDOM",
#'     csem.perturbation.iterations = 50,
#'     projection.splineMatrices = spline_matrices_list
#'   )
#'   
#'   # Access the first iteration results
#'   print(trajectories[[1]])
#' }
#' }
#' @seealso 
#'  \code{\link[sgpFlow]{getPercentileTrajectories}}, 
#'  \code{\link[sgpFlow]{getWideData}}, 
#' @rdname sgpFlowTrajectories
#' @export

sgpFlowTrajectories <-
    function(
        long_data,
        state,
        sgpFlow.config,
        cohort.end.year = NULL,
        growth.distribution = NULL,
        trajectory.type = "EXACT_VALUE",
        csem.perturbation.of.initial.scores = TRUE,
        csem.perturbation.iterations = 100L,
        achievement.percentiles.tables,
        projection.splineMatrices
    ) {

        ## Calculate percentile trajectories
        sgpFlow.trajectories <-
            getPercentileTrajectories(
                wide_data = # Subset and reshape to wide data for getPercentileTrajectories
                    getWideData(
                        long_data = long_data,
                        sgpFlow.config = sgpFlow.config,
                        cohort.end.year = cohort.end.year,
                        achievement.percentiles.tables = achievement.percentiles.tables
                    ),
                state = state,
                sgpFlow.config = sgpFlow.config,
                growth.distribution = growth.distribution,
                trajectory.type = trajectory.type,
                csem.perturbation.of.initial.scores = csem.perturbation.of.initial.scores,
                csem.perturbation.iterations = csem.perturbation.iterations,
                achievement.percentiles.tables = achievement.percentiles.tables,
                projection.splineMatrices = # Get matrix sequence associated with trajectory calculations
                    getGradeProjectionSequenceMatrices(
                        sgpFlow.config,
                        projection.splineMatrices
                    )
            )

        ## Return trajectories
        return(sgpFlow.trajectories)
    } ### END sgpFlowTrajectories
