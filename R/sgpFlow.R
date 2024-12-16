#' sgpFlow: Create sgpFlow trajectories 
#'
#' The `sgpFlow` function facilitates the analysis of sgpFlow trajectories using various cohort data types, such as traditional cohorts, super-cohorts, or achievement percentiles. It offers the flexibility to handle conditional standard error measurement (CSEM) perturbation, growth projection matrices, and other configurations.
#'
#' @param sgp_object An object of class \code{"SGP"} or \code{"data.table"}. If of class \code{"SGP"}, the `@Data` slot is used; otherwise, the supplied \code{data.table} is used directly.
#' @param state A character string indicating the state for which the analysis is conducted. This must match a state entry in \code{sgpFlowStateData}.
#' @param sgpFlow.config A configuration list for the sgpFlow analysis, containing details such as grade progression, year lags, and content area progression.
#' @param superCohort.config A configuration list specific to super-cohort analysis. If \code{NULL}, super-cohort analysis is skipped.
#' @param cohort.data.type A character vector specifying the type of cohort data to analyze. Options include \code{"SUPER_COHORT"}, \code{"COHORT"}, and \code{"ACHIEVEMENT_PERCENTILES"}. Default is \code{"COHORT"}.
#' @param csem.perturbation.of.initial.scores Logical. If \code{TRUE}, initial scores are perturbed using conditional standard error measurement (CSEM). Default is \code{TRUE}.
#' @param csem.perturbation.iterations Integer. Number of iterations for CSEM perturbation. Default is \code{100L}.
#' @param iterate.without.csem.perturbation Logical. If `TRUE`, performs CSEM iterations without perturbing score to derive 100 simulated trajectories from single (non-perturbed) initial score.
#' @param achievement.percentiles.tables Logical. Indicating whether subset based upon the achievement percentile is performed (99 resulting rows) 
#' @param projection.splineMatrices A list of projection spline matrices used for calculating growth trajectories.
#'
#' @details
#' The `sgpFlow` function loops over specified \code{cohort.data.type} values, performing SGP trajectory analysis for each type. It supports super-cohort analysis when a valid \code{superCohort.config} is provided. If \code{csem.perturbation.of.initial.scores} is enabled, the function applies random perturbations to initial scores to account for measurement error, with the number of iterations controlled by \code{csem.perturbation.iterations}.
#'
#' The function requires valid projection spline matrices, provided through \code{projection.splineMatrices}, to perform growth trajectory calculations.
#'
#' @return A list containing sgpFlow analysis results for each specified \code{cohort.data.type}.
#'
#' @examples
#' \dontrun{
#' # Example usage with an SGP object
#' results <- sgpFlow(
#'   sgp_object = sgp_object,
#'   state = "DEMO",
#'   sgpFlow.config = sgpFlow.config,
#'   superCohort.config = superCohort.config,
#'   cohort.data.type = c("COHORT", "SUPER_COHORT"),
#'   csem.perturbation.of.initial.scores = TRUE,
#'   csem.perturbation.iterations = 100L,
#'   projection.splineMatrices = projection.splineMatrices
#' )
#' }
#'
#' @export

sgpFlow <- 
    function(
        sgp_object,
        state,
        sgpFlow.config,
        superCohort.config=NULL,
        cohort.data.type = "SINGLE_COHORT", #c("SUPER_COHORT", "SINGLE_COHORT"),
        csem.perturbation.of.initial.scores = TRUE,
        csem.perturbation.iterations = 100L,
        iterate.without.csem.perturbation = FALSE,
        achievement.percentiles.tables = TRUE,
        projection.splineMatrices
    ) {

    # Test/update arguments
    if (!any(class(sgp_object) %in% c("SGP", "data.table"))) stop("Supplied sgp_object must be either of class 'SGP' or 'data.table'.")

    if (is.null(superCohort.config) & "SUPER_COHORT" %in% cohort.data.type) {
        print("Super-cohort data analysis requires non-null superCohort.config.\nProceeding with analyses without super-cohort cohort.data.type.")
        cohort.data.type <- setdiff(cohort.data.type, "SUPER_COHORT")
    }

    if (achievement.percentiles.tables) achievement.percentiles.tables <- c(FALSE, TRUE)

    # Get long_data
    if ("SGP" %in% class(sgp_object)) long_data <- sgp_object@Data else long_data <- sgp_object

    # Add SCALE_SCORE_STANDARDIZED to long_data
    long_data[VALID_CASE=="VALID_CASE", SCALE_SCORE_STANDARDIZED := fscale(SCALE_SCORE, na.rm=TRUE), by=c("YEAR", "CONTENT_AREA", "GRADE")] 


    # Initialize an empty list to store results
    sgpFlow_results_list <- list()

    # Loop over sgpFlow.config 
    for (sgpFlow.config.iter in sgpFlow.config) {
        tmp_name <- paste(toupper(tail(sgpFlow.config.iter[['content_area.progression']], 1)), "GRADE", tail(sgpFlow.config.iter[['grade.progression']], 1), sep="_")

        # Loop over cohort.data.type
        for (data.type.iter in cohort.data.type) {
#            tmp.long_data <- prepareLongData(long_data, data.type.iter)

            # Loop over growth.distribution
            for (growth.distributions.iter in sgpFlow.config.iter[['growth.distributions']]) {

                # Loop over whether achievement.percentiles.tables get calculated.
                for (achievement.percentiles.tables.iter in achievement.percentiles.tables) {
                    sgpFlow_results_list[[tmp_name]][[data.type.iter]][[growth.distributions.iter]] <- 
                        sgpFlowTrajectories(
                            long_data = long_data,
                            state = state,
                            sgpFlow.config = sgpFlow.config.iter,
                            growth.distribution = growth.distributions.iter,
                            csem.perturbation.of.initial.scores = csem.perturbation.of.initial.scores,
                            csem.perturbation.iterations = csem.perturbation.iterations,
                            iterate.without.csem.perturbation = iterate.without.csem.perturbation,
                            achievement.percentiles.tables = achievement.percentiles.tables.iter,
                            projection.splineMatrices = projection.splineMatrices[[paste(tail(sgpFlow.config.iter[['content_area.progression']], 1), "BASELINE", sep=".")]])
                } ### END achievement.percentiles.iter
            } ### END growth.distributions.iter
        } ### END data.type.iter
    } ### END sgpFlow.config.iter

    return(sgpFlow_results_list)
} ### END sgpFlow