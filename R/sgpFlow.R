#' @title sgpFlow: Create sgpFlow trajectories
#'
#' @description
#' The `sgpFlow` function facilitates the analysis of sgpFlow trajectories using various cohort data types,
#' such as traditional cohorts, super-cohorts, or achievement percentiles. It offers the flexibility to handle
#' conditional standard error measurement (CSEM) perturbation, growth projection matrices, and other configurations.
#'
#' @param sgp_object An object of class \code{"SGP"} or \code{"data.table"}. If of class \code{"SGP"}, the `@Data` slot is used; otherwise, the supplied \code{data.table} is used directly.
#' @param state A character string indicating the state for which the analysis is conducted. This must match a state entry in \code{sgpFlowStateData}.
#' @param sgpFlow.config A configuration list for the sgpFlow analysis, containing details such as grade progression, year lags, and content area progression.
#' @param superCohort.config A configuration list specific to super-cohort analysis. If \code{NULL}, super-cohort analysis is skipped.
#' @param cohort.data.type A character vector specifying the type of cohort data to analyze. Options include \code{"SUPER_COHORT"}, \code{"COHORT"}, and \code{"ACHIEVEMENT_PERCENTILES"}. Default is \code{"COHORT"}.
#' @param trajectory.type A character vector specifying the type of trajectory "rounding" to perform when calculating growth trajectories. Options include \code{"EXACT_VALUE"}, \code{"NEAREST_INTEGER_VALUE"}, and \code{"NEAREST_OBSERVED_VALUE"}. Default is \code{"EXACT_VALUE"}.
#' @param csem.perturbation.of.initial.scores Logical. If \code{TRUE}, initial scores are perturbed using conditional standard error measurement (CSEM). Default is \code{TRUE}.
#' @param csem.perturbation.iterations Integer. Number of iterations for CSEM perturbation. Default is \code{100L}.
#' @param csem.perturbation.distribution A character string specifying the distribution to use for CSEM perturbation. Options include `"NORMAL"`. Default: `"NORMAL"`.
#' @param export.duckdb Logical. If `TRUE`, exports the aggregated results to a DuckDB database.
#' @param export.Rdata Logical. If `TRUE`, exports the sgpFlow results to an Rdata file.
#' @param projection.splineMatrices A list of projection spline matrices used for calculating growth trajectories.
#' @param parallel.config A list of configuration parameters for parallel processing. Default: `list(WORKERS=parallel::detectCores()-1)`.
#'
#' @details
#' The `sgpFlow` function loops over specified \code{cohort.data.type} values, performing SGP trajectory analysis for each type. It supports super-cohort analysis when a valid \code{superCohort.config} is provided. If \code{csem.perturbation.of.initial.scores} is enabled, the function applies random perturbations to initial scores to account for measurement error, with the number of iterations controlled by \code{csem.perturbation.iterations}.
#'
#' The function requires valid projection spline matrices (usually embedded in the sgpFlowMatrices package), provided through \code{projection.splineMatrices}, to perform growth trajectory calculations.
#'
#' @return A list containing sgpFlow analysis results for each specified \code{cohort.data.type}.
#'
#' @examples
#' \dontrun{
#' # Example usage with an SGP object
#' Demonstration_sgpFlow <- sgpFlow(
#'   sgp_object = sgpFlowData::sgpFlowData_LONG,
#'   sgpFlow.config = sgpFlow.config,
#'   superCohort.config = superCohort.config,
#'   cohort.data.type = c("COHORT", "SUPER_COHORT"),
#'   csem.perturbation.of.initial.scores = TRUE,
#'   csem.perturbation.iterations = 100L,
#'   csem.perturbation.distribution = "NORMAL",
#'   projection.splineMatrices = sgpFlowMatrices::sgpFlowMatrices[["DEMO_sgpFlowMatrices"]][["2024_2025"]][["SUPER_COHORT"]]
#' )
#' }
#'
#' @importFrom collapse fscale
#' @importFrom duckdb duckdb
#' @importFrom parallel clusterEvalQ clusterExport detectCores makeCluster parLapply stopCluster
#' @importFrom future plan multisession
#' @importFrom future.apply future_lapply
#' @rdname sgpFlow
#' @export

sgpFlow <- 
    function(
        sgp_object,
        state=NULL,
        sgpFlow.config,
        superCohort.config=NULL,
        cohort.data.type = "SINGLE_COHORT", ## Later to include "SUPER_COHORT"
        trajectory.type = c("EXACT_VALUE", "NEAREST_INTEGER_VALUE"), ## Later to include "NEAREST_OBSERVED_VALUE"
        csem.perturbation.of.initial.scores = TRUE,
        csem.perturbation.iterations = 100L,
        csem.perturbation.distribution = "NORMAL",
        export.duckdb = TRUE,
        export.Rdata = TRUE,
        projection.splineMatrices,
        parallel.config = list(WORKERS=parallel::detectCores()-1)
    ) {

    ### Test/update arguments
    if (!any(class(sgp_object) %in% c("SGP", "data.table"))) stop("Supplied sgp_object must be either of class 'SGP' or 'data.table'.")

    if (is.null(superCohort.config) & "SUPER_COHORT" %in% cohort.data.type) {
        print("Super-cohort data analysis requires non-null superCohort.config.\nProceeding with analyses without super-cohort cohort.data.type.")
        cohort.data.type <- setdiff(cohort.data.type, "SUPER_COHORT")
    }

    if (is.null(state)) {
        tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
        state <- getStateAbbreviation(tmp.name, "sgpFlow")
    }

    csem.perturbation.distribution <- toupper(csem.perturbation.distribution)
    supported.distributions <- c("NORMAL")
    if (!csem.perturbation.distribution %in% supported.distributions) {
        stop(paste0("Distribution supplied (", csem.perturbation.distribution, ") not currently supported."))
    }

    ## Extract long_data from sgp_object if it is an SGP object
    if ("SGP" %in% class(sgp_object)) long_data <- sgp_object@Data else long_data <- sgp_object

    ## Add SCALE_SCORE_STANDARDIZED to long_data
    long_data[VALID_CASE=="VALID_CASE", SCALE_SCORE_STANDARDIZED := collapse::fscale(SCALE_SCORE, na.rm=TRUE), by=c("YEAR", "CONTENT_AREA", "GRADE")] 

    ## Initialize an empty list to store results
    sgpFlow_results_list <- list()

    ## Loop over sgpFlow.config
    for (sgpFlow.config.iter in sgpFlow.config) {
        # Loop over cohort.data.type 
        for (cohort.type.iter in cohort.data.type) {
            tmp_name <- paste(toupper(tail(sgpFlow.config.iter[["content_area.progression"]], 1)), paste("GRADE", paste(sgpFlow.config.iter[["grade.progression"]], collapse=""), sep="_"), sep="__")

            # Parallel processing of combinations
            if (!is.null(parallel.config[["WORKERS"]])) {
                # Setup parallel backend
                future::plan(future::multisession, workers = parallel.config[["WORKERS"]])

                # Create combinations for parallel processing
                combinations <- expand.grid(
                    growth.distribution = sgpFlow.config.iter[["growth.distributions"]],
                    trajectory.type = trajectory.type,
                    stringsAsFactors = FALSE
                )

                results <- future.apply::future_lapply(seq_len(nrow(combinations)), future.seed = TRUE, function(i) {
                    growth.distributions.iter <- combinations[['growth.distribution']][i]
                    trajectory.type.iter <- combinations[['trajectory.type']][i]
                    
                    sgpFlowTrajectories(
                        long_data = long_data,
                        state = state,
                        sgpFlow.config = sgpFlow.config.iter,
                        growth.distribution = growth.distributions.iter,
                        trajectory.type = trajectory.type.iter,
                        csem.perturbation.of.initial.scores = csem.perturbation.of.initial.scores,
                        csem.perturbation.iterations = csem.perturbation.iterations,
                        csem.perturbation.distribution = csem.perturbation.distribution,
                        projection.splineMatrices = projection.splineMatrices[[paste(tail(sgpFlow.config.iter[["content_area.progression"]], 1), "BASELINE", sep=".")]]
                    )
                })
                
                # Assign results back to the list structure
                for (i in seq_len(nrow(combinations))) {
                    growth.distributions.iter <- as.character(combinations[['growth.distribution']][i])
                    trajectory.type.iter <- combinations[['trajectory.type']][i]
                    sgpFlow_results_list[[tmp_name]][[cohort.type.iter]][[growth.distributions.iter]][[trajectory.type.iter]] <- results[[i]]
                }
            } else { # Sequential processing if no parallel config
                # Loop over growth.distributions
                for (growth.distributions.iter in sgpFlow.config.iter[["growth.distributions"]]) {
                    for (trajectory.type.iter in trajectory.type) {
                        sgpFlow_results_list[[tmp_name]][[cohort.type.iter]][[growth.distributions.iter]][[trajectory.type.iter]] <-
                            sgpFlowTrajectories(
                                long_data = long_data,
                                state = state,
                                sgpFlow.config = sgpFlow.config.iter,
                                growth.distribution = growth.distributions.iter,
                                trajectory.type = trajectory.type.iter,
                                csem.perturbation.of.initial.scores = csem.perturbation.of.initial.scores,
                                csem.perturbation.iterations = csem.perturbation.iterations,
                                projection.splineMatrices = projection.splineMatrices[[paste(tail(sgpFlow.config.iter[["content_area.progression"]], 1), "BASELINE", sep=".")]]
                            )
                    } # End trajectory.type.iter
                } # End growth.distributions.iter
            } # End sequential processing if no parallel config
        } # End cohort.type.iter
    } # End sgpFlow.config.iter

    ## Export duckdb and Rdata files
    if (export.duckdb) {
        outputsgpFlow(sgpFlow_results_list, state = state, export.duckdb = export.duckdb, export.Rdata = export.Rdata)
    }

    return(sgpFlow_results_list)
} ### END sgpFlow
