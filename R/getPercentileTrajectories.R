#' @title Generate Percentile Trajectories
#' @description Computes percentile trajectories for student growth percentiles (SGPs) using projection matrices and optionally perturbs initial scores with conditional standard error of measurement (CSEM).
#' 
#' This function generates percentile trajectories over time for students based on provided data, state-specific configurations, and projection spline matrices. It supports iterative simulations with CSEM perturbation and custom growth distributions.
#' 
#' @param wide_data A `data.table` in wide format containing student data, including scale scores and student IDs, for calculating percentile trajectories.
#' @param state A character string indicating the state for which the trajectories are computed. This is used for state-specific configurations in the `sgpFlow` package.
#' @param sgpFlow.config A list of configuration parameters required for the SGP analysis, including grade progressions, content areas, and other metadata.
#' @param projection.splineMatrices A list of projection spline matrices used for modeling growth percentiles over time.
#' @param growth.distribution A character vector specifying the growth distribution for projecting scores. Options include `"UNIFORM_RANDOM"`, `"BETA"`, or percentile values (`"1"` through `"99"`). Default: `NULL`.
#' @param trajectory.type A character vector specifying the type of trajectory "rounding" to perform when calculating growth trajectories. Options include \code{"EXACT_VALUE"}, \code{"NEAREST_INTEGER_VALUE"}, and \code{"NEAREST_OBSERVED_VALUE"}. Default is \code{"EXACT_VALUE"}.
#' @param csem.perturbation.of.initial.scores Logical. If `TRUE`, perturbs initial scale scores using CSEM to introduce variability in simulations. Default: `TRUE`.
#' @param csem.perturbation.iterations Integer. Number of iterations for perturbing scores and calculating trajectories. Default: `100`.
#' @param csem.perturbation.distribution A character string specifying the distribution to use for CSEM perturbation. Options include `"NORMAL"`. Default: `"NORMAL"`.
#' @returns A list of `data.table` objects, where each element represents the results of one simulation iteration. Each `data.table` contains student IDs and their projected scale scores at different percentiles.
#' @details 
#' - The function allows for iterative simulation of percentile trajectories using CSEM perturbations.
#' - Growth distribution can be customized for each year or kept uniform across projections.
#' - Handles projection matrix sequences (`projection.splineMatrices`) to compute scale scores over time.
#' - Perturbation with CSEM requires state-specific meta-data in the `sgpFlowStateData` object.
#' - Internal checks ensure valid configurations for growth distribution and state meta-data.
#' 
#' **Steps:**
#' 1. Generates a growth distribution projection sequence.
#' 2. Optionally perturbs initial scores using CSEM.
#' 3. Computes percentile trajectories using projection spline matrices for each iteration.
#' 4. Binds and returns all iterations as a list of `data.table` objects.
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'   # Example usage
#'   trajectories <- getPercentileTrajectories(
#'     wide_data = student_data,
#'     state = "NY",
#'     sgpFlow.config = sgp_config_list,
#'     projection.splineMatrices = spline_matrices_list,
#'     growth.distribution = "UNIFORM_RANDOM",
#'     csem.perturbation.iterations = 50
#'   )
#'
#'   # Access the first iteration results
#'   print(trajectories[[1]])
#' }
#' }
#' @seealso
#'  \code{\link[data.table]{copy}}, \code{\link[data.table]{setorder}}, \code{\link[data.table]{data.table}}, \code{\link[data.table]{rbindlist}}
#'  \code{\link[sgpFlow]{sgpFlowStateData}}
#'  \code{\link[stats]{runif}}
#'  \code{\link[collapse]{fnrow}}, \code{\link[collapse]{na_omit}}, \code{\link[collapse]{qM}}, \code{\link[collapse]{whichNA}}
#'  \code{\link[splines]{bs}}
#' @rdname getPercentileTrajectories
#' @importFrom data.table copy setorder data.table rbindlist
#' @importFrom stats runif
#' @importFrom collapse fnrow na_omit qM whichNA
#' @importFrom Rfast Pmin Pmax Round
#' @importFrom splines2 bSpline
#' @importFrom strider row_sums
#' @importFrom utils head
#' @export

getPercentileTrajectories <-
    function(
        wide_data,
        state,
        sgpFlow.config,
        projection.splineMatrices,
        growth.distribution = NULL,
        trajectory.type = "EXACT_VALUE",
        csem.perturbation.of.initial.scores = TRUE,
        csem.perturbation.iterations = 100L,
        csem.perturbation.distribution = "NORMAL"
    ) {

        ## Check arguments
        if (!csem.perturbation.of.initial.scores) csem.perturbation.iterations <- 0L
        if (csem.perturbation.of.initial.scores && is.null(sgpFlow::sgpFlowStateData[[state]][["Achievement"]][["CSEM"]])) {
            stop(paste0(
                "CSEM meta-data not included in sgpFlowStateData for state: ", state,
                "\nContact package maintainers for CSEM meta-data incorporation into sgpFlow package."
            ))
        }

        ## Utility functions
        get.simulation.label <- function(row.count, csem.perturbation.iterations) {
            if (csem.perturbation.iterations == 0L) {
                return(rep("OBSERVED_DATA", row.count))
            } else {
                return(c(rep("OBSERVED_DATA", row.count), paste("SIMULATED_DATA", rep(1:csem.perturbation.iterations, each = row.count), sep="_")))
            }
        }

        get.percentile.trajectories.INTERNAL <- function(wide_data, growth.distribution.projection.sequence, trajectory.type) {

            ## Utility functions (INTERNAL)

            #  Create model data matrix based upon b-splined scores
            #  Use function as a "promise" to evaluate directly rather than create intermediate objects
            getModelDataMatrix <- function() {
                spline.data <- lapply(seq_along(qreg_coef_matrix@Time_Lags[[1L]]), function(model.iter) {
                    splines2::bSpline(
                        x = sgpFlow.trajectories.list.INTERNAL[[i]][[paste0("SCALE_SCORE_GRADE_", qreg_coef_matrix@Grade_Progression[[1L]][model.iter])]],
                        knots = qreg_coef_matrix@Knots[[paste("knots", qreg_coef_matrix@Grade_Progression[[1L]][model.iter], sep="_")]],
                        Boundary.knots = qreg_coef_matrix@Boundaries[[paste("boundaries", qreg_coef_matrix@Grade_Progression[[1L]][model.iter], sep="_")]]
                    )
                 })
                do.call(cbind, c(list(rep.int(1L, collapse::fnrow(sgpFlow.trajectories.list.INTERNAL[[i]]))), spline.data))
            }

            # Create optimized model matrix based upon growth.distribution.indices
            #  Use function as a "promise" to evaluate directly rather than create intermediate objects
            getModelMatrix <- function(column.indices) {
                t(qreg_coef_matrix[, column.indices, drop = FALSE])
            }

            ## Initialize vectors to store results
            sgpFlow.trajectories.list.INTERNAL <- vector("list", length(projection.splineMatrices))
            completed_rows <- vector("logical", collapse::fnrow(wide_data))

            ## Loop over daisy-chained, matrix sequence
            for (i in seq_along(projection.splineMatrices)) {
                if (any(!completed_rows)) {
                    cols_to_select <- paste0("SCALE_SCORE_GRADE_", head(projection.splineMatrices[[i]][[1]]@Grade_Progression[[1]], -1))
                    
                    # Subset using direct row indices
                    current_data <- wide_data[collapse::whichv(completed_rows, FALSE), cols_to_select, with=FALSE]
                    sgpFlow.trajectories.list.INTERNAL[[i]] <- collapse::na_omit(current_data)
                    
                    # Update completed rows if no NA values found in this subset
                    completed_rows[collapse::whichNA(strider::row_sums(collapse::qM(current_data)), invert = TRUE)] <- TRUE
                    
                    for (j in seq_along(projection.splineMatrices[[i]])) {
                        qreg_coef_matrix <- projection.splineMatrices[[i]][[j]]
                        loss.hoss <- get.loss.hoss(state, tail(qreg_coef_matrix@Content_Areas[[1]], 1L), tail(qreg_coef_matrix@Grade_Progression[[1L]], 1L))
                        growth.distribution.indices <- getGrowthDistributionIndices(sgpFlow.trajectories.list.INTERNAL[[i]], growth.distribution.projection.sequence[[j]])
                        
                        # Target column names for the projection
                        growth_variable_name <- paste0("SGP_GRADE_", tail(qreg_coef_matrix@Grade_Progression[[1L]], 1L))
                        projection_variable_name <- paste0("SCALE_SCORE_GRADE_", tail(qreg_coef_matrix@Grade_Progression[[1L]], 1L))
                        variable_names <- c(growth_variable_name, projection_variable_name)
                        
                        # Add projections to trajectories list based upon trajectory.type
                        if (trajectory.type == "EXACT_VALUE") {
                            sgpFlow.trajectories.list.INTERNAL[[i]][, (variable_names) :=
                                list(
                                    growth.distribution.indices,
                                    bound.scores(strider::row_sums(getModelDataMatrix() * getModelMatrix(growth.distribution.indices)), loss.hoss)
                                )
                            ]
                        }

                        if (trajectory.type == "NEAREST_INTEGER_VALUE") {
                            sgpFlow.trajectories.list.INTERNAL[[i]][, (variable_names) :=
                                list(
                                    growth.distribution.indices,
                                    bound.scores(Rfast::Round(strider::row_sums(getModelDataMatrix() * getModelMatrix(growth.distribution.indices))), loss.hoss)
                                )
                            ]
                        }

                        if (trajectory.type == "NEAREST_OBSERVED_VALUE") {
                            ## TODO: Implement nearest observed value
                        }
                    } ## END j loop
                } ## END if (any(!completed_rows))
            } ## END i loop
            
            # Combine all results
            return(data.table::rbindlist(sgpFlow.trajectories.list.INTERNAL, fill = TRUE))
        } ## END get.percentile.trajectories.INTERNAL

        ## Create growth distribution sequence associated with each projection matrix
        growth.distribution.projection.sequence <- getGrowthDistributionProjectionSequence(growth.distribution, length(projection.splineMatrices[[1]]))

        ## Calculate and return percentile trajectories
        scale.score.variables.for.projections <- c("ID", paste0("SCALE_SCORE_GRADE_", sgpFlow.config[["grade.progression"]]))
        get.percentile.trajectories.INTERNAL(
                            wide_data = if (csem.perturbation.iterations == 0L) wide_data else perturbScoresWithCSEM(wide_data[,..scale.score.variables.for.projections], state, sgpFlow.config, csem.perturbation.iterations, csem.perturbation.distribution, trajectory.type),
                            growth.distribution.projection.sequence = growth.distribution.projection.sequence,
                            trajectory.type = trajectory.type)[, SIMULATION_ITERATION := get.simulation.label(collapse::fnrow(wide_data), csem.perturbation.iterations)]

    } ## END getPercentileTrajectories
