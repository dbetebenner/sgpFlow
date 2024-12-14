#' @title Generate Percentile Trajectories
#' @description Computes percentile trajectories for student growth percentiles (SGPs) using projection matrices and optionally perturbs initial scores with conditional standard error of measurement (CSEM).
#' 
#' This function generates percentile trajectories over time for students based on provided data, state-specific configurations, and projection spline matrices. It supports iterative simulations with CSEM perturbation and custom growth distributions.
#' 
#' @param wide_data A `data.table` in wide format containing student data, including scale scores and student IDs, for calculating percentile trajectories.
#' @param state A character string indicating the state for which the trajectories are computed. This is used for state-specific configurations in the `sgpFlow` package.
#' @param sgpFlow.config A list of configuration parameters required for the SGP analysis, including grade progressions, content areas, and other metadata.
#' @param projection.splineMatrices A list of projection spline matrices used for modeling growth percentiles over time.
#' @param growth.distribution A character vector specifying the growth distribution for projecting scores. Options include `"UNIFORM-RANDOM"`, `"BETA"`, or percentile values (`"1"` through `"99"`). Default: `NULL`.
#' @param csem.perturbation.of.initial.scores Logical. If `TRUE`, perturbs initial scale scores using CSEM to introduce variability in simulations. Default: `TRUE`.
#' @param csem.perturbation.iterations Integer. Number of iterations for perturbing scores and calculating trajectories. Default: `100`.
#' @param iterate.without.csem.perturbation Logical. If `TRUE`, performs CSEM iterations without perturbing score to derive 100 simulated trajectories from single (non-perturbed) initial score.
#' @param csem.distribution A character string specifying the distribution to use for CSEM perturbation. Options include `"Normal"`. Default: `"Normal"`.
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
#'     growth.distribution = "UNIFORM-RANDOM",
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
#'  \code{\link[collapse]{anyv}}, \code{\link[collapse]{fsubset}}, \code{\link[collapse]{collapv}}, \code{\link[collapse]{na_omit}}, \code{\link[collapse]{pivot}}, \code{\link[collapse]{qDT}}
#'  \code{\link[splines]{bs}}
#' @rdname getPercentileTrajectories
#' @importFrom data.table copy setorder data.table rbindlist
#' @importFrom stats runif
#' @importFrom collapse anyv fsubset collapv na_omit pivot qDT
#' @importFrom splines bs
#' @importFrom utils head
#' @export 

getPercentileTrajectories <-
    function(
        wide_data,
        state,
        sgpFlow.config,
        projection.splineMatrices,
        growth.distribution = NULL,
        csem.perturbation.of.initial.scores = TRUE,
        csem.perturbation.iterations = 100L,
        iterate.without.csem.perturbation = FALSE,
        achievement.percentiles.tables,
        csem.distribution = "Normal"
    ) {

        ## Check arguments
        if (csem.perturbation.of.initial.scores & is.null(sgpFlow::sgpFlowStateData[[state]][["Achievement"]][["CSEM"]])) {
            stop(paste0(
                "CSEM meta-data not included in sgpFlowStateData for state: ", state,
                "\nContact package maintainers for CSEM meta-data incorporation into sgpFlow package."
            ))
        }
        if (csem.perturbation.iterations==0) csem.perturbation.of.initial.scores <- FALSE
        if (csem.perturbation.of.initial.scores & iterate.without.csem.perturbation) stop("Conflicting arguments: Both csem.perturbation.of.initial.scores and iterate.without.csem.perturbation set to TRUE.\nAt least one of the arguments must be set to FALSE.")
        if (!csem.perturbation.of.initial.scores & !iterate.without.csem.perturbation) csem.perturbation.iterations <- 0L

        ## Parameters
        sgpFlow.trajectories.list <- list()
        scale.score.variables.for.projections <- paste0("SCALE_SCORE_GRADE_", sgpFlow.config[['grade.progression']])
        if (csem.perturbation.of.initial.scores | iterate.without.csem.perturbation) wide_data_original <- data.table::copy(wide_data)

        ## Utility functions
        get.growth.distribution.projection.sequence <- function(growth.distribution, years.projected) {
            # Default to "UNIFORM-RANDOM" if NULL
            growth.distribution <- toupper(ifelse(is.null(growth.distribution), "UNIFORM-RANDOM", growth.distribution))
            supported_distributions <- c("UNIFORM-RANDOM", "BETA", as.character(1:99))
    
            # Validate growth.distribution
            if (!all(growth.distribution %in% supported_distributions)) {
                stop(sprintf(
                    "Unsupported 'growth.distribution' supplied: Currently supported values are: %s",
                    paste(sapply(supported_distributions, capWords), collapse = ", ")
                ))
            }
    
            # Validate length of growth.distribution
            if (!length(growth.distribution) %in% c(1, years.projected)) {
                stop(sprintf(
                    "Length of supplied 'growth.distribution' must be either 1 or %d.",
                    years.projected
                ))
            }
    
            # Return growth distribution sequence
            return(rep(growth.distribution, years.projected))
        }

        bound.iso.subset.scores <- function(projected.scores, loss.hoss, subset.indices) {
            ## Pull in outlier to loss/hoss
            projected.scores[
                TEMP_1 < loss.hoss[1L], TEMP_1 := loss.hoss[1L]
            ][TEMP_1 > loss.hoss[2L], TEMP_1 := loss.hoss[2L]]

            ## isotonize projections
            data.table::setorder(projected.scores, ID, TEMP_1)
            tmp.increment <- (seq.int(length(subset.indices)) - 1L) * 100L
            subset.indices <- c(rbind(subset.indices + tmp.increment, 1L + subset.indices + tmp.increment))
            return(
                collapse::fsubset(projected.scores, subset.indices) |>
                    collapse::collapv(by = "ID", keep.by = FALSE) |> # FUN = fmean by default
                    unlist(use.names = FALSE)
            )
        }

        get.percentile.trajectories.INTERNAL <- function(wide_data, growth.distribution.projection.sequence) {
            sgpFlow.trajectories.list.INTERNAL <- vector("list", length(projection.splineMatrices))
            completed_ids <- data.table::data.table(ID = wide_data[["ID"]], COMPLETED = NA, key = "ID")

            ## Loop over daisy-chained, matrix sequence
            for (i in seq_along(projection.splineMatrices)) {
                if (collapse::anyv(completed_ids[['COMPLETED']], NA)) {
                    cols_to_select <- c("ID", paste0("SCALE_SCORE_GRADE_", head(projection.splineMatrices[[i]][[1]]@Grade_Progression[[1]], -1)))
                    sgpFlow.trajectories.list.INTERNAL[[i]] <- collapse::na_omit(wide_data[is.na(completed_ids[['COMPLETED']]), ..cols_to_select])
                    completed_ids[sgpFlow.trajectories.list.INTERNAL[[i]][["ID"]], COMPLETED := TRUE]

                    for (j in seq_along(projection.splineMatrices[[i]])) {
                        qreg_coef_matrix <- projection.splineMatrices[[i]][[j]]
                        loss.hoss <- get.loss.hoss(state, tail(qreg_coef_matrix@Content_Areas[[1]], 1L), tail(qreg_coef_matrix@Grade_Progression[[1L]], 1L))
                        subset.indices <- get.subset.indices(sgpFlow.trajectories.list.INTERNAL[[i]], growth.distribution.projection.sequence[j])

                        #  Create model (design) matrix
                        #  Use function as a "promise" to evaluate directly rather than create intermediate objects
                        getModelMatrix <- function() {
                            Reduce(
                                f = cbind, init = 1L,
                                x = lapply(seq_along(qreg_coef_matrix@Time_Lags[[1L]]), function(model.iter) {
                                    splines::bs(
                                        x = sgpFlow.trajectories.list.INTERNAL[[i]][[ncol(sgpFlow.trajectories.list.INTERNAL[[i]]) - model.iter + 1L]],
                                        knots = qreg_coef_matrix@Knots[[model.iter]],
                                        Boundary.knots = qreg_coef_matrix@Boundaries[[model.iter]]
                                    )
                                })
                            )
                        }

                        projected.scores <-
                            collapse::pivot(
                                data =
                                    collapse::qDT(getModelMatrix() %*% qreg_coef_matrix@.Data)[, ID := sgpFlow.trajectories.list.INTERNAL[[i]][["ID"]]],
                                ids = "ID",
                                names = list("variable", "TEMP_1")
                            )[, variable := NULL]

                        sgpFlow.trajectories.list.INTERNAL[[i]][,
                            eval(paste0("SCALE_SCORE_GRADE_", tail(qreg_coef_matrix@Grade_Progression[[1L]], 1L))) :=
                                bound.iso.subset.scores(projected.scores, loss.hoss, subset.indices)
                        ]
                    } ## END j loop
                } ## END if (collapse::anyv(completed_ids[['COMPLETED']], NA))
            } ## END i loop

            return(data.table::data.table(data.table::rbindlist(sgpFlow.trajectories.list.INTERNAL, fill = TRUE), key = "ID"))
        }

        #######################################################
        ### getPercentileTrajectories Calculations
        #######################################################

        ## Create matrix sequence for projections
        growth.distribution.projection.sequence <- get.growth.distribution.projection.sequence(growth.distribution, length(projection.splineMatrices[[1]]))

        ## Loop over csem.perturbation.iterations (only one iteration if csem.perturbation.iterations == FALSE)
        for (csem.iter in 0:csem.perturbation.iterations) {
            ## On first iteration (csem.iter == 0) apply transformScaleScore
            if (csem.iter == 0) {
                ## Apply transformScaleScore to:
                ## 1. Create achievement percentiles tables if achievement.percentiles.tables == TRUE
                ## 2. Add SCALE_SCORE_PERCENTILES and SCALE_SCORE_PERCENTILES_MULTIVARIATE to wide_data if achievement.percentiles.tables == FALSE
                wide_data <- transformScaleScore(data.table::copy(wide_data_original), scale_score.names = paste("SCALE_SCORE_GRADE", sgpFlow.config[["grade.progression"]], sep="_"), achievement.percentiles.tables = achievement.percentiles.tables)
            }

            ## After first iteration (csem.iter > 0) perturb initial scores with CSEM
            if (csem.perturbation.of.initial.scores & csem.iter != 0L) {
                wide_data <- perturbScoresWithCSEM(data.table::copy(wide_data_original), state, sgpFlow.config, csem.distribution)
                ## Apply transformScaleScore to:
                ## 1. Create achievement percentiles tables if achievement.percentiles.tables == TRUE
                ## 2. Add SCALE_SCORE_PERCENTILES and SCALE_SCORE_PERCENTILES_MULTIVARIATE to wide_data if achievement.percentiles.tables == FALSE
                wide_data <- transformScaleScore(wide_data, scale_score.names = paste("SCALE_SCORE_GRADE", sgpFlow.config[["grade.progression"]], sep="_"), achievement.percentiles.tables = achievement.percentiles.tables)
            }

            ## Get percentile trajectories
            sgpFlow.trajectories.list[[paste("ITERATION", csem.iter, sep="_")]] <- wide_data[,setdiff(names(wide_data), scale.score.variables.for.projections), with=FALSE][get.percentile.trajectories.INTERNAL(wide_data, growth.distribution.projection.sequence), on="ID"]
        } ## END csem.iter loop

        return(sgpFlow.trajectories.list)
    } ### END getPercentileTrajectories
