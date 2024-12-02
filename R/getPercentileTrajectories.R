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
#'  \code{\link[collapse]{setv}}, \code{\link[collapse]{fsubset}}, \code{\link[collapse]{collapv}}, \code{\link[collapse]{na_omit}}, \code{\link[collapse]{pivot}}, \code{\link[collapse]{qDT}}
#'  \code{\link[splines]{bs}}
#' @rdname getPercentileTrajectories
#' @export 
#' @importFrom data.table copy setorder data.table rbindlist
#' @importFrom stats runif
#' @importFrom collapse setv fsubset collapv na_omit pivot qDT
#' @importFrom splines bs
#' @importFrom utils head

getPercentileTrajectories <-
    function(
        wide_data,
        state,
        sgpFlow.config,
        projection.splineMatrices,
        growth.distribution = NULL,
        csem.perturbation.of.initial.scores = TRUE,
        csem.perturbation.iterations = 100L,
        csem.distribution = "Normal"
    ) {
        ## Parameters
        sgpFlow.trajectories.list <- list()
        if (csem.perturbation.of.initial.scores) wide_data_original <- data.table::copy(wide_data)

        ## Check arguments
        if (csem.perturbation.of.initial.scores & is.null(sgpFlow::sgpFlowStateData[[state]][["Achievement"]][["CSEM"]])) {
            stop(paste0(
                "CSEM meta-data not included in sgpFlowStateData for state: ", state,
                "\nContact package maintainers for CSEM meta-data incorporation into sgpFlow package."
            ))
        }

        if (csem.perturbation.of.initial.scores == FALSE) csem.perturbation.iterations <- 1L

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

        get.subset.indices <- function(wide_data, growth.distribution) {
            if (growth.distribution == "UNIFORM-RANDOM") {
                return(
                    stats::runif(nrow(wide_data), min = 0, max = 100) |> ##  select random uniform values (REAL)
                        round() |> as.integer() |> ##  round and convert to INTEGER
                        collapse::setv(0L, 1L) |> collapse::setv(100L, 99L) ##  bound between 1 and 99 by reference
                )
            }

            if (growth.distribution %in% as.character(1:99)) {
                return(rep(as.integer(growth.distribution), nrow(wide_data)))
            }

            if (growth.distribution == "BETA") {
                return(my.beta())
            }
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
            # return(projected.scores[subset.indices, .(TEMP_2=mean(TEMP_1)), by="ID"][['TEMP_2']])
            return(
                collapse::fsubset(projected.scores, subset.indices) |>
                    collapse::collapv(by = "ID", keep.by = FALSE) |> # FUN = fmean by default
                    unlist(use.names = FALSE)
            )
        }

        get.percentile.trajectories.INTERNAL <- function(wide_data, growth.distribution.projection.sequence) {
            sgpFlow.trajectories.list.INTERNAL <- vector("list", length(projection.splineMatrices))
            completed_ids <- data.table::data.table(ID = wide_data[["ID"]], COMPLETED = FALSE, key = "ID")

            ## Loop over daisy-chained, matrix sequence
            for (i in seq_along(projection.splineMatrices)) {
                sgpFlow.trajectories.list.INTERNAL[[i]] <-
                    collapse::na_omit(wide_data[!completed_ids[COMPLETED == TRUE, ID], c("ID", paste0("SS", head(projection.splineMatrices[[i]][[1]]@Grade_Progression[[1]], -1))), with = FALSE])
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

                    sgpFlow.trajectories.list.INTERNAL[[i]][
                        ,
                        eval(paste0("SS", tail(qreg_coef_matrix@Grade_Progression[[1L]], 1L))) :=
                            bound.iso.subset.scores(projected.scores, loss.hoss, subset.indices)
                    ]
                } ## END j loop
            } ## END i loop

            return(data.table::data.table(data.table::rbindlist(sgpFlow.trajectories.list.INTERNAL, fill = TRUE), key = "ID"))
        }

        #######################################################
        ### getPercentileTrajectories Calculations
        #######################################################

        ## Create matrix sequence for projections
        growth.distribution.projection.sequence <- get.growth.distribution.projection.sequence(growth.distribution, length(projection.splineMatrices[[1]]))

        ## Loop over csem.perturbation.iterations
        for (csem.iter in seq(csem.perturbation.iterations)) {
            ## Perturb initial scores with CSEM if requested (after first iteration)
            if (csem.perturbation.of.initial.scores & csem.iter != 1L) {
                wide_data <-
                    perturbScoresWithCSEM(data.table::copy(wide_data_original), state, sgpFlow.config, csem.distribution)
            }

            ## Get percentile trajectories
            sgpFlow.trajectories.list[[csem.iter]] <- get.percentile.trajectories.INTERNAL(wide_data, growth.distribution.projection.sequence)
        } ## END csem.iter loop

        return(sgpFlow.trajectories.list)
    } ### END getPercentileTrajectories
