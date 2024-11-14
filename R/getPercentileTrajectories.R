`getPercentileTrajectories` <- 
    function(
        wide_data,
        state,
        sgpFlow.config,
        projection.splineMatrices,
        growth.distribution=NULL,
        csem.perturbation.of.initial.scores=TRUE,
        csem.perturbation.iterations=100L,
        csem.distribution="Normal"
    ) {

        ## Parameters 
        sgpFlow.trajectories.list <- list()
        if (csem.perturbation.of.initial.scores) wide_data_original <- copy(wide_data)

        ## Check arguments 
        if (csem.perturbation.of.initial.scores & is.null(sgpFlow::sgpFlowStateData[[state]][['Achievement']][['CSEM']])) {
            stop(paste0("CSEM meta-data not included in sgpFlowStateData for state: ", state, 
            "\nContact package maintainers for CSEM meta-data incorporation into sgpFlow package."))
        }

        if (csem.perturbation.of.initial.scores==FALSE) csem.perturbation.iterations <- 1L

        ## Utility functions
        get.growth.distribution.projection.sequence <- function(growth.distribution, years.projected) {
            if (is.null(growth.distribution)) growth.distribution <- "UNIFORM-RANDOM"
            growth.distribution <- toupper(growth.distribution)
            supported_distributions <- c("UNIFORM-RANDOM", as.character(1:99))
            
            # Check for unsupported growth distribution & correct length
            if (!all(growth.distribution %in% supported_distributions)) {
                stop(paste("Unsupported 'growth.distribution' supplied: Currently supported 'growth.distribution':", 
                        paste(sapply(supported_distributions, capwords), collapse = ", ")))
            }
            
            if (!length(growth.distribution) %in% c(1, years.projected)) {
                stop(paste0("Length of supplied growth distribution must be either 1 or ", years.projected, "."))
            }
            
            # Return values based on the input growth.distribution
            if (growth.distribution == "UNIFORM-RANDOM") return(rep("UNIFORM-RANDOM", years.projected))
            if (length(growth.distribution) == 1L && growth.distribution %in% as.character(1:99)) return(rep(growth.distribution, years.projected))
            return(rep(growth.distribution, years.projected))
        }

        get.subset.indices <- function(wide_data, growth.distribution) {
            if (growth.distribution=="UNIFORM-RANDOM") {
                # tmp.quantiles <- runif(nrow(wide_data), min = 0, max = 100)
                # return(pmin(pmax(findInterval(tmp.quantiles, seq(0.5, 100.5, 1), rightmost.closed = TRUE), 1L), 99L))
                return(
                    runif(dim(wide_data)[1L], min = 0, max = 100) |>  ##  select random uniform values (REAL)
                      round() |> as.integer() |>                    ##  round and convert to INTEGER
                        setv(0L, 1L) |> setv(100L, 99L)             ##  bound between 1 and 99 by reference
                )
            }

            if (growth.distribution %in% as.character(1:99)) {
                return(rep(as.integer(growth.distribution), nrow(wide_data)))
            }
        }

        bound.iso.subset.scores <- function(projected.scores, loss.hoss, subset.indices) {
            ## Pull in outlier to loss/hoss
            projected.scores[
              TEMP_1 < loss.hoss[1L], TEMP_1 := loss.hoss[1L]
            ][TEMP_1 > loss.hoss[2L], TEMP_1 := loss.hoss[2L]]

            ## isotonize projections
            setorder(projected.scores, ID, TEMP_1)
            tmp.increment <- (seq.int(length(subset.indices)) - 1L) * 100L
            subset.indices <- c(rbind(subset.indices + tmp.increment, 1L + subset.indices + tmp.increment))
            # return(projected.scores[subset.indices, .(TEMP_2=mean(TEMP_1)), by="ID"][['TEMP_2']])
            return(
                fsubset(projected.scores, subset.indices) |>
                  collapv(by = "ID", keep.by = FALSE) |> # FUN = fmean by default
                    unlist(use.names = FALSE)
            )
		}

        get.percentile.trajectories.INTERNAL <- function(wide_data, growth.distribution.projection.sequence) {

            sgpFlow.trajectories.list.INTERNAL <- vector("list", length(projection.splineMatrices))
            completed_ids <- data.table(ID = wide_data[['ID']], COMPLETED = FALSE, key="ID")

            ## Loop over daisy-chained, matrix sequence
            for (i in seq_along(projection.splineMatrices)) {
                sgpFlow.trajectories.list.INTERNAL[[i]] <-
                    na_omit(wide_data[!completed_ids[COMPLETED == TRUE, ID], c("ID", paste0("SS", head(projection.splineMatrices[[i]][[1]]@Grade_Progression[[1]], -1))), with = FALSE])
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
                                    bs( x = sgpFlow.trajectories.list.INTERNAL[[i]][[ncol(sgpFlow.trajectories.list.INTERNAL[[i]]) - model.iter + 1L]],
                                        knots = qreg_coef_matrix@Knots[[model.iter]],
                                        Boundary.knots = qreg_coef_matrix@Boundaries[[model.iter]]
                                    )}
                                )
                        )
                    }

                    projected.scores <-
                        pivot(
                            data =
                              qDT(getModelMatrix() %*% qreg_coef_matrix@.Data)[, ID := sgpFlow.trajectories.list.INTERNAL[[i]][['ID']]],
                            ids = "ID",
                            names = list("variable", "TEMP_1")
                        )[, variable := NULL]

                    sgpFlow.trajectories.list.INTERNAL[[i]][,
                        eval(paste0("SS", tail(qreg_coef_matrix@Grade_Progression[[1L]], 1L))) :=
                            bound.iso.subset.scores(projected.scores, loss.hoss, subset.indices)
                    ]
	    	    } ## END j loop
            } ## END i loop

            return(data.table(rbindlist(sgpFlow.trajectories.list.INTERNAL, fill=TRUE), key="ID"))
        }

        #######################################################
        ### getPercentileTrajectories Calculations
        #######################################################

        ## Create matrix sequence for projections 
        growth.distribution.projection.sequence <- get.growth.distribution.projection.sequence(growth.distribution, length(projection.splineMatrices[[1]]))

        ## Loop over csem.perturbation.iterations
        for (csem.iter in seq(csem.perturbation.iterations)) {

            ## Perturb initial scores with CSEM if requested (after first iteration) 
            if (csem.perturbation.of.initial.scores & csem.iter!=1L) {
                wide_data <-
                    perturbScoresWithCSEM(copy(wide_data_original), state, sgpFlow.config, csem.distribution)
            }

            ## Get percentile trajectories
            sgpFlow.trajectories.list[[csem.iter]] <- get.percentile.trajectories.INTERNAL(wide_data, growth.distribution.projection.sequence)
        } ## END csem.iter loop
        
    return(sgpFlow.trajectories.list)
} ### END getPercentileTrajectories
