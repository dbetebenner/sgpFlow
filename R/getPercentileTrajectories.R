`getPercentileTrajectories` <- 
function(
         ss.data,
         state,
         sgpFlow.config,
         projection.splineMatrices,
         growth.distribution=NULL,
         csem.perturbation.of.initial.scores=TRUE,
         csem.perturbation.iterations=100L) {

        ## Parameters 
        sgpFlow.trajectories.list <- list()
        if (csem.perturbation.of.initial.scores) ss.data.original <- copy(ss.data)

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

        get.loss.hoss <- function(state, content_area, grade) {
            return(sgpFlow::sgpFlowStateData[[state]][['Achievement']][['Knots_Boundaries']][[content_area]][[paste("loss.hoss", grade, sep="_")]])
        }

        get.subset.indices <- function(ss.data, growth.distribution) {

            if (growth.distribution=="UNIFORM-RANDOM") {
                tmp.quantiles <- runif(nrow(ss.data), min = 0, max = 100)
                return(pmin(pmax(findInterval(tmp.quantiles, seq(0.5, 100.5, 1), rightmost.closed = TRUE), 1L), 99L))
            }

            if (growth.distribution %in% as.character(1:99)) {
                return(rep(as.integer(growth.distribution), nrow(ss.data)))
            }
        }

        bound.iso.subset.scores <- function(projected.scores, loss.hoss, subset.indices) {
            ## Pull in outlier to loss/hoss
            projected.scores[TEMP_1 < loss.hoss[1L], TEMP_1 := loss.hoss[1L]]
            projected.scores[TEMP_1 > loss.hoss[2L], TEMP_1 := loss.hoss[2L]]

            ## isotonize projections
            setorder(projected.scores, ID, TEMP_1)
            tmp.increment <- (seq.int(length(subset.indices)) - 1L) * 100L
            subset.indices <- c(rbind(subset.indices + tmp.increment, 1L + subset.indices + tmp.increment))
            return(projected.scores[subset.indices, .(TEMP_2=mean(TEMP_1)), by="ID"][['TEMP_2']])
		}

        get.percentile.trajectories.INTERNAL <- function(ss.data, growth.distribution.projection.sequence) {

            sgpFlow.trajectories.list.INTERNAL <- vector("list", length(projection.splineMatrices))
            completed.ids <- rep(FALSE, nrow(ss.data))  # Logical vector for completed IDs

            ## Loop over daisy-chained, matrix sequence
            for (i in seq_along(projection.splineMatrices)) {
                sgpFlow.trajectories.list.INTERNAL[[i]] <- na.omit(ss.data[!completed.ids, c("ID", paste0("SS", head(projection.splineMatrices[[i]][[1]]@Grade_Progression[[1]], -1))), with = FALSE])
                completed.ids[which(ss.data[['ID']] %in% sgpFlow.trajectories.list.INTERNAL[[i]][["ID"]])] <- TRUE

                for (j in seq_along(projection.splineMatrices[[i]])) {
		    	    tmp.matrix <- projection.splineMatrices[[i]][[j]]
                    loss.hoss <- get.loss.hoss(state, tail(tmp.matrix@Content_Areas[[1]], 1L), tail(tmp.matrix@Grade_Progression[[1L]], 1L))
                    subset.indices <- get.subset.indices(sgpFlow.trajectories.list.INTERNAL[[i]], growth.distribution.projection.sequence[j])

                   # Create basis terms directly without eval(parse(...))
                    bspline_terms <- lapply(seq_along(tmp.matrix@Time_Lags[[1L]]), function(model.iter) {
                        knt <- tmp.matrix@Knots[[model.iter]]
                        bnd <- tmp.matrix@Boundaries[[model.iter]]
                            bs(sgpFlow.trajectories.list.INTERNAL[[i]][[ncol(sgpFlow.trajectories.list.INTERNAL[[i]]) - model.iter + 1L]], knots = knt, Boundary.knots = bnd)
                    })

#		    	    mod <- character()
#		    	    int <- "data.table(ID=sgpFlow.trajectories.list.INTERNAL[[i]][[1L]], INT=1L,"
#		    	    for (model.iter in seq_along(tmp.matrix@Time_Lags[[1L]])) {
#		    		    knt <- paste0("tmp.matrix@Knots[[", model.iter, "]]")
#			    	    bnd <- paste0("tmp.matrix@Boundaries[[", model.iter, "]]")
#			    	    mod <- paste0(mod, ", bs(sgpFlow.trajectories.list.INTERNAL[[i]][[", ncol(sgpFlow.trajectories.list.INTERNAL[[i]])-model.iter+1L, "]], knots=", knt, ", Boundary.knots=", bnd, ")")
#		    	    }

                    tmp.scores <- cbind(1, do.call(cbind, bspline_terms))
                    projected.scores <- melt(as.data.table(tmp.scores %*% tmp.matrix@.Data)[,ID:=sgpFlow.trajectories.list.INTERNAL[[i]][['ID']]], id.vars="ID", value.name="TEMP_1")[, variable:=NULL]
                    sgpFlow.trajectories.list.INTERNAL[[i]][, TEMP_2 := bound.iso.subset.scores(projected.scores, loss.hoss, subset.indices)]
                    setnames(sgpFlow.trajectories.list.INTERNAL[[i]], "TEMP_2", paste0("SS", tail(tmp.matrix@Grade_Progression[[1L]], 1L)))

#    			    tmp.scores <- eval(parse(text=paste0(int, substring(mod, 2L), ", key='ID')")))
#                   projected.scores <- melt(as.data.table(as.matrix(tmp.scores[,-1L]) %*% tmp.matrix@.Data)[,ID:=tmp.scores[['ID']]], id.vars="ID", value.name="TEMP_1")[,variable:=NULL]
#                   sgpFlow.trajectories.list.INTERNAL[[i]][,TEMP_2:=bound.iso.subset.scores(projected.scores, loss.hoss, subset.indices)]
#    			    setnames(sgpFlow.trajectories.list.INTERNAL[[i]], "TEMP_2", paste0("SS", tail(tmp.matrix@Grade_Progression[[1L]], 1L)))
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
                ss.data <- copy(ss.data.original)
                ss.data <- perturbScoresWithCSEM(ss.data, state, sgpFlow.config)
            }

            ## Get percentile trajectories
            sgpFlow.trajectories.list[[csem.iter]] <- get.percentile.trajectories.INTERNAL(ss.data, growth.distribution.projection.sequence)
        } ## END csem.iter loop
        
    return(sgpFlow.trajectories.list)
} ### END getPercentileTrajectories
