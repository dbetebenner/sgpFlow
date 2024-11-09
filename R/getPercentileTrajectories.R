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
                tmp.quantiles <- runif(dim(ss.data)[1L], min = 0, max = 100)
                my.indices <- findInterval(tmp.quantiles, seq(0.5, by = 1, length = 100), rightmost.closed = TRUE)
                my.indices <- pmin(pmax(my.indices, 1L), 99L)
                return(my.indices)
            }

            if (growth.distribution %in% as.character(1:99)) {
                return(rep(as.integer(growth.distribution), dim(ss.data)[1L]))
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
            ## Loop over daisy-chained, matrix sequence
            for (i in seq_along(projection.splineMatrices)) {
                label.iter <- 1L
                for (j in seq_along(projection.splineMatrices[[i]])) {
		    	    tmp.matrix <- projection.splineMatrices[[i]][[j]]
                    loss.hoss <- get.loss.hoss(state, tail(tmp.matrix@Content_Areas[[1]], 1L), tail(tmp.matrix@Grade_Progression[[1L]], 1L))
                    subset.indices <- get.subset.indices(ss.data, growth.distribution.projection.sequence[j])

		    	    mod <- character()
		    	    int <- "data.table(ID=ss.data[[1L]], INT=1L,"
		    	    for (model.iter in seq_along(projection.splineMatrices[[i]][[j]]@Time_Lags[[1L]])) {
		    		    knt <- paste0("tmp.matrix@Knots[[", model.iter, "]]")
			    	    bnd <- paste0("tmp.matrix@Boundaries[[", model.iter, "]]")
			    	    mod <- paste0(mod, ", bs(ss.data[[", dim(ss.data)[2L]-model.iter+1L, "]], knots=", knt, ", Boundary.knots=", bnd, ")")
		    	    }

    			    tmp.scores <- eval(parse(text=paste0(int, substring(mod, 2L), ", key='ID')")))
                    projected.scores <- melt(as.data.table(as.matrix(tmp.scores[,-1L]) %*% tmp.matrix@.Data)[,ID:=tmp.scores[['ID']]], id.vars="ID", value.name="TEMP_1")[,variable:=NULL]
                    ss.data[,TEMP_2:=bound.iso.subset.scores(projected.scores, loss.hoss, subset.indices)]
    			    setnames(ss.data, "TEMP_2", paste0("SS", tail(tmp.matrix@Grade_Progression[[1L]], 1L)))
	    		    label.iter <- label.iter + 1L
	    	    } ## END j loop
            } ## END i loop
            return(ss.data)
        }

        ## Create matrix sequence for projections 
        growth.distribution.projection.sequence <- get.growth.distribution.projection.sequence(growth.distribution, length(projection.splineMatrices[[1]]))

        ## Loop over csem.perturbation.iterations
        for (csem.iter in seq(csem.perturbation.iterations)) {

            ## Perturb initial scores with CSEM if requested (after first iteration) 
            if (csem.perturbation.of.initial.scores & csem.iter!=1L) {
                ss.data <- perturbScoresWithCSEM(ss.data, state, sgpFlow.config)
            }

            ## Get percentile trajectories
            sgpFlow.trajectories.list <- get.percentile.trajectories.INTERNAL(ss.data, growth.distribution.projection.sequence)
        } ## END csem.iter loop
        
    return(sgpFlow.trajectories.list)
} ### END getPercentileTrajectories
