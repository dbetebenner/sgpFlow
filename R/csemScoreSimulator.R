`csemScoreSimulator` <-
function(
	scale_score_csem_data,
	state,
	content_area,
	grade,
	csem_variable="SCALE_SCORE_CSEM",
	distribution="Normal") {

	## Utility functions
    get.loss.hoss <- function(state, content_area, grade) {
        return(sgpFlow::sgpFlowStateData[[state]][['Achievement']][['Knots_Boundaries']][[content_area]][[paste("boundaries", grade, sep="_")]])
    }

	## Parameters
	supported.distributions <- c("Normal")

	### Define relevant variables
	if (!distribution %in% supported.distributions) stop(paste0("Distribution supplied (", distribution, ") not currently supported."))
	if (!is.null(state)) {
		loss.hoss <- get.loss.hoss(state, content_area, grade) 
	} else {
		loss.hoss <- range(scale_score_csem_data[['SCALE_SCORE']], na.rm=TRUE)
	}

	scale_score_csem_data[, SCALE_SCORE_PERTURBED := SCALE_SCORE + rnorm(.N, mean = 0, sd = SCALE_SCORE_CSEM)]
	scale_score_csem_data[SCALE_SCORE_PERTURBED < loss.hoss[1L], SCALE_SCORE_PERTURBED := loss.hoss[1L]]
	scale_score_csem_data[SCALE_SCORE_PERTURBED > loss.hoss[2L], SCALE_SCORE_PERTURBED := loss.hoss[2L]]
	return(scale_score_csem_data[['SCALE_SCORE_PERTURBED']])
} ### END csemScoreSimulator
