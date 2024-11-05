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
	distribution <- toupper(distribution)
	supported.distributions <- c("NORMAL")

	### Define relevant variables
	if (!distribution %in% supported.distributions) stop(paste0("Distribution supplied (", distribution, ") not currently supported."))
	if (!is.null(state)) {
		loss.hoss <- get.loss.hoss(state, content_area, grade) 
	} else {
		loss.hoss <- range(scale_score_csem_data[['SCALE_SCORE']], na.rm=TRUE)
	}

	if (distribution=="NORMAL") {
		scale_score_csem_data[, SCALE_SCORE_ORIGINAL := SCALE_SCORE]
		scale_score_csem_data[, SCALE_SCORE := SCALE_SCORE_ORIGINAL + rnorm(.N, mean = 0, sd = get(csem_variable))]
	}

	## Pull in scores to loss and hoss
	scale_score_csem_data[SCALE_SCORE < loss.hoss[1L], SCALE_SCORE := loss.hoss[1L]]
	scale_score_csem_data[SCALE_SCORE > loss.hoss[2L], SCALE_SCORE := loss.hoss[2L]]

	## Return perturbed values
	return(scale_score_csem_data[['SCALE_SCORE']])
} ### END csemScoreSimulator
