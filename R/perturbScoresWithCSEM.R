`perturbScoresWithCSEM` <-
function(
	ss.data,
	state,
	sgpFlow.config,
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

	### Loop over grades in grade.progression
	for (grade.iter in seq_along(sgpFlow.config[['grade.progression']])) {
		tmp.grade <- sgpFlow.config[['grade.progression']][grade.iter]
		tmp.content_area <- sgpFlow.config[['content_area.progression']][grade.iter]

		if (!is.null(state)) {
			loss.hoss <- get.loss.hoss(state, tmp.content_area, tmp.grade) 
		} else {
			loss.hoss <- range(ss.data[[paste0("SS", tmp.grade)]], na.rm=TRUE)
		}

		if (distribution=="NORMAL") {
#			ss.data[, (paste0("SS", tmp.grade, "_ORIGINAL")) := get(paste0("SS", tmp.grade))]
			ss.data[, (paste0("SS", tmp.grade)) := get(paste0("SS", tmp.grade)) + rnorm(nrow(ss.data), sd=sgpFlow::sgpFlowStateData[[state]][['Achievement']][['CSEM']][[tmp.content_area]][[paste("GRADE", tmp.grade, sep="_")]](ss.data[[paste0("SS", tmp.grade)]]))]
		}

		## Pull in scores to loss and hoss
		ss.data[(paste0("SS", tmp.grade)) < loss.hoss[1L], (paste0("SS", tmp.grade)) := loss.hoss[1L]]
		ss.data[(paste0("SS", tmp.grade)) > loss.hoss[2L], (paste0("SS", tmp.grade)) := loss.hoss[2L]]
	}

	## Return perturbed values
	return(ss.data)
} ### END perturbScoresWithCSEM 