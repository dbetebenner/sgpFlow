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

	perturb.rnorm <- function(mean = 0, sd) {
	    dt <- data.table(sd = sd, VALUE = as.numeric(NA))
		dt[!is.na(sd), VALUE:=rnorm(.N, mean, sd)]
		return(dt[["VALUE"]])
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
			ss.data[, (paste0("SS", tmp.grade)) := 
				get(paste0("SS", tmp.grade)) + perturb.rnorm(sd=sgpFlow::sgpFlowStateData[[state]][['Achievement']][['CSEM']][[tmp.content_area]][[paste("GRADE", tmp.grade, sep="_")]](ss.data[[paste0("SS", tmp.grade)]]))]
		}

		## Pull in scores to loss and hoss
		tmp.column.name <- paste0("SS", tmp.grade)
		ss.data[get(tmp.column.name) < loss.hoss[1L], (tmp.column.name) := loss.hoss[1L]]
		ss.data[get(tmp.column.name) > loss.hoss[2L], (tmp.column.name) := loss.hoss[2L]]
	}

	## Return perturbed values
	return(ss.data)
} ### END perturbScoresWithCSEM 