`getGradeProjectionSequenceMatrices` <- 
function(
	grade.progression,
	content_area.progression,
	year_lags.progression,
	grade.projection.sequence,
	content_area.projection.sequence,
	year_lags.projection.sequence,
        max.order.for.progression,
        projection.splineMatrices
) {

		tmp.list <- list()
        grade.progression.index <- seq_along(grade.progression)
		for (i in seq_along(grade.progression.index)) {
			tmp.list[[i]] <- list()
			for (j in seq_along(grade.projection.sequence)) {
				tmp.years_lags <- c(tail(year_lags.progression, grade.progression.index[i]-1L), head(year_lags.projection.sequence, j))
				tmp.years <- rep("BASELINE", length(tmp.years_lags)+1L)
				tmp.matrix <- SGP:::getsplineMatrices(
						projection.splineMatrices,
						c(tail(content_area.progression, grade.progression.index[i]), head(content_area.projection.sequence, j)),
						c(tail(grade.progression, grade.progression.index[i]), head(grade.projection.sequence, j)),
						tmp.years,
						tmp.years_lags,
						return.highest.order.matrix=TRUE,
						my.matrix.highest.order=max.order.for.progression,
						my.matrix.time.dependency=NULL)

				tmp.list[[i]][[j]] <- tmp.matrix[[1L]]
			} ## END i loop
		} ## END j loop

		return(rev(tmp.list)) ### rev gives highest orders first
} ### END getGradeProjectionSequenceMatrices
