`sgpFlowTrajectories` <-
function(
	long_data,
	state,
	sgpFlow.config,
	cohort_end_year=NULL,
	growth.distribution=NULL,
	csem.perturbation.of.initial.scores=TRUE,
	csem.perturbation.iterations=100L,
	projection.splineMatrices) {


		### Get matrix sequence associated with trajectory calculations
		# grade_projection_sequence_matrices <- getGradeProjectionSequenceMatrices(
		# 																sgpFlow.config,
		# 																projection.splineMatrices)

		### Subset and reshape to wide data for getPercentileTrajectories
		# panel_data <- getWideData(
		# 					long_data = long_data,
		# 					sgpFlow.config = sgpFlow.config,
		# 					cohort_end_year = cohort_end_year)

		### Calculate percentile trajectories
		sgpFlow.trajectories <-
		    getPercentileTrajectories(
				wide_data = # Subset and reshape to wide data for getPercentileTrajectories
					getWideData(
						long_data = long_data,
						sgpFlow.config = sgpFlow.config,
						cohort_end_year = cohort_end_year),
				state = state,
				sgpFlow.config = sgpFlow.config,
				growth.distribution = growth.distribution,
				csem.perturbation.of.initial.scores = csem.perturbation.of.initial.scores,
				csem.perturbation.iterations = csem.perturbation.iterations,
				projection.splineMatrices = # Get matrix sequence associated with trajectory calculations
					getGradeProjectionSequenceMatrices(
						sgpFlow.config,
						projection.splineMatrices)
			)

		### Return trajectories
		return(sgpFlow.trajectories)
} ### END sgpFlowTrajectories