`sgpFlowTrajectories` <- 
function(
	long_data,
	state,
	grade.progression,
	content_area.progression,
	year_lags.progression,
	grade.projection.sequence,
	content_area.projection.sequence,
	year_lags.projection.sequence,
	max.order.for.progression,
	projection.splineMatrices) {


		### Get matrix sequence associated with trajectory calculations
		grade_projection_sequence_matrices <- getGradeProjectionSequenceMatrices(
      					grade.progression,
						content_area.progression,
						year_lags.progression,
						grade.projection.sequence,
						content_area.projection.sequence,
						year_lags.projection.sequence,
						max.order.for.progression,
						projection.splineMatrices)

		### Subset and reshape to wide data for getPercentileTrajectories
		panel_data <- getPanelData(
						long_data,
						grade.progression,
						content_area.progression,
						year_lags.progression)

		### Calculate percentile trajectories
		tmp.trajectories <- getPercentileTrajectories(
								ss.data =  panel_data,
								state = state,
								projection.splineMatrices = grade_projection_sequence_matrices)

		return(tmp.trajectories)
} ### END sgpFlowTrajectories
