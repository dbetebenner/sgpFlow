`sgpFlowTrajectories` <- 
function(
	long_data,
	state,
	sgpFlow.config,
	projection.splineMatrices) {


		### Get matrix sequence associated with trajectory calculations
		grade_projection_sequence_matrices <- getGradeProjectionSequenceMatrices(
						sgpFlow.config,
						projection.splineMatrices)

		### Subset and reshape to wide data for getPercentileTrajectories
		panel_data <- getPanelData(
						long_data,
						sgpFlow.config)

		### Calculate percentile trajectories
		tmp.trajectories <- getPercentileTrajectories(
								ss.data =  panel_data,
								state = state,
								sgpFlow.config = sgpFlow.config,
								projection.splineMatrices = grade_projection_sequence_matrices)

		return(tmp.trajectories)
} ### END sgpFlowTrajectories