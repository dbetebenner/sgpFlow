#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param long_data PARAM_DESCRIPTION
#' @param state PARAM_DESCRIPTION
#' @param sgpFlow.config PARAM_DESCRIPTION
#' @param cohort_end_year PARAM_DESCRIPTION, Default: NULL
#' @param growth.distribution PARAM_DESCRIPTION, Default: NULL
#' @param csem.perturbation.of.initial.scores PARAM_DESCRIPTION, Default: TRUE
#' @param csem.perturbation.iterations PARAM_DESCRIPTION, Default: 100
#' @param projection.splineMatrices PARAM_DESCRIPTION
#' @returns OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sgpFlowTrajectories
#' @export 

sgpFlowTrajectories <-
    function(
        long_data,
        state,
        sgpFlow.config,
        cohort_end_year = NULL,
        growth.distribution = NULL,
        csem.perturbation.of.initial.scores = TRUE,
        csem.perturbation.iterations = 100L,
        projection.splineMatrices
    ) {
        ### Get matrix sequence associated with trajectory calculations
        # grade_projection_sequence_matrices <- getGradeProjectionSequenceMatrices(
        #                                                                 sgpFlow.config,
        #                                                                 projection.splineMatrices)

        ### Subset and reshape to wide data for getPercentileTrajectories
        # panel_data <- getWideData(
        #                     long_data = long_data,
        #                     sgpFlow.config = sgpFlow.config,
        #                     cohort_end_year = cohort_end_year)

        ### Calculate percentile trajectories
        sgpFlow.trajectories <-
            getPercentileTrajectories(
                wide_data = # Subset and reshape to wide data for getPercentileTrajectories
                    getWideData(
                        long_data = long_data,
                        sgpFlow.config = sgpFlow.config,
                        cohort_end_year = cohort_end_year
                    ),
                state = state,
                sgpFlow.config = sgpFlow.config,
                growth.distribution = growth.distribution,
                csem.perturbation.of.initial.scores = csem.perturbation.of.initial.scores,
                csem.perturbation.iterations = csem.perturbation.iterations,
                projection.splineMatrices = # Get matrix sequence associated with trajectory calculations
                    getGradeProjectionSequenceMatrices(
                        sgpFlow.config,
                        projection.splineMatrices
                    )
            )

        ### Return trajectories
        return(sgpFlow.trajectories)
    } ### END sgpFlowTrajectories
