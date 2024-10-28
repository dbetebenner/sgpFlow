`createMatrices` <-
function(
    data_for_matrices,### Either data.table or object of class SGP
    state,
    matrix.sgp.config,
    super_cohort.sgp.config,
    matrix_types=c("single-cohort", "super-cohort")
) {
    # Parameters
    sgpFlowMatrices.list <- list()

    # Test arguments
    if ("SGP" %in% class(data_for_matrices)) data_for_matrices <- data_for_matrices@Data
    if ("super-cohort" %in% matrix_types & missing(super_cohort.sgp.config)) stop("Super-cohort analysis requires super_cohort.sgp.config for data set construction.")


    # Loop over matrix types
    for (matrix_type.iter in matrix_types) {
        matrix_type.name <- toupper(gsub("-", "_", matrix_type.iter))

        if (matrix_type.iter=="single-cohort") {
            tmp.data <- data_for_matrices
        }
        if (matrix_type.iter=="super-cohort") {
            tmp.data <- SGP::createSuperCohortData(
                        base_data=data_for_matrices[, .(VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID, SCALE_SCORE)],
                        sgp.config=super_cohort.sgp.config)
        }

        ### Create Baseline Matrices
        TEMP_SGP <- prepareSGP(tmp.data, state=state, create.additional.variables=FALSE, create.achievement.level=FALSE)

        sgpFlowMatrices.list[[matrix_type.name]] <- baselineSGP(
				TEMP_SGP,
                state=state,
				sgp.baseline.config=matrix.sgp.config,
				return.matrices.only=TRUE,
				calculate.baseline.sgps=FALSE,
				goodness.of.fit.print=FALSE,
				parallel.config = list(BACKEND="PARALLEL", WORKERS=list(TAUS=8)))
    }
    return(sgpFlowMatrices.list)
} ### END createMatrices
