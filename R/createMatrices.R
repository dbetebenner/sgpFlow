#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data_for_matrices PARAM_DESCRIPTION
#' @param state PARAM_DESCRIPTION
#' @param matrix.sgp.config PARAM_DESCRIPTION
#' @param super_cohort.sgp.config PARAM_DESCRIPTION
#' @param parallel.config PARAM_DESCRIPTION
#' @param matrix_types PARAM_DESCRIPTION, Default: c("single-cohort", "super-cohort")
#' @returns OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [createSuperCohortData][SGP::createSuperCohortData], [prepareSGP][SGP::prepareSGP], [baselineSGP][SGP::baselineSGP]
#' @rdname createMatrices
#' @export 
#' @importFrom SGP prepareSGP baselineSGP

createMatrices <-
    function(
        data_for_matrices, ### Either data.table or object of class SGP
        state,
        matrix.sgp.config,
        super_cohort.sgp.config,
        parallel.config,
        matrix_types = c("single-cohort", "super-cohort")
    ) {
        # Parameters
        sgpFlowMatrices.list <- list()

        # Test arguments
        if ("SGP" %in% class(data_for_matrices)) data_for_matrices <- data_for_matrices@Data
        if ("super-cohort" %in% matrix_types & missing(super_cohort.sgp.config)) stop("Super-cohort analysis requires super_cohort.sgp.config for data set construction.")


        # Loop over matrix types
        for (matrix_type.iter in matrix_types) {
            matrix_type.name <- toupper(gsub("-", "_", matrix_type.iter))

            if (matrix_type.iter == "single-cohort") {
                tmp.data <- data_for_matrices
            }
            if (matrix_type.iter == "super-cohort") {
                tmp.data <- createSuperCohortData(
                    base_data = data_for_matrices[, .(VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID, SCALE_SCORE)],
                    sgp.config = super_cohort.sgp.config
                )
            }

            ### Create Baseline Matrices
            TEMP_SGP <- SGP::prepareSGP(tmp.data, state = state, create.additional.variables = FALSE, create.achievement.level = FALSE)

            sgpFlowMatrices.list[[matrix_type.name]] <- SGP::baselineSGP(
                TEMP_SGP,
                state = state,
                sgp.baseline.config = matrix.sgp.config,
                return.matrices.only = TRUE,
                calculate.baseline.sgps = FALSE,
                goodness.of.fit.print = FALSE,
                parallel.config = parallel.config
            )
        }

        return(sgpFlowMatrices.list)
    } ### END createMatrices
