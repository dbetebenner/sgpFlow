#' @title Create SGP Coefficient Matrices
#' @description Generates single-cohort and super-cohort Student Growth Percentile (SGP) coefficient matrices using the provided data and configuration settings.
#' 
#' This function processes input data to create coefficient matrices that are essential for baseline SGP calculations. It supports both single-cohort and super-cohort analyses, based on the specified matrix types.
#' 
#' @param data_for_matrices Either a `data.table` or an object of class `SGP` containing the necessary data for matrix construction. If the input is an `SGP` object, the `Data` slot will be used.
#' @param state A character string indicating the state for which matrices are being generated.
#' @param matrix.sgp.config A configuration list specifying the SGP baseline setup for generating coefficient matrices.
#' @param super_cohort.sgp.config A configuration list used specifically for creating super-cohort datasets. Required if `matrix_types` includes `"super-cohort"`.
#' @param super_cohort_base_years A character vector specifying the base years for the super-cohort analysis. If not provided, the function will use all years in the data.
#' @param parallel.config A configuration list for specifying parallel processing options.
#' @param matrix_types A character vector specifying the types of matrices to generate. Options are `"single-cohort"` and `"super-cohort"`. Default: `c("single-cohort", "super-cohort")`.
#' @returns A list of SGP coefficient matrices, with separate entries for each matrix type specified in `matrix_types`.
#' @details 
#' This function processes input data to generate SGP coefficient matrices for both single-cohort and super-cohort analyses. The workflow includes:
#' - Preparing data for SGP calculations using \code{\link[SGP]{prepareSGP}}.
#' - Constructing super-cohort datasets using \code{\link[SGP]{createSuperCohortData}} (if applicable).
#' - Generating SGP baseline matrices using \code{\link[SGP]{baselineSGP}} with the provided configuration settings.
#' 
#' The resulting matrices can be used for subsequent baseline SGP analyses or stored for further use.
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'   DEMO_sgpFlow_Matrices <- createMatrices(
#'      data_for_matrices = SGPdata::sgpData_LONG,
#'      state = "DEMO",
#'      matrix.sgp.config = DEMO_Matrix.config,
#'      super_cohort.sgp.config = DEMO_Super_Cohort.config,
#'      parallel.config=list(BACKEND = "PARALLEL", WORKERS=list(TAUS=num_cores)),
#'      matrix_types=c("single-cohort", "super-cohort")
#'   )
#'   save(DEMO_sgpFlow_Matrices, file="Data/DEMO_sgpFlow_Matrices.rda", compress="xz")
#' }
#' }
#' @seealso 
#'  \code{\link[SGP]{createSuperCohortData}}, \code{\link[SGP]{prepareSGP}}, \code{\link[SGP]{baselineSGP}}
#' @rdname createMatrices
#' @importFrom SGP prepareSGP baselineSGP
#' @export 

createMatrices <-
    function(
        data_for_matrices, ### Either data.table or object of class SGP
        state = NULL,
        matrix.sgp.config,
        super_cohort.sgp.config,
        super_cohort_base_years,
        parallel.config,
        matrix_types = c("single-cohort", "super-cohort")
    ) {
        # Parameters
        sgpFlowMatrices.list <- list()

        # Test and update arguments
        if ("SGP" %in% class(data_for_matrices)) data_for_matrices <- data_for_matrices@Data
        if ("super-cohort" %in% matrix_types & missing(super_cohort.sgp.config)) stop("Super-cohort analysis requires super_cohort.sgp.config for data set construction.")

        if (is.null(state)) {
            tmp.name <- toupper(gsub("_", " ", deparse(substitute(data_for_matrices))))
            state <- getStateAbbreviation(tmp.name, "sgpFlow")
        }

        # Loop over matrix types
        for (matrix_type.iter in matrix_types) {
            matrix_type.name <- toupper(gsub("-", "_", matrix_type.iter))

            if (matrix_type.iter == "single-cohort") {
                tmp.data <- data_for_matrices
            }
            if (matrix_type.iter == "super-cohort") {
                tmp.data <- createSuperCohortData(
                    base_data = data_for_matrices[, .(VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID, SCALE_SCORE)],
                    sgp.config = super_cohort.sgp.config,
                    super_cohort_base_years = super_cohort_base_years
                )
            }

            ### Create Baseline Matrices
            TEMP_SGP <- SGP::prepareSGP(tmp.data, state = state, create.additional.variables = FALSE, create.achievement.level = FALSE)

            sgpFlowMatrices.list[[matrix_type.name]] <- SGP::baselineSGP(
                TEMP_SGP,
                state = state,
                sgp.baseline.config = matrix.sgp.config,
                return.matrices.only = TRUE,
                sgp.quantiles = seq.int(99)/100,
                calculate.baseline.sgps = FALSE,
                goodness.of.fit.print = FALSE,
                parallel.config = parallel.config
            )
        }

        return(sgpFlowMatrices.list)
    } ### END createMatrices
