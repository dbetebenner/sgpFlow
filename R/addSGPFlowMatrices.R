#' Add SGPFlow Matrices to SGPstateData
#'
#' This function adds SGPFlow matrices to the SGPstateData environment.
#' It retrieves the appropriate matrices based on the state, year, and cohort type,
#' and assigns them to the SGPstateData object.
#'
#' @param state The state abbreviation (e.g., "IN", "DEMO")
#' @param year The year for which to add the matrices
#' @param cohort_type The type of cohort (e.g., "EOC", "EOC_CSEM", "EOC_CSEM_CSEM")
#' @param add.matrices.to.which.state The state to which to add the matrices
#'
#' @return The updated SGPstateData object with the added matrices
#' @note This function is not exported and is intended for internal use only.
#' @keywords internal

addsgpFlowMatrices <-
    function(
        state,
        year,
        cohort_type,
        add.matrices.to.which.state = NULL
    ) {
        SGPstateData <- SGP::SGPstateData ### Needed due to assignment of values to SGPstateData

        ### Utility function
        getMatrixLabel <- function(state, year, cohort_type, add.matrices.to.which.state) {
            if (!paste(state, "sgpFlowMatrices", sep = "_") %in% names(sgpFlowMatrices::sgpFlowMatrices)) {
                stop(paste("\tNOTE: sgpFlow matrices for state:", state, "not in sgpFlowMatrices."))
            }
            tmp.years <- names(sgpFlowMatrices::sgpFlowMatrices[[paste(state, "sgpFlowMatrices", sep = "_")]])
            if (year < sort(tmp.years)[1]) stop("NOTE: Supplied year precedes years associated with sgpFlow matrices in package. Contact package developer if you think this is in error.")
            if (!year %in% tmp.years) year <- tmp.years[which(year == sort(c(tmp.years, year))) - 1L]
            matrix.label <- paste0("sgpFlowMatrices::sgpFlowMatrices[['", paste(state, "sgpFlowMatrices", sep = "_"), "']][['", year, "']][['", cohort_type, "']]")
            message(paste0(
                "\tNOTE: Adding ", SGP::getStateAbbreviation(state, type = "Long"), " (", state, ") ", year, " sgpFlow matrices to SGPstateData for state ",
                add.matrices.to.which.state
            ))
            return(matrix.label)
        }

        if (is.null(add.matrices.to.which.state)) add.matrices.to.which.state <- state

        SGPstateData[[add.matrices.to.which.state]][["Baseline_splineMatrix"]][["Coefficient_Matrices"]] <- eval(parse(text = getMatrixLabel(state, year, cohort_type, add.matrices.to.which.state)))
        return(SGPstateData)
    } ### END addsgpFlowMatrices
