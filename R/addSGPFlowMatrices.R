`addSGPFlowMatrices` <-
function(state,
         year,
         cohort_type,
         add.matrices.to.which.state=NULL) {

        SGPstateData <- SGP::SGPstateData ### Needed due to assignment of values to SGPstateData

        ### Utility function
        getMatrixLabel <- function(state, year, cohort_type, add.matrices.to.which.state) {
             if (!paste(state, "sgpFlowMatrices", sep="_") %in% names(sgpFlowMatrices::sgpFlowMatrices)) {
                 stop(paste("\tNOTE: sgpFlow matrices for state:", state, "not in sgpFlowMatrices."))
             }
             tmp.years <- names(sgpFlowMatrices::sgpFlowMatrices[[paste(state, "sgpFlowMatrices", sep="_")]])
             if (year < sort(tmp.years)[1]) stop("NOTE: Supplied year precedes years associated with sgpFlow matrices in package. Contact package developer if you think this is in error.")
             if (!year %in% tmp.years) year <- tmp.years[which(year==sort(c(tmp.years, year)))-1L]
             matrix.label <- paste0("sgpFlowMatrices::sgpFlowMatrices[['", paste(state, "sgpFlowMatrices", sep="_"), "']][['", year, "']][['", cohort_type, "']]")
             message(paste0("\tNOTE: Adding ", SGP::getStateAbbreviation(state, type="Long"), " (", state, ") ", year, " sgpFlow matrices to SGPstateData for state ",
			add.matrices.to.which.state))
             return(matrix.label)
        }

	if (is.null(add.matrices.to.which.state)) add.matrices.to.which.state <- state

        SGPstateData[[add.matrices.to.which.state]][["Baseline_splineMatrix"]][["Coefficient_Matrices"]] <- eval(parse(text=getMatrixLabel(state, year, cohort_type, add.matrices.to.which.state)))
        return(SGPstateData)
} ### END addSGPFlowMatrices
