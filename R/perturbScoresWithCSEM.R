#' @importFrom data.table data.table
#' @importFrom stats rnorm

perturbScoresWithCSEM <-
    function(
        wide_data,
        state,
        sgpFlow.config,
        distribution = "Normal"
    ) {

        ## Utility functions
        perturb.rnorm <- function(mean = 0, sd) {
            dt <-
                data.table::data.table(sd = sd, VALUE = as.numeric(NA))[!is.na(sd), VALUE := stats::rnorm(.N, mean, sd)]
            return(dt[["VALUE"]])
        }

        ## Parameters
        distribution <- toupper(distribution)
        supported.distributions <- c("NORMAL")

        ## Define relevant variables
        if (!distribution %in% supported.distributions) {
            stop(paste0("Distribution supplied (", distribution, ") not currently supported."))
        }

        ## Loop over grades in grade.progression
        for (grade.iter in seq_along(sgpFlow.config[["grade.progression"]])) {
            tmp.grade <- sgpFlow.config[["grade.progression"]][grade.iter]
            tmp.content_area <- sgpFlow.config[["content_area.progression"]][grade.iter]

            if (!is.null(state)) {
                loss.hoss <- get.loss.hoss(state, tmp.content_area, tmp.grade)
            } else {
                loss.hoss <- range(wide_data[[paste0("SCALE_SCORE_GRADE_", tmp.grade)]], na.rm = TRUE)
            }

            if (distribution == "NORMAL") {
                wide_data[, (paste0("SCALE_SCORE_GRADE_", tmp.grade)) :=
                    get(paste0("SCALE_SCORE_GRADE_", tmp.grade)) +
                    perturb.rnorm(sd = sgpFlow::sgpFlowStateData[[state]][["Achievement"]][["CSEM"]][[tmp.content_area]][[paste("GRADE", tmp.grade, sep = "_")]](wide_data[[paste0("SCALE_SCORE_GRADE_", tmp.grade)]]))]
            }

            ## Pull in scores to loss and hoss
            tmp.column.name <- paste0("SCALE_SCORE_GRADE_", tmp.grade)
            wide_data[
                get(tmp.column.name) < loss.hoss[1L], (tmp.column.name) := loss.hoss[1L]
            ][get(tmp.column.name) > loss.hoss[2L], (tmp.column.name) := loss.hoss[2L]]
        }

        ## Return perturbed values
        return(wide_data)
    } ### END perturbScoresWithCSEM
