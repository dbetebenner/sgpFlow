#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param state PARAM_DESCRIPTION
#' @param sgp.config PARAM_DESCRIPTION
#' @param permutations PARAM_DESCRIPTION, Default: 'ALL'
#' @returns OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [melt][data.table::melt], [data.table][data.table::data.table], [set][data.table::set], [setcolorder][data.table::setcolorder], [rbindlist][data.table::rbindlist]
#'  [sgpFlowStateData][sgpFlow::sgpFlowStateData]
#' @rdname createScaleScorePermutations
#' @importFrom data.table melt data.table set setcolorder rbindlist

createScaleScorePermutations <-
    function(
        state,
        sgp.config,
        permutations = "ALL"
    ) {
        ### Utility function
        convert_to_long <- function(wide.dt, content_area_sequence, year_sequence, grade_sequence) {
            long.dt <- data.table::melt(
                wide.dt,
                measure.vars = names(wide.dt),
                variable.name = "TEMP",
                value.name = "SCALE_SCORE"
            )

            long.dt[, INDEX := as.integer(INDEX)] ### Converts factor INDEX to underlying positive integers

            tmp.dt <- data.table::data.table(
                INDEX = seq_along(year_sequence),
                GRADE = grade_sequence,
                YEAR = year_sequence,
                CONTENT_AREA = content_area_sequence,
                VALID_CASE = "VALID_CASE"
            )

            long.dt <- tmp.dt[long.dt, on = "INDEX"]

            data.table::set(long.dt, j = "INDEX", value = NULL)
            data.table::setcolorder(long.dt, c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", "SCALE_SCORE"))

            return(long.dt)
        }

        ### Create table with all or observed scale score permutations
        tmp.dt.list <- list()
        for (sgp.config.iter in seq_along(sgp.config)) {
            for (grade_sequence.iter in seq_along(sgp.config[[sgp.config.iter]][["sgp.grade.sequences"]])) {
                grade_sequence <- tail(sgp.config[[sgp.config.iter]][["sgp.grade.sequences"]][grade_sequence.iter][[1]], 3)
                content_area_sequence <- tail(sgp.config[[sgp.config.iter]][["sgp.content.areas"]], length(grade_sequence))
                year_sequence <- tail(sgp.config[[sgp.config.iter]][["sgp.panel.years"]], length(grade_sequence))
                scale_score_range_list <- list()
                for (scale_score_range_list.iter in seq_along(grade_sequence)) {
                    tmp.scale.range <- sgpFlow::sgpFlowStateData[[state]][["Achievement"]][["Knots_Boundaries"]][[content_area_sequence[1]]][[paste("loss.hoss", grade_sequence[scale_score_range_list.iter], sep = "_")]]
                    scale_score_range_list[[scale_score_range_list.iter]] <- as.numeric(seq(tmp.scale.range[1], tmp.scale.range[2]))
                }

                tmp.dt.list[[paste(names(sgp.config)[sgp.config.iter], paste(sgp.config[[sgp.config.iter]][["sgp.grade.sequences"]][[grade_sequence.iter]], collapse = ""), sep = "_")]] <-
                    convert_to_long(do.call(CJ, scale_score_range_list), content_area_sequence, year_sequence, grade_sequence)
            }
        }

        return(data.table::rbindlist(tmp.dt.list))
    } ### END createScaleScorePermutations
