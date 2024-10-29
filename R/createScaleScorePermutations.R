`createScaleScorePermutations` <- 
function(state,
    sgp.config
) {

    ### Utility function
    convert_to_long <- function(wide.dt, content_area_sequence, year_sequence, grade_sequence) {
        tmp.dt.list <- list()
        for (num_years.iter in seq_along(grade_sequence)) {
            tmp.dt.list[[num_years.iter]] <- data.table(VALID_CASE="VALID_CASE", CONTENT_AREA=content_area_sequence[num_years.iter], YEAR=year_sequence[num_years.iter], GRADE=grade_sequence[num_years.iter], ID=paste(seq(dim(wide.dt)[1]), paste(grade_sequence, collapse=""), sep="_"), SCALE_SCORE=wide.dt[[num_years.iter]])
        }
        return(rbindlist(tmp.dt.list))
    }

    ### Create table with all scale score permutations 
    tmp.dt.list <- list()
    for (sgp.config.iter in seq_along(sgp.config)) {
        for (grade_sequence.iter in seq_along(sgp.config[[sgp.config.iter]][['sgp.grade.sequences']])) {
            grade_sequence <- tail(sgp.config[[sgp.config.iter]][['sgp.grade.sequences']][grade_sequence.iter][[1]], 3)
            content_area_sequence <- tail(sgp.config[[sgp.config.iter]][['sgp.content.areas']], length(grade_sequence))
            year_sequence <- tail(sgp.config[[sgp.config.iter]][['sgp.panel.years']], length(grade_sequence))
            scale_score_range_list <- list()
            for (scale_score_range_list.iter in seq_along(grade_sequence)) {
                tmp.scale.range <- SGP::SGPstateData[[state]][['Achievement']][['Knots_Boundaries']][[content_area_sequence[1]]][[paste("loss.hoss", grade_sequence[scale_score_range_list.iter], sep="_")]]
                scale_score_range_list[[scale_score_range_list.iter]] <- as.numeric(seq(tmp.scale.range[1], tmp.scale.range[2]))
            }

            tmp.dt.list[[paste(names(sgp.config)[sgp.config.iter], paste(sgp.config[[sgp.config.iter]][['sgp.grade.sequences']][[grade_sequence.iter]], collapse=""), sep="_")]] <- 
                convert_to_long(do.call(CJ, scale_score_range_list), content_area_sequence, year_sequence, grade_sequence)
        }
    }
    
    return(rbindlist(tmp.dt.list))
} ### END createScaleScorePermutations
