`extractLookupTables` <- 
function(
    sgp_object) {

    ### Parameters
    variables.to.keep <- c("CONTENT_AREA", "GRADE", "SGP_BASELINE", grep("PERCENTILE_CUT_BASELINE", names(sgp_object@Data), value=TRUE), "SGP_NORM_GROUP_BASELINE_SCALE_SCORES", "SGP_NORM_GROUP_BASELINE")
    tmp.final.year <- tail(sgp_object@Data[,1,keyby=c("YEAR")]$YEAR, 1)
    tmp.content.areas <- unique(sgp_object@Data[['CONTENT_AREA']])

    ### Create sgpPercentileLookupTable 
    sgpPercentileLookupTable <- sgp_object@Data[YEAR==tmp.final.year & !is.na(SGP_BASELINE), variables.to.keep, with=FALSE][,c("SCALE_SCORE_PRIOR_1", "SCALE_SCORE"):=tstrsplit(SGP_NORM_GROUP_BASELINE_SCALE_SCORES, "; ", fixed=TRUE)]
    sgpPercentileLookupTable[, `:=`(
          SCALE_SCORE_PRIOR_1 = as.integer(SCALE_SCORE_PRIOR_1),
          SCALE_SCORE = as.integer(SCALE_SCORE))]
    sgpPercentileLookupTable[,SGP_NORM_GROUP_BASELINE_SCALE_SCORES:=NULL]
    setcolorder(sgpPercentileLookupTable, c("CONTENT_AREA", "GRADE", grep("SCALE_SCORE", names(sgpPercentileLookupTable), value=TRUE), "SGP_BASELINE", grep("PERCENTILE_CUT_BASELINE", names(sgpPercentileLookupTable), value=TRUE)))

    ### Create sgpProjectionLookupTable
    sgpProjectionLookupTable <- sgpPercentileLookupTable[!duplicated(sgpPercentileLookupTable, by=c(grep("SCALE_SCORE_PRIOR", names(sgpPercentileLookupTable), value=TRUE), "CONTENT_AREA", "GRADE"))]
    sgpProjectionLookupTable[, GRADE:=sub("^.*_[0-9]{4}/[A-Z]+_([0-9]+);.*$", "\\1", SGP_NORM_GROUP_BASELINE)]
    sgpProjectionLookupTable[,c("SCALE_SCORE", "SGP_BASELINE", "SGP_NORM_GROUP_BASELINE"):=NULL]
    sgpProjectionLookupTable <- melt.data.table(sgpProjectionLookupTable, id.vars=c("SCALE_SCORE_PRIOR_1", "CONTENT_AREA", "GRADE"))
    setattr(sgpProjectionLookupTable[['variable']], "levels", gsub("PERCENTILE_CUT_BASELINE_", "", levels(sgpProjectionLookupTable[['variable']])))
    setnames(sgpProjectionLookupTable, c("variable", "value"), c("PERCENTILE_TRAJECTORY", "PERCENTILE_TRAJECTORY_SCALE_SCORE"))
    sgpProjectionLookupTable[,PERCENTILE_TRAJECTORY:=as.integer(as.character(PERCENTILE_TRAJECTORY))]
    setcolorder(sgpProjectionLookupTable, c("CONTENT_AREA", "GRADE", "SCALE_SCORE_PRIOR_1", "PERCENTILE_TRAJECTORY", "PERCENTILE_TRAJECTORY_SCALE_SCORE"))
    setnames(sgpProjectionLookupTable, gsub("_BASELINE", "", names(sgpProjectionLookupTable)))
    setkey(sgpProjectionLookupTable, CONTENT_AREA, GRADE, SCALE_SCORE_PRIOR_1)

    ### Final cleanup of sgpPercentileLookupTable
    sgpPercentileLookupTable[,grep("PERCENTILE_CUT", names(sgpPercentileLookupTable)):=NULL]
    setnames(sgpPercentileLookupTable, gsub("_BASELINE", "", names(sgpPercentileLookupTable)))
    setkey(sgpPercentileLookupTable, CONTENT_AREA, GRADE, SCALE_SCORE_PRIOR_1, SCALE_SCORE)

    ### Return tables
    return(list(sgpPercentileLookupTable=sgpPercentileLookupTable, sgpProjectionLookupTable=sgpProjectionLookupTable))
} ### END extractLookupTables