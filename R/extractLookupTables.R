#' @title Extract SGP Lookup Tables
#' @description Extracts percentile and projection lookup tables from an SGP object for use in Student Growth Percentile (SGP) analyses.
#' 
#' This function processes an SGP object to generate two key lookup tables: 
#' - `sgpPercentileLookupTable`: A table containing baseline percentiles and associated scale scores.
#' - `sgpProjectionLookupTable`: A table with projected percentile trajectories and their corresponding scale scores.
#' These tables can be used for further analysis, reporting, or visualization of SGP data.
#' 
#' @param sgp_object An object of class `SGP` containing the data required for extracting lookup tables. The `Data` slot of the SGP object must include columns such as `CONTENT_AREA`, `GRADE`, `YEAR`, `SGP_BASELINE`, and `SGP_NORM_GROUP_BASELINE_SCALE_SCORES`.
#' @returns A list containing two `data.table` objects:
#' - `sgpPercentileLookupTable`: A table of baseline percentiles, scale scores, and related metadata.
#' - `sgpProjectionLookupTable`: A table of projected percentile trajectories with scale scores.
#' @details 
#' - The function identifies the most recent year (`tmp.final.year`) in the data and processes the relevant subset.
#' - It generates `sgpPercentileLookupTable` by extracting baseline percentiles and splitting the `SGP_NORM_GROUP_BASELINE_SCALE_SCORES` column into prior and current scale scores.
#' - The `sgpProjectionLookupTable` is derived from `sgpPercentileLookupTable` by isolating and transforming percentile trajectories and their corresponding scale scores.
#' - The function ensures that both tables are keyed for efficient lookup and manipulation.
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'   # Extract lookup tables from an SGP object
#'   sgp_tables <- extractLookupTables(sgp_object)
#'   
#'   # Access the percentile lookup table
#'   print(sgp_tables$sgpPercentileLookupTable)
#'   
#'   # Access the projection lookup table
#'   print(sgp_tables$sgpProjectionLookupTable)
#' }
#' }
#' @seealso 
#'  \code{\link[data.table]{tstrsplit}}, \code{\link[data.table]{setcolorder}}, \code{\link[data.table]{melt.data.table}}, \code{\link[data.table]{setattr}}, \code{\link[data.table]{setnames}}, \code{\link[data.table]{setkey}}
#' @rdname extractLookupTables
#' @export 
#' @importFrom data.table tstrsplit setcolorder melt.data.table setattr setnames setkey

extractLookupTables <-
    function(
        sgp_object
    ) {
        ### Parameters
        variables.to.keep <- c("CONTENT_AREA", "GRADE", "SGP_BASELINE", grep("PERCENTILE_CUT_BASELINE", names(sgp_object@Data), value = TRUE), "SGP_NORM_GROUP_BASELINE_SCALE_SCORES", "SGP_NORM_GROUP_BASELINE")
        tmp.final.year <- tail(sgp_object@Data[, 1, keyby = c("YEAR")]$YEAR, 1)
        tmp.content.areas <- unique(sgp_object@Data[["CONTENT_AREA"]])

        ### Create sgpPercentileLookupTable
        sgpPercentileLookupTable <- sgp_object@Data[YEAR == tmp.final.year & !is.na(SGP_BASELINE), variables.to.keep, with = FALSE][, c("SCALE_SCORE_PRIOR_1", "SCALE_SCORE") := data.table::tstrsplit(SGP_NORM_GROUP_BASELINE_SCALE_SCORES, "; ", fixed = TRUE)]
        sgpPercentileLookupTable[, `:=`(
            SCALE_SCORE_PRIOR_1 = as.integer(SCALE_SCORE_PRIOR_1),
            SCALE_SCORE = as.integer(SCALE_SCORE)
        )]
        sgpPercentileLookupTable[, SGP_NORM_GROUP_BASELINE_SCALE_SCORES := NULL]
        data.table::setcolorder(sgpPercentileLookupTable, c("CONTENT_AREA", "GRADE", grep("SCALE_SCORE", names(sgpPercentileLookupTable), value = TRUE), "SGP_BASELINE", grep("PERCENTILE_CUT_BASELINE", names(sgpPercentileLookupTable), value = TRUE)))

        ### Create sgpProjectionLookupTable
        sgpProjectionLookupTable <- sgpPercentileLookupTable[!duplicated(sgpPercentileLookupTable, by = c(grep("SCALE_SCORE_PRIOR", names(sgpPercentileLookupTable), value = TRUE), "CONTENT_AREA", "GRADE"))]
        sgpProjectionLookupTable[, GRADE := sub("^.*_[0-9]{4}/[A-Z]+_([0-9]+);.*$", "\\1", SGP_NORM_GROUP_BASELINE)]
        sgpProjectionLookupTable[, c("SCALE_SCORE", "SGP_BASELINE", "SGP_NORM_GROUP_BASELINE") := NULL]
        sgpProjectionLookupTable <- data.table::melt.data.table(sgpProjectionLookupTable, id.vars = c("SCALE_SCORE_PRIOR_1", "CONTENT_AREA", "GRADE"))
        data.table::setattr(sgpProjectionLookupTable[["variable"]], "levels", gsub("PERCENTILE_CUT_BASELINE_", "", levels(sgpProjectionLookupTable[["variable"]])))
        data.table::setnames(sgpProjectionLookupTable, c("variable", "value"), c("PERCENTILE_TRAJECTORY", "PERCENTILE_TRAJECTORY_SCALE_SCORE"))
        sgpProjectionLookupTable[, PERCENTILE_TRAJECTORY := as.integer(as.character(PERCENTILE_TRAJECTORY))]
        data.table::setcolorder(sgpProjectionLookupTable, c("CONTENT_AREA", "GRADE", "SCALE_SCORE_PRIOR_1", "PERCENTILE_TRAJECTORY", "PERCENTILE_TRAJECTORY_SCALE_SCORE"))
        data.table::setnames(sgpProjectionLookupTable, gsub("_BASELINE", "", names(sgpProjectionLookupTable)))
        data.table::setkey(sgpProjectionLookupTable, CONTENT_AREA, GRADE, SCALE_SCORE_PRIOR_1)

        ### Final cleanup of sgpPercentileLookupTable
        sgpPercentileLookupTable[, grep("PERCENTILE_CUT", names(sgpPercentileLookupTable)) := NULL]
        data.table::setnames(sgpPercentileLookupTable, gsub("_BASELINE", "", names(sgpPercentileLookupTable)))
        data.table::setkey(sgpPercentileLookupTable, CONTENT_AREA, GRADE, SCALE_SCORE_PRIOR_1, SCALE_SCORE)

        ### Return tables
        return(list(sgpPercentileLookupTable = sgpPercentileLookupTable, sgpProjectionLookupTable = sgpProjectionLookupTable))
    } ### END extractLookupTables
