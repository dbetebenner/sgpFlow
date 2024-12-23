#' @title Create Super-Cohort Data
#' @description Constructs super-cohort data by collapsing and reassigning years in the provided base data, based on SGP configuration and specified base years.
#' 
#' This function prepares super-cohort datasets, which are used for analyses involving students grouped across multiple years. It adjusts year mappings, applies grade sequences, and optionally assigns cohort indicators.
#' 
#' @param base_data A `data.table` containing the base data with columns `YEAR`, `CONTENT_AREA`, `GRADE`, and `ID`.
#' @param sgp.config A list of SGP configurations specifying `sgp.content.areas`, `sgp.grade.sequences`, and `sgp.grade.sequences.lags`.
#' @param super_cohort_base_years A vector of years (subset of `base_data$YEAR`) to include for super-cohort construction. If not specified, all years in `base_data` are used.
#' @param indicate_cohort A logical value indicating whether to include a `COHORT` column in the output, which assigns a label for each super-cohort. Default: `FALSE`.
#' @returns A `data.table` with adjusted year mappings and optionally a cohort indicator. The output combines data across all specified configurations and collapses it into a super-cohort structure.
#' @details 
#' - The function verifies that `super-cohort_base_years` are present in `base_data`.
#' - Each SGP configuration is processed iteratively, and grade-year-content mappings are applied to create super-cohort datasets.
#' - The `YEAR` column is updated based on the most recent year in the grade sequence (`YEAR_NEW`).
#' - Duplicate records are removed by keeping only the most recent (last) instance for each unique combination of `YEAR`, `GRADE`, and `ID`.
#' - The optional `COHORT` column provides a unique identifier for each super-cohort, formed by concatenating `CONTENT_AREA`, `YEAR`, and `GRADE`.
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'   # Example usage
#'   super_cohort_data <- createSuperCohortData(
#'     base_data = SGPdata::sgpData_LONG[, .(VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID, ACHIEVEMENT_LEVEL)],
#'     sgp.config = sgp_config_list,
#'     super_cohort_base_years = c("2021_2022", "2022_2023", "2023_2024"),
#'     indicate_cohort = TRUE
#'   )
#'   print(super_cohort_data)
#' }
#' }
#' @seealso 
#'  \code{\link[data.table]{data.table}}, \code{\link[data.table]{rbindlist}}, \code{\link[data.table]{setkey}}
#' @rdname createSuperCohortData
#' @export 
#' @importFrom data.table data.table rbindlist setkey

createSuperCohortData <-
    function(
        base_data,
        sgp.config,
        super_cohort_base_years,
        indicate_cohort = FALSE
    ) {
        YEAR <- CONTENT_AREA <- GRADE <- YEAR_NEW <- COHORT <- ID <- NULL

        ### Parameters
        data.years <- sort(unique(base_data[["YEAR"]]))
        tmp.cohort.list <- list()

        ### Test parameters
        if (!missing(super_cohort_base_years) && !all(super_cohort_base_years %in% data.years)) stop("Note: super_cohort_base_years supplied not all in years provided in base_data.")

        ### Use super_cohort_base_years to filter data if it is provided.
        if (!missing(super_cohort_base_years)) base_data <- base_data[YEAR %in% super_cohort_base_years]

        ### Loop over configurations
        for (sgp.config.iter in sgp.config) {
            tmp.list <- list()
            for (data.years.iter in 1:(length(data.years) - sum(sgp.config.iter[["sgp.grade.sequences.lags"]]))) {
                grade_year_content_area_map <- data.table::data.table(
                    CONTENT_AREA = sgp.config.iter[["sgp.content.areas"]],
                    YEAR = data.years[c(data.years.iter, data.years.iter + (cumsum(sgp.config.iter[["sgp.grade.sequences.lags"]])))],
                    GRADE = sgp.config.iter[["sgp.grade.sequences"]],
                    YEAR_NEW = tail(data.years, length(sgp.config.iter[["sgp.content.areas"]]))
                )

                tmp.list[[data.years.iter]] <- base_data[grade_year_content_area_map, on = list(CONTENT_AREA, YEAR, GRADE)][, YEAR := YEAR_NEW][, YEAR_NEW := NULL]

                if (indicate_cohort) {
                    tmp.list[[data.years.iter]][, COHORT := paste(unlist(grade_year_content_area_map[, 1:3]), collapse = "_")]
                }

                tmp.dt <- data.table::rbindlist(tmp.list)
                data.table::setkey(tmp.dt, YEAR, GRADE, ID)
                tmp.dt <- tmp.dt[unique(tmp.dt[, .(YEAR, GRADE, ID)]), mult = "last"] ### Remove duplicates created by collapsing data into YEAR_NEW taking LAST (most recent) case
            }

            tmp.cohort.list[[paste(sgp.config.iter[["sgp.content.areas"]][1], paste(sgp.config.iter[["sgp.grade.sequences"]], collapse = ""), sep = "_")]] <- tmp.dt
        }
        return(data.table::rbindlist(tmp.cohort.list))
    } ### END createSuperCohortData
