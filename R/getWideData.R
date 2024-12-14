#' Convert Long Data to Wide Format for Cohort Analysis
#'
#' The `getWideData` function takes long-format student data and converts it into a wide-format data table for cohort-based analyses.
#' It aligns years, grades, and content areas based on the provided `sgpFlow.config` and optionally filters by the specified cohort end year.
#'
#' @param long_data A `data.table` containing student-level data in long format, including variables such as \code{ID}, \code{YEAR}, \code{GRADE}, \code{CONTENT_AREA}, and \code{SCALE_SCORE}.
#' @param sgpFlow.config A list containing configuration parameters for student growth percentile (SGP) analysis, including:
#'   \describe{
#'     \item{\code{year_lags.progression}}{A vector of year lags for cohort progression.}
#'     \item{\code{grade.progression}}{A vector of grade levels for the cohort.}
#'     \item{\code{content_area.progression}}{A vector of content areas for the cohort.}
#'   }
#' @param cohort.end.year An optional integer specifying the cohort's end year. Defaults to the maximum year present in \code{long_data}.
#' @param achievement.percentiles.tables Logical. Indicating whether subset based upon the achievement percentile is performed (99 resulting rows) 
#'
#' @return A wide-format `data.table` containing the following columns:
#'   \describe{
#'     \item{\code{ID}}{Student IDs.}
#'     \item{\code{SSX}}{SCALE_SCORE values for grades specified in \code{sgpFlow.config}, where \code{X} corresponds to the grade.}
#'   }
#'   The final column is filtered to remove rows with \code{NA} values.
#'
#' @details
#' This function performs the following steps:
#' \enumerate{
#'   \item Generates a lookup table for valid cases based on cohort configuration.
#'   \item Joins the long-format data with the lookup table to retain only matching records.
#'   \item Converts the data into a wide format using \code{collapse::pivot}.
#'   \item Filters rows with \code{NA} in the final column of \code{SCALE_SCORE}.
#' }
#'
#' @importFrom data.table SJ setnames setkeyv
#' @importFrom collapse pivot join fselect na_omit
#' @export

getWideData <-
    function(
        long_data,
        sgpFlow.config,
        cohort.end.year = NULL,
        achievement.percentiles.tables
    ) {
        ### Parameters
        if (!is.null(cohort.end.year) & length(cohort.end.year) > 1) stop("Argument cohort.end.year must be of length 1.")
        if (is.null(cohort.end.year)) cohort.end.year <- long_data[, max(YEAR)]
        year.progression <-
            if (length(sgpFlow.config[["year_lags.progression"]]) > 0) {
                yearIncrement(cohort.end.year, sgpFlow.config[["year_lags.progression"]])
            } else {
                cohort.end.year
            }

        cohort_lookup <-
            data.table::SJ(
                VALID_CASE = "VALID_CASE",
                YEAR = year.progression,
                GRADE = sgpFlow.config[["grade.progression"]],
                CONTENT_AREA = sgpFlow.config[["content_area.progression"]]
            )
        new.names <- c("ID", paste0("SCALE_SCORE_GRADE_", sgpFlow.config[["grade.progression"]]))
        vars.to.keep <- c("ID", names(cohort_lookup), "SCALE_SCORE")

        return(
            collapse::pivot(
                how = "wider",
                data =
                    collapse::join(
                        x = collapse::fselect(long_data, vars.to.keep), y = cohort_lookup,
                        how = "inner", on = names(cohort_lookup), verbose = FALSE, overid = 2
                    ),
                ids = "ID", names = c("YEAR", "CONTENT_AREA"),
                values = "SCALE_SCORE"
            ) 
            |>
                data.table::setnames(new.names) |> collapse::na_omit(cols = tail(new.names, 1))
            |>
                data.table::setkey(ID)
        )
    } ## END getWideData