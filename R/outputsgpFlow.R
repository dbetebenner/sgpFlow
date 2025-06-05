#' @title Output Results from sgpFlow Analysis
#'
#' @description
#' This function processes the nested list structure returned by sgpFlow() and combines the results 
#' into a single data.table. It adds metadata columns for cohort type, content area, grade, and
#' growth distribution type. The function can output results to both DuckDB and/or R data formats.
#'
#' @param sgpFlow_results_list A list containing the results from sgpFlow analysis
#' @param state A character string indicating the state for which trajectories are computed, used for state-specific configurations in the `sgpFlow` package.
#' @param export.duckdb Logical. If TRUE, exports results to a DuckDB database. Default is TRUE.
#' @param export.Rdata Logical. If TRUE, saves results as an R data file. Default is TRUE.
#'
#' @details
#' This function processes the nested list structure returned by sgpFlow() and combines the results 
#' into a single data.table. It adds metadata columns for cohort type, content area, grade, and
#' growth distribution type. The function can output results to both DuckDB and/or R data formats.
#'
#' @return A data.table containing the processed and combined results from sgpFlow analysis
#'
#' @importFrom data.table patterns rbindlist
#' @importFrom collapse unlist2d
#' @export

outputsgpFlow <-
    function(
        sgpFlow_results_list,
        state,
        export.duckdb = TRUE,
        export.Rdata = TRUE
    ) {

    ## Get state name from state abbreviation
    state.name <- getStateAbbreviation(state, type="FULL_NAME")

    ## Names of added fields to exported large data.tables
    variables.to.add <- c("YEAR", "COHORT_TYPE", "GROWTH_DISTRIBUTION", "GROWTH_DISTRIBUTION_TYPE")

    ## Loop through grade x content area groups
    for (sgpFlow.config.iter in names(sgpFlow_results_list)) {
        assign(paste0(state.name, "_sgpFlow_", sgpFlow.config.iter), collapse::unlist2d(sgpFlow_results_list[[sgpFlow.config.iter]], DT=TRUE))
    } ## END sgpFlow.config.iter loop

    ## Export results to DuckDB
    if (export.duckdb) {
        ## Create DuckDB connection and database
        con <- duckdb::dbConnect(duckdb::duckdb(), paste0("Data/", state.name, "_sgpFlow.duckdb"))

        # Write each table to DuckDB
        duckdb::dbWriteTable(con, "ENTIRE_COHORT", tmp.list.final[["ENTIRE_COHORT"]], overwrite = TRUE)
        duckdb::dbWriteTable(con, "ACHIEVEMENT_PERCENTILES", tmp.list.final[["ACHIEVEMENT_PERCENTILES"]], overwrite = TRUE)

        ## Close connection
        duckdb::dbDisconnect(con, shutdown = TRUE)
    }

    if (export.Rdata) {
        assign(paste0(state.name, "_sgpFlow"), tmp.list.final)
        save(list=paste0(state.name, "_sgpFlow"), file=file.path("Data", paste0(state.name, "_sgpFlow.Rdata")))
    }
} ### END outputsgpFlow
