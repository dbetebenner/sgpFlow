#' Output Results from sgpFlow Analysis
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
#' @importFrom collapse fmean
#' @export
#' 
outputsgpFlow <-
function(
    sgpFlow_results_list,
    state,
    export.duckdb = TRUE,
    export.Rdata = TRUE
) {

    ## Initialize lists
    tmp.list <- tmp.list.final <- list()

    ## Initialize directory if it doesn't exist
    if (!dir.exists("Data")) {
        dir.create("Data")
    }

    ## Loop through cohort types
    for (cohort.type.iter in names(sgpFlow_results_list)) {
        ## Loop through sgpFlow.config
        for (sgpFlow.config.iter in names(sgpFlow_results_list[[cohort.type.iter]])) {
            ## Loop through growth distributions
            for (growth.distributions.iter in names(sgpFlow_results_list[[cohort.type.iter]][[sgpFlow.config.iter]])) {
                ## Loop through achievement.percentiles.tables
                for (achievement.percentiles.tables.iter in names(sgpFlow_results_list[[cohort.type.iter]][[sgpFlow.config.iter]][[growth.distributions.iter]])) {
                    # Extract content area and grade from config name
                    tmp.content_area.grade <- strsplit(sgpFlow.config.iter, "__")
                    content_area <- tmp.content_area.grade[[1]][1]
                    grade <- gsub("GRADE_", "", tmp.content_area.grade[[1]][2], "")

                    # Add metadata columns to each iteration's data
                    tmp.list[[paste(cohort.type.iter, sgpFlow.config.iter, growth.distributions.iter, achievement.percentiles.tables.iter, sep=".")]] <- 
                    rbindlist(sgpFlow_results_list[[cohort.type.iter]][[sgpFlow.config.iter]][[growth.distributions.iter]][[achievement.percentiles.tables.iter]], fill=TRUE)[,
                        lapply(.SD, collapse::fmean), 
                            .SDcols = data.table::patterns("^SCALE_SCORE"),
                            keyby = "ID"][,
                            `:=`(
                                COHORT_TYPE = cohort.type.iter,
                                CONTENT_AREA = content_area,
                                GRADE = grade,
                                GROWTH_DISTRIBUTION = growth.distributions.iter)]
                }
            }
        }
    }

    ## Combine results into final lists
    for (table.type.iter in c("ENTIRE_COHORT", "ACHIEVEMENT_PERCENTILES")) {
        tmp.list.final[[table.type.iter]] <- rbindlist(tmp.list[grep(table.type.iter, names(tmp.list))], fill=TRUE)
    }

    ## Export results to DuckDB
    if (export.duckdb) {
        ## Create DuckDB connection and database
        con <- duckdb::dbConnect(duckdb::duckdb(), paste0("Data/sgpFlow_", state, ".duckdb"))

        # Write each table to DuckDB
        duckdb::dbWriteTable(con, "ENTIRE_COHORT", tmp.list.final[["ENTIRE_COHORT"]], overwrite = TRUE)
        duckdb::dbWriteTable(con, "ACHIEVEMENT_PERCENTILES", tmp.list.final[["ACHIEVEMENT_PERCENTILES"]], overwrite = TRUE)

        ## Close connection
        duckdb::dbDisconnect(con, shutdown = TRUE)
    }

    if (export.Rdata) {
        assign(paste0("sgpFlow_", state), tmp.list.final)
        save(list=paste0("sgpFlow_", state), file=file.path("Data", paste0("sgpFlow_", state, ".Rdata")))
    }
} ### END outputsgpFlow
