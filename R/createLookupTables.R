#' @title Create Lookup Tables for SGP Analysis
#' @description This function generates lookup tables from sgpFlow analyses. It allows for the creation of percentile and projection lookup tables based on the provided state, configuration settings, provided sgpFlow matrices.
#' @param state A character string acronymn indicating the state for which the lookup tables are being created.
#' @param sgp.config A list containing the sgpFlow configuration settings, including panel years and other required parameters.
#' @param parallel.config A list specifying parallel computation configurations for SGP calculations.
#' @param matrices A list of coefficient matrices for the sgpFlow analyses. If not provided, the function attempts to retrieve matrices from the `sgpFlowMatrices` package. Default: `NULL`.
#' @param lookup_table_types A character vector specifying the types of lookup tables to generate. Options include `"single-cohort"` and `"super-cohort"`. Default: `c("single-cohort", "super-cohort")`.
#' @returns A set of lookup tables saved as `.Rdata` and `.csv.gzip` files in a `Data` directory, organized by the type of lookup table.
#' @details 
#' This function:
#' - Validates the availability of necessary matrices.
#' - Adds matrices to state-level SGP configuration data.
#' - Generates scale score permutations and embeds them into an SGP object.
#' - Runs the `abcSGP` process for baseline percentile calculations.
#' - Saves the resulting lookup tables in specified file formats.
#' 
#' The function can handle single-cohort and super-cohort data structures, allowing for flexible analysis configurations.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'   createLookupTables(
#'      state = "DEMO",
#'      sgp.config = DEMO_Lookup_Table.config[[1]],
#'      parallel.config = list(BACKEND="PARALLEL", WORKERS=list(BASELINE_PERCENTILES=num_cores)),
#'      lookup_table_types = c('single-cohort', 'super-cohort')
#'   )
#' }
#' }
#' @seealso 
#'  \code{\link[sgpFlowMatrices]{sgpFlowMatrices}}
#'  \code{\link[methods]{new}}
#'  \code{\link[SGP]{abcSGP}}
#'  \code{\link[data.table]{setDTthreads}}, \code{\link[data.table]{fwrite}}
#' @rdname createLookupTables
#' @export 
#' @importFrom methods new
#' @importFrom SGP abcSGP
#' @importFrom data.table setDTthreads fwrite

createLookupTables <-
    function(
        state,
        sgp.config,
        parallel.config,
        matrices = NULL,
        lookup_table_types = c("single-cohort", "super-cohort")
    ) {
        ### Parameters
        matrix_year <- tail(sgp.config[[1]][["sgp.panel.years"]], 1)

        ### Get matrices to use
        if (is.null(matrices) & is.null(sgpFlowMatrices::sgpFlowMatrices[[paste(state, "sgpFlowMatrices", sep = "_")]])) {
            stop("No matrices for lookup table construction found.\nMatrices for lookup table calculation must either be supplied or available in the sgpFlowMatrices package.")
        }
        if (!is.null(matrices)) sgpFlow_matrices <- matrices else sgpFlow_matrices <- sgpFlowMatrices::sgpFlowMatrices[[paste(state, "sgpFlowMatrices", sep = "_")]]

        ### Loop over lookup_table_types
        for (lookup_table_type.iter in lookup_table_types) {
            ### Parameters
            lookup_table_type.label <- toupper(gsub("-", "_", lookup_table_type.iter))

            ### Add matrices from sgpFlow_Matrices to SGPstateData
            SGPstateData <- addsgpFlowMatrices(state, matrix_year, lookup_table_type.label)

            ### Get all 99 percentile cuts
            SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]] <- 1:99

            ### create Lookup tables and embed in an SGP object
            sgp_object <- methods::new("SGP", Data = createScaleScorePermutations(state = state, sgp.config = sgp.config))

            sgp_object <- SGP::abcSGP(
                sgp_object = sgp_object,
                state = state,
                steps = c("prepareSGP", "analyzeSGP", "combineSGP"),
                sgp.percentiles = FALSE,
                sgp.projections = FALSE,
                sgp.projections.lagged = FALSE,
                sgp.percentiles.baseline = TRUE,
                sgp.projections.baseline = FALSE,
                sgp.projections.lagged.baseline = FALSE,
                get.cohort.data.info = FALSE,
                sgp.config = sgp.config,
                simulate.sgps = FALSE,
                parallel.config = parallel.config,
                prepareSGP.create.achievement.level = FALSE
            )

            ### Extract tables and save
            data.table::setDTthreads(0)
            if (!dir.exists(file.path("Data", lookup_table_type.label))) dir.create(file.path("Data", lookup_table_type.label), recursive = TRUE)
            SGP_LOOKUP_TABLES <- extractLookupTables(sgp_object = sgp_object)
            save(SGP_LOOKUP_TABLES, file = file.path("Data", lookup_table_type.label, "SGP_LOOKUP_TABLES.Rdata"))
            data.table::fwrite(SGP_LOOKUP_TABLES[["sgpPercentileLookupTable"]], file = file.path("Data", lookup_table_type.label, "sgpPercentileLookupTable.csv.gzip"), compress = "gzip")
            data.table::fwrite(SGP_LOOKUP_TABLES[["sgpProjectionLookupTable"]], file = file.path("Data", lookup_table_type.label, "sgpProjectionLookupTable.csv.gzip"), compress = "gzip")
        }
    } ### END createLookupTables