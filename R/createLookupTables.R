`createLookupTables` <-
function(
    state,
    sgp.config,
    parallel.config,
    matrices=NULL,
    lookup_table_types=c('single-cohort', 'super-cohort')
) {
    ### Parameters
    matrix_year <- tail(sgp.config[[1]][['sgp.panel.years']], 1)

	### Get matrices to use
    if (is.null(matrices) & is.null(sgpFlowMatrices::sgpFlowMatrices[[paste(state, "sgpFlowMatrices", sep="_")]])) {
        stop("No matrices for lookup table construction found.\nMatrices for lookup table calculation must either be supplied or available in the sgpFlowMatrices package.")
    }
    if (!is.null(matrices)) sgpFlow_matrices <- matrices else sgpFlow_matrices <- sgpFlowMatrices::sgpFlowMatrices[[paste(state, "sgpFlowMatrices", sep="_")]] 

    ### Loop over lookup_table_types 
    for (lookup_table_type.iter in lookup_table_types) {
        
        ### Parameters
        lookup_table_type.label <- toupper(gsub("-", "_", lookup_table_type.iter))

        ### Add matrices from sgpFlow_Matrices to SGPstateData
        SGPstateData <- addSGPFlowMatrices(state, matrix_year, lookup_table_type.label)

        ### Get all 99 percentile cuts 
        SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]] <- 1:99

        ### create Lookup tables and embed in an SGP object
        sgp_object <- new("SGP", Data=createScaleScorePermutations(state=state, sgp.config=sgp.config))

        sgp_object <- SGP::abcSGP(
                sgp_object=sgp_object,
                state=state,
                steps=c("prepareSGP", "analyzeSGP", "combineSGP"),
                sgp.percentiles=FALSE,
                sgp.projections=FALSE,
                sgp.projections.lagged=FALSE,
                sgp.percentiles.baseline=TRUE,
                sgp.projections.baseline=FALSE,
                sgp.projections.lagged.baseline=FALSE,
                get.cohort.data.info=FALSE,
                sgp.config=sgp.config,
                simulate.sgps=FALSE,
                parallel.config=parallel.config,
                prepareSGP.create.achievement.level=FALSE)

        ### Extract tables and save
        if (!dir.exists(file.path("Data", lookup_table_type.label))) dir.create(file.path("Data", lookup_table_type.label), recursive=TRUE)
        SGP_LOOKUP_TABLES <- extractLookupTables(sgp_object=sgp_object)
        save(SGP_LOOKUP_TABLES, file=file.path("Data", lookup_table_type.label, "SGP_LOOKUP_TABLES.Rdata"))
        fwrite(SGP_LOOKUP_TABLES[['sgpPercentileLookupTable']], file=file.path("Data", lookup_table_type.label, "sgpPercentileLookupTable.csv.gzip"), compress="gzip")
        fwrite(SGP_LOOKUP_TABLES[['sgpProjectionLookupTable']], file=file.path("Data", lookup_table_type.label, "sgpProjectionLookupTable.csv.gzip"), compress="gzip")
    }
} ### END createLookupTables
