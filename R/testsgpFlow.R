#' @title Test sgpFlow
#' @description Test of omnibus sgpFlow package functionalities. 
#' @param test.number Test number
#' @param save.results Save results to disk from tests (default=TRUE)
#' @param test.option Test option
#' @importFrom parallel detectCores
#' @importFrom utils recover
#' @export

testsgpFlow <- 
    function(
        test.number,
        save.results=TRUE,
        test.option
    ) {

        ###
        ### testsgpFlow(1): Omnibus test of sgpFlow package functionalities.
        ###
        if ("1" %in% toupper(test.number)) {

            options(error=recover)
            options(warn=2)
            if (.Platform$OS.type == "unix") number.cores <- detectCores() else number.cores <- NULL
            tmp.messages <- "##### Begin testsgpFlow test number 1 #####\n\n"

            ### Define configurations
            MATHEMATICS_2024_2025.config <- list(
                MATHEMATICS.2024 = list(
                    sgp.content.areas = c("MATHEMATICS", "MATHEMATICS", "MATHEMATICS"),
                    sgp.panel.years = c("2022_2023", "2023_2024", "2024_2025"),
                    sgp.grade.sequences = list(
                        c("3", "4"), c("3", "4", "5"),
                        c("4", "5", "6"), c("5", "6", "7"), c("6", "7", "8"),
                        c("7", "8", "9"), c("8", "9", "10")
                    )
                )
            )

            READING_2024_2025.config <- list(
                READING.2024 = list(
                    sgp.content.areas = c("READING", "READING", "READING"),
                    sgp.panel.years = c("2022_2023", "2023_2024", "2024_2025"),
                    sgp.grade.sequences = list(
                        c("3", "4"), c("3", "4", "5"),
                        c("4", "5", "6"), c("5", "6", "7"), c("6", "7", "8"),
                        c("7", "8", "9"), c("8", "9", "10")
                    )
                )
            )

            DEMO_sgpFlow.config <- c(MATHEMATICS_2024_2025.config, READING_2024_2025.config)

            ### Use super-cohort matrices
            projection.splineMatrices <- sgpFlowMatrices::sgpFlowMatrices[['DEMO_sgpFlowMatrices']][['2024_2025']][['SUPER_COHORT']]

            expression.to.evaluate <- 
            		paste0("Demonstration_sgpFlow <- sgpFlow(\n\tsgp_object=sgpFlowData::sgpFlowData_LONG,\n\tsgpFlow.config=DEMO_sgpFlow.config,\n\tprojection.splineMatrices=projection.splineMatrices,\n\tparallel.config=NULL)\n")
            
		    if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "dir.create('Data', showWarnings=FALSE)", "save(Demonstration_sgpFlow, file='Data/Demonstration_sgpFlow.Rdata')", sep="\n")

            cat(paste0("EVALUATING sgpFlow Test Number 1:\n", expression.to.evaluate), fill=TRUE)

            started.at.overall <- proc.time()
			eval(parse(text=expression.to.evaluate))

            ### TBD: Add tests of Demonstration_sgpFlow object

            tmp.messages <- c(tmp.messages, paste0("\n##### End testsgpFlow test number ", test.number, ":  ", convertTime(timetakensgpFlow(started.at.overall)), " #####\n"))
			messagesgpFlow(tmp.messages)

        } ### END testsgpFlow(1)





} ### END testsgpFlow