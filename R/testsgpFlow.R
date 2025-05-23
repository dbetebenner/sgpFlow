#' @title Test sgpFlow
#' @description Test of omnibus sgpFlow package functionalities. 
#' @param test.number Test number
#' @param save.results Save results
#' @param test.option Test option
#' @importFrom parallel detectCores
#' @importFrom utils recover
#' @export

testsgpFlow <- 
    function(
        test.number,
        save.results,
        test.option
    ) {

        ###
        ### testsgpFlow(1): Omnibus test of sgpFlow package functionalities.
        ###
        if ("1" %in% toupper(test.number)) {

            options(error=recover)
            options(warn=2)
            if (.Platform$OS.type == "unix") number.cores <- detectCores() else number.cores <- NULL
            tmp.messages <- "##### Begin testSGP test number 1 #####\n\n"

            if (is.null(test.option[['parallel.config']])) {
                if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE='doParallel', "
                if (.Platform$OS.type != "unix") {
                    parallel.config <- paste0("list(BACKEND='FOREACH', TYPE='doParallel', WORKERS=list(TAUS=", number.cores, "))")
                } else  parallel.config <- paste0("list(BACKEND='PARALLEL', WORKERS=list(TAUS=", number.cores, "))")
            } else parallel.config <- test.option[['parallel.config']]

            expression.to.evaluate <- 
            		paste0("Demonstration_sgpFlow <- sgpFlow(\n\tsgp_object=sgpFlowData::sgpFlowData_LONG,\n\tparallel.config=", parallel.config, "\n)\n")

		    if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "dir.create('Data', showWarnings=FALSE)", "save(Demonstration_sgpFlow, file='Data/Demonstration_sgpFlow.Rdata')", sep="\n")

        } ### END testsgpFlow(1)





} ### END testsgpFlow function