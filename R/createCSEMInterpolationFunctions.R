#' @importFrom stats approxfun
createCSEMInterpolationFunctions <-
    function(
        scale_score_csem_data
    ) {
        ### Test for appropriate variables in supplied data
        if (!all(c("CONTENT_AREA", "GRADE", "SCALE_SCORE", "SCALE_SCORE_CSEM") %in% names(scale_score_csem_data))) {
            stop("Please provide a LONG formatted data set with 'CONTENT_AREA', 'GRADE', 'SCALE_SCORE', and 'SCALE_SCORE_CSEM' variables.")
        }

        ### Create list for functions
        csemInterpolationFunctions <- list()

        ### Create unique combinations of SCALE_SCORE and SCALE_SCORE_CSEM by CONTENT_AREA and GRADE
        tmp.unique.data <- unique(scale_score_csem_data, by = c("CONTENT_AREA", "GRADE", "SCALE_SCORE", "SCALE_SCORE_CSEM"), na.rm = TRUE)
        tmp.unique.data[, SCALE_SCORE_CSEM := mean(SCALE_SCORE_CSEM), keyby = c("CONTENT_AREA", "GRADE", "SCALE_SCORE")]

        ### Loop over unique values of CONTENT_AREA and GRADE to create CSEM interpolation functions
        for (content_area.iter in unique(tmp.unique.data[["CONTENT_AREA"]])) {
            for (grade.iter in unique(tmp.unique.data[CONTENT_AREA == content_area.iter][["GRADE"]])) {
                tmp.approx.fun.data <- tmp.unique.data[.(content_area.iter, grade.iter)]
                csemInterpolationFunctions[[content_area.iter]][[paste("GRADE", grade.iter, sep = "_")]] <- stats::approxfun(tmp.approx.fun.data[["SCALE_SCORE"]], tmp.approx.fun.data[["SCALE_SCORE_CSEM"]])
            }
        }

        save(csemInterpolationFunctions, file = "csemInterpolationFunctions.Rdata")
    } ### END createCSEMInterpolationFunctions
