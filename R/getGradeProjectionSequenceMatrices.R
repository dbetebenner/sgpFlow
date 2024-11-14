getGradeProjectionSequenceMatrices <-
    function(
        sgpFlow.config,
        projection.splineMatrices
    ) {
        tmp.list <- list()
        grade.progression.index <- seq_along(sgpFlow.config[["grade.progression"]])
        for (i in seq_along(grade.progression.index)) {
            tmp.list[[i]] <- list()
            for (j in seq_along(sgpFlow.config[["grade.projection.sequence"]])) {
                tmp.years_lags <- c(tail(sgpFlow.config[["year_lags.progression"]], grade.progression.index[i] - 1L), head(sgpFlow.config[["year_lags.projection.sequence"]], j))
                tmp.years <- rep("BASELINE", length(tmp.years_lags) + 1L)
                tmp.matrix <- getSplineMatrices(
                    projection.splineMatrices,
                    c(tail(sgpFlow.config[["content_area.progression"]], grade.progression.index[i]), head(sgpFlow.config[["content_area.projection.sequence"]], j)),
                    c(tail(sgpFlow.config[["grade.progression"]], grade.progression.index[i]), head(sgpFlow.config[["grade.projection.sequence"]], j)),
                    tmp.years,
                    tmp.years_lags,
                    return.highest.order.matrix = TRUE,
                    my.matrix.highest.order = sgpFlow.config[["max.order.for.progression"]],
                    my.matrix.time.dependency = NULL
                )

                tmp.list[[i]][[j]] <- tmp.matrix[[1L]]
            } ## END i loop
        } ## END j loop

        return(rev(tmp.list)) ### rev gives highest orders first
    } ### END getGradeProjectionSequenceMatrices
