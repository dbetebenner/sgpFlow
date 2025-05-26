#' @title Generate Grade Projection Sequence Matrices
#'
#' @description 
#' Creates a sequence of matrices for grade projections based on configuration settings.
#' This is an internal function used to generate the necessary matrices for SGP projections.
#'
#' @param sgpFlow.config A list containing configuration settings including:
#'   \itemize{
#'     \item grade.progression: Vector of grade levels
#'     \item grade.projection.sequence: Vector of projection sequence grades
#'     \item year_lags.progression: Vector of year lags for progression
#'     \item year_lags.projection.sequence: Vector of year lags for projection sequence
#'     \item content_area.progression: Vector of content areas for progression
#'     \item content_area.projection.sequence: Vector of content areas for projection sequence
#'     \item max.order.for.progression: Maximum order for progression
#'   }
#' @param projection.splineMatrices A list containing spline matrices used for projections
#'
#' @return A list of matrices, with highest orders first, where each element represents
#'   a combination of grade progression and projection sequence.
#'
#' @rdname getGradeProjectionSequenceMatrices
#' @keywords internal

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
