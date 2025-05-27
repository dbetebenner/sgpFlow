#' @title Get Achievement Percentiles 
#' 
#' @description 
#' This function processes wide-format data to calculate various percentile scores
#' (e.g., univariate and multivariate percentiles) based on scale score variables.
#'
#' @param wide_data A data.table containing wide-format data with scale scores.
#' @param scale_score.names A character vector of column names representing scale score variables.
#' @param achievement.percentiles.tables Logical. If TRUE, creates percentile tables instead of augmenting `wide_data` with new variables.
#'
#' @return A data.table containing the original data along with additional columns for scale score percentiles:
#' \itemize{
#'   \item \code{ACHIEVEMENT_PERCENTILE_INITIAL_1}: Percentile rank of the most recent scale score.
#'   \item \code{ACHIEVEMENT_PERCENTILE_INITIAL}: Multivariate percentile rank across all scale score variables (if applicable).
#' }
#' If `achievement.percentiles.tables` is TRUE, returns percentile tables.
#'
#' @importFrom collapse missing_cases
#' @importFrom copula normalCopula fitCopula pCopula
#' @importFrom data.table as.data.table
#' @importFrom Rfast Round
#' @importFrom stats uniroot
#' @rdname getAchievementPercentiles
#' @keywords internal

getAchievementPercentiles <-
function(
        wide_data,
        scale_score.names,
        achievement.percentiles.tables
    ) {
        # Set varialbe to NULL to avoid R CMD check notes
        ACHIEVEMENT_PERCENTILE_INITIAL <- ACHIEVEMENT_PERCENTILE_INITIAL_1 <- NULL

        # Utility functions
        get_percentile <- function(scale_score) {
            percentiles <- collapse::fquantile(scale_score, probs=seq(0.005, 0.995, length=100), na.rm=TRUE)
            return(bound.scores(findInterval(scale_score, percentiles), c(1, 99)))
        }

        calculate_copula_quantiles <- function(complete_scores, what.to.return, probs=seq(0.01, 0.99, 0.01)) {
            # Step 1: Transform variables to uniform marginals
            scores_uniform <- apply(complete_scores, 2, function(x) rank(x, ties.method = "average") / length(x))

            # Step 2: Fit a copula to the uniform data
            # Use a Gaussian copula as an example
            copula_model <- normalCopula(dim = ncol(scores_uniform))
            fit <- fitCopula(copula_model, scores_uniform, method = "ml") # Maximum likelihood fit

            # Step 3: Using copula return PERCENTILE_RANKS or PERCENTILE_CUTS
            if (what.to.return=="PERCENTILE_RANKS") {
                return(as.integer(bound.scores(Rfast::Round(100*pCopula(scores_uniform, copula = fit@copula)), c(1, 99))))
            }
            if (what.to.return=="PERCENTILE_CUTS") {
                u1_values <- vapply(1:99/100, function(prob) {
                    uniroot(
                        function(u1) pCopula(c(u1, u1), copula = fit@copula) - prob, interval = c(0, 1))$root
                }, numeric(1))

                percentile_cuts_table <- as.data.table(apply(complete_scores, 2, function(x) collapse::fquantile(x, probs=u1_values)))[,ACHIEVEMENT_PERCENTILE_INITIAL_1:=as.integer(bound.scores(Rfast::Round(100*u1_values), c(1, 99)))][,ACHIEVEMENT_PERCENTILE_INITIAL := 1:99]

                return(percentile_cuts_table)
            }
        } ## calculate_copula_quantiles

        # Prepare data
        complete_scores <- collapse::na_omit(wide_data[, ..scale_score.names])
        complete_cases <- !collapse::missing_cases(wide_data[, ..scale_score.names])

        if (achievement.percentiles.tables) {
            if (length(scale_score.names) > 1) {
                wide_data <- data.table(ID=sprintf("%02d", 1:99), calculate_copula_quantiles(complete_scores, "PERCENTILE_CUTS"))
            } else {
                wide_data <- setnames(data.table(ID=sprintf("%02d", 1:99), TEMP=collapse::fquantile(wide_data[[scale_score.names]], probs=1:99/100))[, ACHIEVEMENT_PERCENTILE_INITIAL_1 := 1:99], "TEMP", scale_score.names)
            }
        } else {
            wide_data[, ACHIEVEMENT_PERCENTILE_INITIAL_1 := get_percentile(get(tail(scale_score.names, 1)))]
            if (length(scale_score.names) > 1) {
                wide_data[complete_cases, ACHIEVEMENT_PERCENTILE_INITIAL := calculate_copula_quantiles(complete_scores, "PERCENTILE_RANKS")]
                wide_data[is.na(ACHIEVEMENT_PERCENTILE_INITIAL), ACHIEVEMENT_PERCENTILE_INITIAL := ACHIEVEMENT_PERCENTILE_INITIAL_1]
            }
        }
        return(wide_data)
    } ### END getAchievementPercentiles