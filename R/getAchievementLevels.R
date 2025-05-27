#' @title Categorize Scale Scores into Achievement Levels
#'
#' @description 
#' This function takes a vector of scale scores and categorizes them into achievement levels
#' based on provided cut scores and corresponding labels.
#'
#' @param scale_score A numeric vector of scale scores to be categorized
#' @param cuts A numeric vector of cut scores that define the boundaries between achievement levels
#' @param labels A character vector of achievement level labels. Must be one element longer than cuts
#'
#' @return A character vector with the same length as scale_score, containing achievement level labels
#'
#' @details
#' The function uses R's \code{.bincode()} function to categorize scale scores into achievement levels.
#' The number of labels must be exactly one more than the number of cut scores, as each interval
#' between cuts (including -Inf to first cut and last cut to Inf) requires a label.
#'
#' @examples
#' # Example with 3 achievement levels
#' scale_scores <- c(100, 200, 300, 400, 500)
#' cut_scores <- c(200, 400)
#' level_labels <- c("Below Basic", "Basic", "Proficient")
#' getAchievementLevels(scale_scores, cut_scores, level_labels)
#'
#' # Example with 4 achievement levels
#' cut_scores <- c(200, 300, 400)
#' level_labels <- c("Below Basic", "Basic", "Proficient", "Advanced")
#' getAchievementLevels(scale_scores, cut_scores, level_labels)
#' @rdname getAchievementLevels
#' @export

getAchievementLevels <-
    function(scale_score, cuts, labels) {
      if (length(labels) != length(cuts) + 1) {
        stop("Number of labels must be one more than the number of cuts")
      }

      # Use .bincode for fast categorization
      result <- labels[.bincode(scale_score, breaks = c(-Inf, cuts, Inf), right = FALSE, include.lowest = TRUE)]
      result[result == 0 | is.na(result)] <- NA_character_

      return(result)
} ### END getAchievementLevels
