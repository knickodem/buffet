#' Calculate scale score
#'
#' Combine item responses into a scale score using a sum, mean, or count
#'
#' @param data a \code{data.frame}
#' @param items character vector of column names in \code{data} to use in the scale
#' @param type should the score be the sum or the mean of item responses?
#' @param min.valid the minimum number of valid responses to receive a score.
#'
#' @return numeric vector
#'
#' @examples
#' set.seed(9876)
#' data <- data.frame(x1 = sample(0:4, 20, replace = TRUE),
#'                  x2 = sample(0:4, 20, replace = TRUE),
#'                  x3 = sample(0:4, 20, replace = TRUE),
#'                  x4 = sample(0:4, 20, replace = TRUE))
#'
#' sum.score <- scale_score(data, paste0("x", 1:4), type = "sum")
#'
#' ## Incorporate missing data
#' dfmiss <- apply(data, 2, function(x){
#'                 x[sample(c(1:nrow(df)), floor(nrow(df)/10))] <- NA
#'                 x})
#' dfmiss <- as.data.frame(dfmiss)
#'
#' # Score NA if missing on more than 1 item
#' mean.score <- scale_score(dfmiss, paste0("x", 1:4), type = "mean", min.valid = 3)
#'
#' @export
scale_score <- function(data, items, type = c("sum", "mean"), min.valid = NULL){


  if(type == "sum"){
    score <- rowSums(data[, items], na.rm = TRUE)
  } else if(type == "mean"){
    score <- rowMeans(data[, items], na.rm = TRUE)
  }

  nacount <- rowSums(is.na(data[, items]), na.rm = TRUE)

  if(!is.null(min.valid)){
    score <- ifelse((length(items) - nacount) < min.valid, NA, score)
  } else {
    score <- ifelse(nacount == length(items), NA, score)
  }

  return(score)
}

