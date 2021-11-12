#' Calculate scale score
#'
#' Combine item responses into a scale score using a sum, mean, or count
#'
#' @param df
#' @param items character vector of column names in \code{df} to use in the scale
#' @param type character indicating the type of score
#' @inheritParams base::sum
#' @param all.na.missing logical. When na.rm = TRUE, should rows with all NA
#' still be treated as missing? Default is \code{FALSE}.
#'
#' @return numeric vector
#'
#' @examples
#' set.seed(9876)
#' df <- data.frame(x1 = sample(0:4, 20, replace = TRUE),
#'                  x2 = sample(0:4, 20, replace = TRUE),
#'                  x3 = sample(0:4, 20, replace = TRUE),
#'                  x4 = sample(0:4, 20, replace = TRUE))
#'
#' sum.score <- scale_score(df, paste0("x", 1:4), type = "sum")
#'
#' ## Incorporate missing data
#' dfmiss <- apply(df, 2, function(x){
#'                 x[sample(c(1:nrow(df)), floor(nrow(df)/10))] <- NA
#'                 x})
#' dfmiss <- as.data.frame(dfmiss)
#'
#' # count number of items with a response
#' count.score <- scale_score(dfmiss, paste0("x", 1:4), type = "count", na.rm = TRUE)
#'
#' # Only score NA if NA for all items
#' mean.score <- scale_score(df, paste0("x", 1:4), type = "mean",
#'                           na.rm = TRUE, all.na.missing = TRUE)
#'
#' @export

scale_score <- function(df, items,
                        type = c("sum","count","mean"),
                        na.rm = FALSE, all.na.missing = FALSE){

  if(type %in% c("sum", "mean")){

    s <- rowSums(df[,items], na.rm = na.rm)
    score <- s

  } else if(type %in% c("count", "mean")){

    c <- rowSums(!is.na(df[,items]), na.rm = na.rm)
    score <- c

  }

  if(type == "mean"){

    score <- s / c

  }

  if(na.rm == TRUE & all.na.missing == TRUE){

    nacount <- rowSums(is.na(df[,items]), na.rm = TRUE)
    score <- ifelse(nacount == length(items), NA, score)

  }

  return(score)

}

