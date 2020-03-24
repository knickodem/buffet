#' @title Shannon Entropy Index.
#'
#' @description 
#' Calculates the Shannon's entropy index as a measure of diversity within a sample.
#' 
#' @param x a contingency table or a vector to be passed to the function [base::table()].
#' @param base base of the logarithm to be used. Default is exp(1), natural. Base 2 and base 10 are also common.
#' @param ... if x is a vector, further arguments to be passed on to [base::table()].
#' 
#' @details 
#' The Shannon entropy index uses information theory to measure the uncertainty of a random variable.
#' As the index increases, there is greater uncertainty that a random observation is from a particular group.
#' Commonly used in ecological contexts, the utility of the index can extend to measure the heterogeneity, or diversity, of human populations.
#' The Shannon index uses the formula $$H = -\sum_{k=1}^{K} \pi_k log\left(\pi_k\right)$$ where $$\pi$$ is the proportion of group *k* within a population.
#' 
#' @return a numeric value.
#' 
#' @examples 
#' vec <- rep(LETTERS[1:7],times = c(50,30,20,10,10,5,5))
#' ShannonIndex(tab)
#' 
#' tab <- table(vec, useNA = "always)
#' ShannonIndex(vec, useNA = "always")
#'
ShannonIndex <- function(x, base = exp(1), ...){
  
  if(class(x) != "table"){
    
    x <- table(x, ...)
  }
  
  prop <- as.matrix(prop.table(x))
  H <- -sum(prop * log(prop, base = base))
  
  return(H)
  
}