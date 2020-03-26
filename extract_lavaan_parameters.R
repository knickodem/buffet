#' @title Extract lavaan parameters.
#'
#' @description 
#' Extracts standardized or unstandardized parameter estimates from a lavaan object.
#' 
#' @param object A `lavaan` object from [lavaan::cfa()] or [lavaan::sem()].
#' @param std If "no", unstandardized estimates are returned. Otherwise specify the type of standardization ("std.all", "std.lv", or "std.nox") which is passed to [lavaan::standardizedSolution()]. 
#' @param parameters Quoted parameter or character vector of quoted parameters to be included in the output. Default is "all" parameters.
#' @param ... Additional arguments passed to [lavaan::parameterEstimates()] or [lavaan::standardizedSolution()].
#' 
#' @details
#' Similar to [broom::tidy.lavaan()] but includes option for filtering which parameters are included in the output.
#' Additionally, uses [lavaan::standardizedSolution()] instead of [lavaan::parameterEstimates()]. Thus, the SEs and tests are based on the standardized estimates rather than the unstandardized estimates.
#' 
#' @return a data.frame containing parameter estimates, confidence interval, standard error, test statistic, and p-value.
#' 
#' @examples 
#' library(lavaan)
#' HS.model <- ' visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9 '
#' fit <- cfa(HS.model, data = HolzingerSwineford1939)
#' 
#' # all unstandardized parameters
#' extract_lavaan_parameters(fit)
#' 
#' # only standardized factor loadings and correlations
#' extract_lavaan_parameters(fit, std = "std.all", parameters = c("=~", "~~"))
extract_lavaan_parameters <- function(object, std = "no", parameters = "all", ...){
  
  name <- quo_name(enquo(name))
  
  if(parameters == "all"){
    
    params = c("=~", "~~", "~*~", "~1", "~", "|", "<~")
    
  } else {
    
    params <- parameters
    
  }
  
  if(std == "no"){
    
    TheParams <- parameterEstimates(object, standardized = FALSE, ...) %>%
      filter(op %in% params)
    
  } else if(std %in% c("std.all", "std.lv", "std.nox")){
    
    TheParams <- standardizedSolution(object, type = std) %>%
      filter(op %in% params)
    
  } else {
    stop("std argument must be 'no' for unstandardized estimates or one of 'std.all', std.lv', or 'std.nox' for standardized estimates")
  }
  
  
  return(TheParams)
}
