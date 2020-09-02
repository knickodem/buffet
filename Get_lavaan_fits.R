#' @title Fit measures for a `lavaan` model.
#'
#' @description 
#' Extracts nubmer of observations and groups along with user-specified fit measures from a `lavaan` object into a data.frame.
#' 
#' @param object A `lavaan` object from [lavaan::cfa()] or [lavaan::sem()].
#' @param measures If "all", all fit measures available are returned.
#' A single quoted fit measure or character vector of fit measures can also be specified.
#' There are also sets of pre-selected fit measures: "naive" for measures using the naive chi-square test statistic,
#' "scaled" for measures using the scaled chi-square test statistic (default), or
#' "robust for measures calculated with population-consistent formulas.
#' @details
#' Similar to [broom::glance.lavaan()] but with more flexibility in which fit measures to output.
#' 
#' @return a single row data.frame with columns for:
#' 
#' \item{ntotal}{Number of observations included}
#' \item{ngroups}{Number of groups in model}
#' \item{measures}{The fit measures specified in the `measures`` argument}
#' \item{AIC, BIC}{Akaike and Bayesian information criterion, if a maximum likelihood estimator was used}
#' 
#' @examples 
#' library(lavaan)
#' HS.model <- ' visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9 '
#'               
#' fit <- cfa(HS.model, data = HolzingerSwineford1939)
#' 
#' Get_lavaan_fits(fit),
#' Get_lavaan_fits(measures = "robust")
#' Get_lavaan_fits(fit, measures = c("cfi","rmsea", "srmr))
Get_lavaan_fits <- function(object, measures = "scaled"){
  
  if(length(measures) > 1){
    
    indices <- measures
    
  } else if(measures == "scaled"){
    
    indices <- c('npar', 'chisq.scaled', 'df.scaled', 'pvalue.scaled',
                 'cfi.scaled', 'tli.scaled', 'agfi',
                 'rmsea.scaled', 'rmsea.ci.lower.scaled', 'rmsea.ci.upper.scaled','srmr')
    
  } else if(measures == "robust"){
    
    indices <- c('npar', 'chisq.scaled', 'df.scaled', 'pvalue.scaled',
                 'cfi.robust', 'tli.robust', 'agfi',
                 'rmsea.robust', 'rmsea.ci.lower.robust', 'rmsea.ci.upper.robust', 'srmr')
    
  } else if(measures == "naive"){
    
    indices <- c('npar', 'chisq', 'df', 'pvalue',
                 'cfi', 'tli', 'agfi',
                 'rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper', 'srmr')
  }
  


    fits <- as.data.frame(t(c(lavaan::fitMeasures(object, fit.measures = indices))))
    
    fits <- fits %>% mutate(ntotal = lavaan::inspect(object, "ntotal"),
                            ngroups = lavaan::inspect(object, "ngroups")) %>%
      select(ntotal, ngroups, everything())
    
    if(!is.na(object@loglik$loglik)){
      
      fits <- fits %>%
        mutate(AIC = round(AIC(object), 1),
               BIC = round(BIC(object), 1))
      
    }
    
  return(fits)
}

