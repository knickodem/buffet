#' @title Fit measures for a lavaan model.
#'
#' @description
#' Extracts number of observations and groups along with user-specified fit measures
#' from a \code{lavaan} object into a \code{data.frame}.
#'
#' @inheritParams extract_lavaan_params
#' @param measures If "all", all fit measures available are returned.
#' A single quoted fit measure or character vector of fit measures can also be specified.
#' There are also sets of pre-selected fit measures: "naive" for measures using the naive chi-square test statistic,
#' "scaled" for measures using the scaled chi-square test statistic (default), or
#' "robust for measures calculated with population-consistent formulas.
#'
#' @details
#' Similar to \code{\link[broom]{glance.lavaan}} but with more flexibility in which fit measures to output.
#'
#' @return a single row data.frame with columns for:
#' \itemize{
#'
#' \item{"ntotal"}{Number of observations included}
#' \item{"ngroups"}{Number of groups in model}
#' \item{"measures"}{The fit measures specified in the \code{measures} argument}
#' \item{"AIC, BIC"}{Akaike and Bayesian information criterion, if a maximum likelihood estimator was used}
#' }
#'
#' @examples
#' library(lavaan)
#' HS.model <- ' visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9 '
#'
#' fit <- cfa(HS.model, data = HolzingerSwineford1939)
#'
#' get_lavaan_fits(fit)
#' get_lavaan_fits(fit, measures = "robust")
#' get_lavaan_fits(fit, measures = c("cfi","rmsea", "srmr"))
#'
#' @export

get_lavaan_fits <- function(object, measures = "scaled"){

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

#### Gathers fit from multiple models into a single table and readies it for presentation ####
## wrapper around get_lavaan_fits
# mods.list - list of lavaan objects
# type      - "scaled" or "robust"; passed to measures argument in get_lavaan_fits
# digits    - number of digits to round numeric columns
fits_wrapper <- function(mods.list, type = "scaled", digits = 2){

  fit.tab <- purrr::map_dfr(mods.list, ~get_lavaan_fits(.x, measures = type), .id = "Model") %>%
    rename_with(.cols = ends_with(paste0(".",type)), .fn = ~gsub(paste0("\\.", type), "", .)) %>%
    mutate(across(.cols = c(chisq, pvalue:srmr), .fn = ~format(round(., digits), nsmall = digits))) %>%
    mutate(`90CI` = paste0("[", rmsea.ci.lower, ", ", rmsea.ci.upper, "]")) %>%
    select(Model, n = ntotal, ngroups, x2 = chisq, df, p = pvalue, CFI = cfi, RMSEA = rmsea, `90CI`, SRMR = srmr)

  return(fit.tab)
}
