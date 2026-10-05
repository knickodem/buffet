#' Summary of mirt object
#'
#' Gathers output from [mirt::`summary-method`] into formats that can
#' be directly reported or used for further analyses.
#'
#' @inheritParams mirt::`SingleGroupClass-class`
#' @param object an object of class [mirt::`SingleGroupClass-class`]
#' @param item_names optional character vector of item names to use instead of
#' original column names
#' @param cut numeric indicating the absolute value of the standardized factor
#' loadings below which will be converted to NA and hidden in reporting.
#' This functionality is distinct from the `suppress` argument in [mirt::`SingleGroupClass-class`].
#'
#' @return
#' a `list`

mirt_summary <- function(object, item_names = NULL,
                         SE = FALSE, rotate = "oblimin",
                         cut = .2){
  # prep possible output
  out <- list(
    loadings = NULL,
    se = NULL,
    fcor = NULL
  )
  # run summary
  os <- summary(object, SE, rotate, verbose = FALSE)

  if(is.null(item_names)){
    item_names <- attr(object@Data$data, "dimnames")[[2]]
  }

  # format factor loadings and variance explained (h2)
  fs <- cbind(data.frame(item = item_names), os$rotF, os$h2)
  fs <- fs |>
    mutate(across(starts_with("F"), ~ifelse(abs(.x) < cut, NA, .x)))
  rownames(fs) <- NULL
  out$loadings <- fs

  # format standard errors if requested and available
  # mirt only computes for unidimensional models
  if(SE == TRUE & "SE.F" %in% names(os)){
    se <- cbind(data.frame(item = item_names), os$SE.F)
    rownames(se) <- NULL
    out$se <- se
  }

  # keep matrix of factor correlations when > 1 factor
  if(dim(os$fcor)[[1]] > 1){
    out$fcor <- os$fcor
  }

  # remove null elements
  out <- out[lengths(out) > 0]

  return(out)
}

#' Get item parameters from mirt object
#'
#' Gathers output from [mirt::`coef-method`] into formats that can
#' be directly reported or used for further analyses.
#'
#' @inheritParams mirt::`coef-method`
#' @param object an object of class [mirt::`SingleGroupClass-class`]
#' @param simplify logical; if `TRUE` (default) then `printSE` is ignored and neither
#' the SE nor CI are included. Set to `FALSE` to include the SE or CI based on `printSE`.
#'
#' @return
#' a `list` with item parameters in longer format, including rows for factor
#' means and variances and (if selected) column(s) for standard errors or
#' confidence intervals, and in wider format.

get_item_params <- function(object, IRTpars = TRUE,
                            simplify = TRUE,
                            printSE = FALSE){

  ## parameters - long format
  pars_long <- coef(object, IRTpars = IRTpars,
                    printSE = printSE,
                    simplify = simplify,
                    as.data.frame = TRUE, # puts in long format matrix
                    verbose = FALSE) |>
    as.data.frame() |> # actually converts to a dataframe
    tibble::rownames_to_column("temp") |>
    tidyr::separate(temp, into = c("item", "parameter"), sep = "\\.")

  items <- attr(object@Data$data, "dimnames")[[2]]

  ## parameters - wide format
  temp_wide <- coef(object, IRTpars = IRTpars,
                    printSE = printSE, # ignored
                    simplify = TRUE,
                    as.data.frame = FALSE,
                    verbose = FALSE)

  # converting to dataframe
  pars_wide <- temp_wide$items |>
    as.data.frame() |>
    tibble::rownames_to_column("item") |>
    mutate(item = factor(item, levels = items))

  # Saving parameters for test assembly (wide format)
  # pars_wide <- pars_long |>
  #   filter(item != "GroupPars") |>
  #   select(-SE) |>
  #   tidyr::spread(parameter, par)  |>
  #   mutate(item = factor(item, levels = items))  |>
  #   arrange(item)


  # If we want to format for automated test assembly
  # ata.long <- mirt.wide %>%
  #   tidyr::gather(key = "threshold", value = "b", starts_with("b")) %>%
  #   mutate(threshold = gsub("b", "", threshold),
  #          grm.id = as.character(item)) %>%
  #   arrange(item) %>%
  #  tidyr::unite(col = item, item, threshold, sep = ".")

  return(list(pars_long = pars_long,
              pars_wide = pars_wide
              # ata.long = ata.long
  ))
}

#' Change plot title
#'
#' Shortcut for changing title of lattice plots
#'
#' @param plot a lattice plot
#' @param title character string for main title to print on plot
#'
retitle_plot <- function(plot, title){
  plot$main <- title
  return(plot)
}

#' Gather results for reporting
#'
#' Gathers results from a mirt object and formats for reporting.
#'
#' @param mod1 an object of class [mirt::`SingleGroupClass-class`]
#' @param item_names optional character vector of item names to use instead of
#' original column names
#' @param mod2 a second mirt model object to compare to `mod1`.
#' Unless specified, all other gathered results are from `mod1`.
#' @param mod_names two-element vector providing names for `mod1` and `mod2`. Ignored
#' if `mod2` is `NULL`.
#' @inheritParams mirt_summary
#' @inheritParams get_item_params
#' @param fit_type passed to the `type` argument in [`mirt::M2`].
#' @param fit_stats passed to the `fit_stats` argument in [`mirt::itemfit`].
#' @param wrightmap logical; print a Wright map via [`WrightMap::wrightMap`]. The plot
#' is displayed in the viewer but not saved in the list. However, the thresholds are returned
#' and can be used with the retunred thetas to call [`WrightMap::wrightMap`] again when
#' needed.
#' @inheritParams mirt::fscores
#'
#' @return
#' a `list`

mirt_results <- function(mod1, item_names = NULL,
                         mod2 = NULL, mod_names = c("1-factor", "2-factor"),
                         SE = FALSE, rotate = "oblimin", cut = .2,
                         IRTpars = TRUE, simplify = TRUE, printSE = FALSE,
                         fit_type = "M2*", fit_stats = c("S_X2", "infit"),
                         wrightmap = FALSE, method = "EAP"){

  items <- attr(mod1@Data$data, "dimnames")[[2]]
  if(is.null(item_names)){
    item_names <- items
  }

  ## Model Comparison
  if(!is.null(mod2)){

    comp <- list(mod1 = mirt_summary(mod1, item_names = item_names,
                                       SE = SE, rotate = rotate, cut = cut),
                 mod2 = mirt_summary(mod2, item_names = item_names,
                                       SE = SE, rotate = rotate, cut = cut),
                 comp = cbind(data.frame(Model = mod_names), anova(mod1, mod2)))

    ## global fit statistics
    global_fit <- purrr::map_dfr(.x = list(mod1, mod2),
                                 ~M2(.x, type = fit_type),
                                 .id = "Model") |>
      mutate(Model = c(mod_names))
    row.names(global_fit) <- NULL

  } else {
    comp <- mirt_summary(mod1, item_names = item_names,
                           SE = SE, rotate = rotate, cut = cut)

    ## global fit statistics
    global_fit <- M2(mod1, type = fit_type)
  }

  ## item fit statistics
  item_fit <- itemfit(mod1, fit_stats = fit_stats,  na.rm = FALSE)

  ## scale and item info
  mrxx <- marginal_rxx(mod1)
  scaleplots <- lapply(c("infoSE", "rxx", "score", "itemscore","infotrace", "trace"),
                       function(x) plot(mod1, type = x, as.table = TRUE))
  names(scaleplots) <- c("scale_info_se", "scale_rxx", "scale_score",
                         "item_score","item_info", "item_probs")

  ## empirical plots
  empplot <- lapply(1:length(items),
                    function(x) itemfit(mod1, empirical.plot = x, as.table = TRUE))
  empplot <- purrr::map2(.x = empplot, .y = item_names,
                         ~retitle_plot(.x, paste("Empirical plot for", .y)))

  ## item parameters
  params <- get_item_params(mod1, IRTpars = IRTpars,
                            simplify = simplify, printSE = printSE)

  ## person parameters
  thetas <- fscores(mod1, method = method)

  #### Gathering Results ####
  results <- list(Items = items,
                  `Model Summary` = comp,
                  `Global Fit` = global_fit,
                  `Item Fit` = item_fit,
                  `Scale Reliability` = mrxx,
                  `Scale Plots` = scaleplots,
                  `Empirical Plots` = empplot,
                  `Item Parameters` = params,
                  Thetas = thetas,
                  `Wright Map` = NULL)

  if(wrightmap == TRUE){
    thresholds <- params$pars_wide |>
      select(matches("^b|^d"))
    results$`Wright Map` <- WrightMap::wrightMap(thetas = thetas,
                                                 thresholds = thresholds,
                                                 main.title = NULL)

  } else {
    results <- results[lengths(results) > 0]
  }

  return(results)

}
