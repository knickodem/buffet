#' @title Plots for checking model assumptions
#'
#' @description 
#' Produces plots via [ggplot2] to examine the fit of single or two-level linear or logistic regression models 
#' 
#' @ params model An `lm`, `glm`, or `merMod` object.
#' @ params glm.predict Type of predictions to use when model is a `glm` object. Can be "response" or "link", but only "response" is currently used for outputs.
#' @ params glm.resid Type of predictions to use when model is a `glm` or glmerMod object. Can be "deviance", "pearson", "working", "response", or "partial".
#' @ params smooth_method Method of smoothing, which is passed to [ggplot2::geom_smooth()]. Default is "loess".
#' @ params ... Additional arguments passed to [ggplot2::geom_smooth()].
#' 
#' @details 
#' Currently accepts linear models from [stats::lm()] or [lme4::lmer()] or logistic models from [stats::glm()] or [lme4::glmer()].
#' Linear models output a Q-Q plot of raw residuals and scatterplot of fitted and raw residual values to examine normality and
#' homoscedasticity assumptions, respectively. Logistic models output a scatterplot of residuals grouped by binary outcome to examine
#' homoscedasticity. Additionally, a ROC analysis and plot of the ROC curve, along with area under the curve estimate, demonstrates the
#' discrimination of the logistic model. All models produce a plot of Cook's distance for each observation.
#' Two-level models from [lme4] also output a scatterplot of level 1 and level 2 residuals to evaluate their correlation and, for linear models,
#' a Q-Q plot of the level 2 residuals (i.e., random intercept estimates).  
#' 
#' @return a list object.
#' 
#' @examples 
#' ## OLS
#' data(sleepstudy, package = "lme4")
#' lm.mod <- lm(Reaction ~ Days, data = sleepstudy)
#' lm.plots <- ModelCheckPlots(lm.mod, se = FALSE, color = "black")
#' 
#' ## Two-level logistic regression
#' data(grouseticks, package = "lme4")
#' grouseticks$TICKS01 <- ifelse(grouseticks$TICKS > 0, 1, 0)
#' 
#' glmer.mod <- lme4::glmer(TICKS01 ~ YEAR + cHEIGHT + (1|LOCATION), family = binomial(link = "logit"), data = grouseticks)
#' glmer.plots <- ModelCheckPlots(glmer.mod, glm.predict = "response", glm.resid = "deviance")
#' 
ModelCheckPlots <- function(model,
                            glm.predict = "response", glm.resid = "deviance",
                            smooth_method = "loess", ...){
  
  library(ggplot2)
  
  ## Collects output list
  MCPlots <- list(QQ = NULL,
                  L2_QQ = NULL,
                  Fitted_and_Residual_Values = NULL,
                  Residual_Correlation = NULL,
                  Cooks_Distance = NULL,
                  ROC_Curve = NULL)
  
  ## Extracting residuals
  if("lm" %in% class(model)){
    
    ## Dependent variable name
    DV <- names(model$model)[[1]]
    
    ## fit information
    aug <- broom::augment(model, type.predict = glm.predict, type.residuals = glm.resid) #glm. arguments ignored for lm objects
    
    
  } else if(grepl("merMod", class(model))){
    
    ## Dependent variable name
    DV <- names(model@frame)[[1]]
    
    ## Level 1 fit information
    aug <- broom.mixed::augment(model, type.predict = glm.predict, type.residuals = glm.resid) #glm. arguments ignored for lmer objects
    
    ## Level 2 information and plots
    clusternm <- names(model@cnms)[[1]]
    L2aug <- broom.mixed::augment(lme4::ranef(model))
    L1L2 <- dplyr::left_join(dplyr::rename(aug, level = all_of(clusternm)), L2aug, by = "level")
    
    ## L1 and L2 Residual Correlation
    L1L2corr <- ggplot(L1L2, aes(x = estimate, y = .resid)) +
      geom_hline(aes(yintercept = 0), color = "grey", size = 1, linetype = 1) +
      geom_point(shape = 1) +
      geom_smooth(method = smooth_method, ...) +
      labs(x = "Level 2 Residual", y = "Level 1 Residual") +
      theme_bw()
    
    # Storing plot in output object
    MCPlots[["Residual_Correlation"]] <- L1L2corr
    
  } else {stop("model must be of class 'lm', 'glm', or '(g)lmerMod'")}
  
  ## Adding id column
  aug <- tibble::rowid_to_column(aug, ".id")
  
  ## Cook's d
  cookplot <- ggplot(aug, aes(x = .id, y = .cooksd)) +
    geom_bar(stat = "identity", position = "identity") +
    labs(x = "Observation", y = "Cook's distance") +
    theme_bw()
  
  # Storing plot in output object
  MCPlots[["Cooks_Distance"]] <- cookplot
  
  

  if("glm" %in% class(model) | class(model) == "glmerMod"){
    
    ## Fitted vs. Residuals grouped by DV
    aug[[DV]] <- factor(aug[[DV]]) 
    
    binned <- ggplot(aug, aes(x = .id, y = .resid)) + 
      geom_hline(aes(yintercept = 0), color = "grey", size = 1, linetype = 1) +
      geom_point(aes_string(color = DV)) +
      geom_smooth(aes_string(color = DV), ...) +
      labs(x = "Observation", y = paste(tools::toTitleCase(glm.resid), "Residual")) +
      theme_bw()
    
    ## ROC Analysis
    rocit <- pROC::roc(response = aug[[DV]], predictor = aug[[".fitted"]]) # glm.predict muste be "response"
    
    # Extracting ROC Results
    AUC <- round(rocit$auc[[1]], 3)
    senspec <- data.frame(Sensitivity = rocit$sensitivities,
                          Specificity = rocit$specificities)
    
    rocplot <- ggplot(senspec, aes(x = Specificity, y = Sensitivity)) + 
      geom_abline(slope = 1, intercept = 1, colour = "grey", size = 1) +
      geom_line(size = 1.5, color = "black") +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
      scale_x_reverse(limits = c(1, 0), breaks = seq(1, 0, -.2)) +
      annotate(geom = "text", x = .3, y = .2, label = paste("AUC =", AUC)) +
      theme_bw()
    
    
    # Storing plots in output object
    MCPlots[["Fitted_and_Residual_Values"]] <- binned
    MCPlots[["ROC_Curve"]] <- rocplot
    
  } else if(class(model) == "lm" | class(model) == "lmerMod"){
    
    ## Q-Q Plot
    qq <- ggplot(aug, aes(sample = .resid)) +  ## standardizing doesn't change the shape of the distribution so it doesn't matter of .resid or .std.resid
      stat_qq(shape = 1) + stat_qq_line() +
      labs(x = "Theoretical Quantiles", y = "Residual") +
      theme_bw()
    
    ## Fitted vs. Residuals Plot
    frplot <- ggplot(aug, aes(x = .fitted, y = .resid)) +
      geom_hline(aes(yintercept = 0), color = "grey", size = 1, linetype = 1) +
      geom_point(shape = 1) +
      geom_smooth(method = smooth_method, ...) +
      labs(x = "Fitted", y = "Residual") +
      theme_bw()
    
    # Storing plots in output object
    MCPlots[["QQ"]] <- qq
    MCPlots[["Fitted_and_Residual_Values"]] <- frplot
    
  }
  
  if(class(model) == "lmerMod"){
    
    ## Q-Q Plot of L2 residuals
    L2qq <- ggplot(L2aug, aes(sample = estimate)) +  
      stat_qq(shape = 1) + stat_qq_line() +
      labs(x = "Theoretical Quantiles", y = "Level 2 Residual") +
      theme_bw()
    
    # Storing plot in output object
    MCPlots[["L2_QQ"]] <- L2qq
    
  }
  
  ## Removing NULL list elements 
  MCPlots <- MCPlots[lengths(MCPlots) != 0]
  
  return(MCPlots)
  
}


