#### Indices for evaluating bifactor model from lavaan object ####
## from Rodriguez, Reise, & Havelin (2016) with corrections
Bifactor_Indices <- function(model, genName, groupName = NULL){
  
  ## Extracting matrices
  params<- lavInspect(model, "est")                 # list of model matrices
  phi <- lavInspect(model, "cor.lv")                # factor correlation matrix; in a bifactor model this will always be an identity matrix
  sigma <- lavInspect(model, "cor.ov")              # model implied item correlation matrix, equivalent to params$lambda %*% phi %*% t(params$lambda) + params$theta
  
  ## Interim calculations
  lambdas <- split(params$lambda, col(params$lambda, as.factor = TRUE))                 # standardized factor loadings for each latent variable
  common <- sum(purrr::map_dbl(lambdas,~sum(.x)^2))                                     # common variance from all factors
  resid <- sum(params$theta)                                                            # sum of residual variance for each item
  AllFD <- diag(phi %*% t(params$lambda) %*% solve(sigma) %*% params$lambda %*% phi)^.5 # Factor determinancy for all factors
  
  if(is.null(groupName)){
    
    Indices <- data.frame(
      omega = common / (common + resid),
      omegaH = sum(lambdas[[genName]])^2 / (common + resid),
      ECV = sum(lambdas[[genName]]^2) / sum(purrr::map_dbl(lambdas,~sum(.x^2))),
      FD = AllFD[[genName]],
      H = 1 / (1 + (1 / sum(lambdas[[genName]]^2 / (1 - lambdas[[genName]]^2))))
    )
    
  } else {
    
    group <- sum(lambdas[[genName]])^2 + sum(lambdas[[groupName]])^2
    
    Indices <- data.frame(
      omegaS = group / (group + resid),
      omegaHS = sum(lambdas[[groupName]])^2 / (group + resid),
      FD = AllFD[[groupName]],
      H = 1 / (1 + (1 / sum(lambdas[[groupName]]^2 / (1 - lambdas[[groupName]]^2))))
    )
    
  }
  
  return(Indices)
}
