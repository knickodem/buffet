## calculate mean, sd for each group and run t-test; combine into single table
the_t <- function(data, outcome, by, name = outcome, covariates = NULL){

  out <- sym(outcome)
  b <- sym(by)

  if(is.null(covariates)){ # conducts t-test

    test <- t.test(data[[outcome]] ~ data[[by]]) %>%
      broom::tidy() %>%
      mutate(Variable = name,
             d = effectsize::t_to_d(statistic, parameter)[[1]]) %>%
      select(Variable, t = statistic, df = parameter, p = p.value, d)

  } else { # conducts linear regression

    mod <- lm(formula(paste(outcome, "~", by, "+", covariates)), data = data2)
    test <- parameters::model_parameters(mod) %>%
      filter(Parameter == dsb) %>%
      mutate(Variable = name,
             d = effectsize::t_to_d(t, df_error)[[1]]) %>%
      select(Variable, t, df = df_error, p, d)
  }

  together <- data %>%
    filter(!is.na(!!b)) %>%
    group_by(!!b) %>%
    summarize(an = sum(!is.na(!!out)),
              M = mean(!!out, na.rm = TRUE),
              SD = sd(!!out, na.rm = TRUE)) %>%
    tidyr::gather(x, y, an:SD) %>%
    unite(col = "temp", !!b, x, sep = '_') %>%
    spread(temp, y) %>%
    bind_cols(test) %>%
    select(Variable, everything()) %>%
    rename_with(.cols = ends_with("_an"), ~str_replace(.x, "_an", "_n"))

  return(together)

}
