
format_flex <- function(df, bold = FALSE, digits = 2, width = NULL){

  if(is.data.frame(df)){
    numericcols <- which(unlist(lapply(df, is.numeric)))
    ftab <- flextable(df)
    ftab <- colformat_double(ftab, j = numericcols, digits = digits)
  } else{
    ftab <- df
  }

  ftab <- flextable::font(ftab, fontname = "Times New Roman", part = "all")
  ftab <- flextable::padding(ftab, padding = 0, part = "all")
  ftab <- flextable::align(ftab, align = "center", part = "all")
  ftab <- flextable::align(ftab, j = 1, align = "left")

  if(bold == TRUE){
    ftab <- bold(ftab, i = ~ is.na(pvalue) == FALSE & pvalue < .05, part =  "body")
    # ftab <- bold(ftab, i = ~ est.std > .20, part = "body")
  }

  ftab <- autofit(ftab)
  if(!is.null(width)){
    ftab <- fit_to_width(ftab, width)
  }

  return(ftab)
}


two_level_flex <- function(flex, mapping, vert.cols, border = NULL){

  if(is.null(border)){
    border <- officer::fp_border(width = 2) # manual horizontal flextable border width
  }

  flex <- flextable::set_header_df(flex, mapping = mapping)
  flex <- flextable::merge_h(flex, part = "header")
  flex <- flextable::merge_v(flex, j = vert.cols, part = "header")
  flex <- flextable::fix_border_issues(flex)
  flex <- flextable::border_inner_h(flex, border = border, part = "header")
  flex <- flextable::hline_top(flex, border = border, part = "all")
  # flex <- flextable::theme_vanilla(flex)
  flex <- flextable::align(flex, align = "center", part = "all")
  flex <- flextable::font(flex, fontname = "Times New Roman", part = "all")
  flex <- flextable::padding(flex, padding = 0, part = "all")
  flex <- flextable::autofit(flex)
}


# example
vd.map <- data.frame(col_keys = names(VariableDescrips.wide),
                     top = c("Variable", rep(c("Sources of Strength", "Waitlist"), each = 4)),
                     bottom = c("Variable", rep(paste0("W", 1:4), times = 2)))

vd.flex <- flextable(VariableDescrips.wide) %>%
  two_level_flex(mapping = vd.map, vert.cols = "Variable", border = border) %>%
  autofit()
