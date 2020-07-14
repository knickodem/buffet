#### packages I use for almost everything ####
pacman::p_load(haven,readxl,tidyr,stringr,lubridate,
               dplyr,purrr,tibble,forcats)

####################################
#### Functions related to dates ####

#### Today's Date ####
sysdate <- format(Sys.Date(), '%m%d%y')
# For use in markdown reports
# "`r format(Sys.Date(), '%B %d, %Y')`"

#### Changes dates back to the intended ID ####
# Excel changes how certain values are interpreted
dates_to_id <- function(x){
  x <- recode(x,"'42370'='1-1';'42401'='2-1';'42430'='3-1';'42461'='4-1';'42491'='5-1';
              c('42522','42887')='6-1';'42552'='7-1';'42583'='8-1';'42614'='9-1';
              '42644'='10-1';'42675'='11-1';'42705'='12-1'")
}


#### Change dates from factors to date ####
library(lubridate)
data <- data %>% mutate_at(vars(var1,var2,var3), ymd)

####################################


####################################################################################################
#### Descriptive and Frequency Functions ####

#### Getting summary descriptives on a continuous variable ####
## unquoted grouping variables should be specified in ...
# requires semTools for skew and kurtosis
Get_Descriptives <- function(data,ContinuousVariable,...,digits=5,AllContinuous=TRUE){
  
  groups <- quos(...)
  CV <- enquo(ContinuousVariable)
  
  data_descrip <- data %>% group_by(!!!groups) %>%
    summarize(n = sum(!is.na(!!CV)),
              Median = median(!!CV, na.rm=TRUE),
              Mean = mean(!!CV, na.rm=TRUE),
              SE = sd(!!CV, na.rm=TRUE)/sqrt(sum(!is.na(!!CV))),
              SD = sd(!!CV, na.rm=TRUE),
              Min = min(!!CV, na.rm=TRUE),
              Max = max(!!CV, na.rm=TRUE),
              Skew = suppressWarnings(skew(!!CV)[[1]]),
              Kurtosis = suppressWarnings(kurtosis(!!CV)[[1]])) %>% ungroup()
  
    if(AllContinuous==FALSE){
      
      data_descrip <- data_descrip %>%
        mutate(SE = ifelse(Min==0 & Max==1,(Mean*(1-Mean))/sqrt(n),SE),
               SD = ifelse(Min==0 & Max==1,NA,SD))
    }
  
  data_descrip <- data_descrip %>%
    mutate_if(is.numeric,~round(.,digits=digits))
  
  return(data_descrip)
}

## or just use psych::describe saved to an object
# example:
# test <- dataframe %>% select(one_of(variables)) %>%
#   psych::describe(., fast = TRUE, trim = 0, check = FALSE, ranges = FALSE, skew = FALSE)
# If grouping is needed, use describeBy(mat = TRUE)
# Not sure how one could use this within a tidy framework
# skimr package

#### Standard closed-responsed multiple-choice question with and without margin totals ####
ItemFrequencies <- sample %>% select(Exp1:Cre59) %>%
  gather(Item,Response,Exp1:Cre59) %>%
  group_by(Item,Response)%>%
  summarize(n=n())%>% spread(Response,n)

basic_table <- function(data,item){
  
  ig <- c("Q9",item);ig <- lapply(ig,as.name)
  i <- c(item);i <- lapply(i,as.name)
  
  n <- data %>% group_by_(.dots=ig) %>% summarize(Count=n()) %>% rename_("Response"=item) %>% 
    filter(is.na(Response)==FALSE) %>% mutate(n=(sum(Count))) %>% select(Q9,n) %>% unique()
  
  basic <- data %>% group_by_(.dots=ig) %>% summarize(Count=n()) %>% rename_("Response"=item) %>% 
    filter(is.na(Response)==FALSE) %>% mutate(n=sum(Count)) %>%
    ungroup() %>% mutate(Prop=paste0(round((Count/n)*100),"%")) %>% select(Q9,Response,Prop) %>%
    spread(Response,Prop) %>% mutate_all(funs(replace(., is.na(.), "0%"))) %>%
    left_join(n) %>% select(Position=Q9,n,everything())
  
  n_all <- data %>% group_by_(.dots=i) %>% summarize(Count=n()) %>% rename_("Response"=item) %>% 
    filter(is.na(Response)==FALSE) %>% mutate(n=(sum(Count))) %>% select(n) %>% unique()
  
  basic_all <- data %>% group_by_(.dots=i) %>% summarize(Count=n()) %>% rename_("Response"=item) %>% 
    filter(is.na(Response)==FALSE) %>% mutate(n=sum(Count)) %>% ungroup() %>%
    mutate(Prop=paste0(round((Count/n)*100),"%")) %>% select(Response,Prop) %>% spread(Response,Prop) %>%
    mutate_all(funs(replace(., is.na(.), "0%"))) %>%
    mutate(n=n_all[[1]],Position="All Respondents") %>% select(Position,n,everything())
  
  basic <- bind_rows(basic_all,basic)
  
  basic$Position <- ifelse(is.na(basic$Position)==TRUE,"Unspecified",basic$Position)
  
  return(basic)
}

#### Respondents allowed to select all options that apply ####
select_all_table <- function(data,i1,i2,i3=NULL,i4=NULL,i5=NULL,i6=NULL,i7=NULL,i8=NULL,i9=NULL,marginal=TRUE){
  
  ig <- c("Q9",i1,i2,i3,i4,i5,i6,i7,i8,i9);ig <- lapply(ig,as.name)
  items <- c(i1,i2,i3,i4,i5,i6,i7,i8,i9);items <- lapply(items,as.name)
  
  n <- data %>% count(Q9)
  
  if(marginal==TRUE){
    
    attempt <- data %>% select_(.dots=ig) %>% gather_("Q6","Response",items) %>% 
      group_by(Q9,Q6,Response) %>% summarize(Count=n()) %>%
      filter(is.na(Response)==FALSE) %>% left_join(n) %>%
      ungroup() %>% mutate(Prop=paste0(round((Count/n)*100),"%")) %>% 
      select(Q9,Response,Prop) %>% spread(Response,Prop) %>%
      mutate_all(funs(replace(., is.na(.), "0%"))) %>%
      left_join(n) %>% select(Position=Q9,n,everything())
    
    n_all <- sum(attempt$n)
    attempt_all <- data %>% select_(.dots=items) %>% gather_("Q6","Response",items) %>%
      group_by(Q6,Response) %>% summarize(Count=n(),n=n_all) %>%
      filter(is.na(Response)==FALSE) %>% ungroup() %>% mutate(Prop=paste0(round((Count/n)*100),"%"))%>%
      select(Response,Prop) %>% spread(Response,Prop) %>% mutate_all(funs(replace(., is.na(.), "0%"))) %>%
      mutate(n=n_all,Position="All Respondents") %>% select(Position,n,everything())
    
    attempt <- bind_rows(attempt_all,attempt)
    
    attempt$Position <- ifelse(is.na(attempt$Position)==TRUE,"Unspecified",attempt$Position)
    
  } else {
    
    attempt <- data %>% select_(.dots=ig) %>% gather_("Q6","Response",items) %>% 
      group_by(Q9,Q6,Response) %>% summarize(Count=n()) %>%
      filter(is.na(Response)==FALSE) %>% left_join(n) %>%
      ungroup() %>% mutate(Prop=round((Count/n)*100)) %>% 
      select(Q9,Response,Prop) %>% spread(Response,Prop) %>%
      mutate_all(funs(replace(., is.na(.), 0))) %>%
      left_join(n) %>% select(-Q9,-n) %>% t() %>% as.data.frame() %>%
      rownames_to_column("Item") %>% rename(Percent=V1) %>%
      arrange(desc(Percent))
    attempt$Percent <- paste0(attempt$Percent,"%")
    
  }
  
  return(attempt)
}

#### Multiple questions in an array format ####
array_table <- function(data,array){
  
  n <- data %>% select(starts_with(array)) %>% gather(Item,Response) %>% 
    filter(is.na(Response)==FALSE) %>% group_by(Item) %>% summarize(n=n())
  
  array <- data %>% select(starts_with(array)) %>% gather(Item,Response) %>% filter(is.na(Response)==FALSE) %>%
    group_by(Item,Response) %>% summarize(Count=n()) %>% mutate(n=sum(Count)) %>%
    ungroup() %>% mutate(Prop=paste0(round((Count/n)*100),"%")) %>%
    select(Item,Response,Prop) %>% spread(Response,Prop) %>% mutate_all(funs(replace(., is.na(.), "0%"))) %>%
    left_join(n) %>% select(Item,n,everything())
  
  return(array)
  
}

#### Item Frequencies (including NAs) for multiple items with similar response options ####
Get_ItemFreqs <- function(data,NAto0=FALSE){
  Freqs <- data %>% gather(Item,Response) %>%
    group_by(Item,Response)%>%
    summarize(n=n()) %>%
    spread(Response,n) %>% ungroup()
  if(NAto0==TRUE){
    Freqs <- Freqs %>%
      mutate_if(is.numeric,~replace_na(.,0))
  }
    return(Freqs)
}
#### Frequency and Proportion for a single variable (crosstabs and integration with purrr::map still needed) ####
FreqProp <- function(x, handleNA = "no", marg = NULL, varname = "Variable", percent_integer = TRUE){
  
  datable <- table(x, useNA = handleNA)
  prop <- prop.table(datable, margin = marg) %>% data.frame()
  
  if(percent_integer==TRUE){
    prop <- prop %>% mutate(Freq = round(Freq*100,0))
  }
  
  joined <- datable %>% data.frame() %>%
    left_join(prop, by = "x")
  names(joined) <- c(varname, "n", "Percent")
  

return(joined)
}

#### Crosstab - currently project specific after utilizing survey package (specifically svytable) ####
Get_Crosstab <- function(svytable, marg = NULL, percent_integer = TRUE){
  
  crosstab <- svytable[["table"]]
  stats <- svytable[["statistic"]]
  
  prop <- prop.table(crosstab, margin = marg) %>%
    data.frame() %>%
    rename(Percent = Freq)
  
  if(percent_integer==TRUE){
    prop <- prop %>% mutate(Percent = round(Percent*100,0))
  }
  
  joined <- crosstab %>% data.frame() %>%
    mutate(Freq = round(Freq,0)) %>%
    left_join(prop) %>%
    mutate(n = paste0(Freq," (",Percent,")")) %>%
    select(-Freq,-Percent) %>%
    tidyr::spread(Service_Utilization,n) %>%
    mutate(F_ndf.ddf = paste0(round(stats[["statistic"]],2)," (",paste(round(stats[["parameter"]],2),collapse=", "),")"),
           p = round(stats[["p.value"]],3))
  
  return(joined)
}

#### Frequency, proportion of, as well as # of missing, responses for survey item  outputted in a list ####
Freq <- lapply(LPS_Questions, function(x){
  x <- list(
    Question = x[1],
    Valid_Response = sum(!is.na(x[2:length(x)])),
    Missing_Response = sum(is.na(x[2:length(x)])),
    Table = data.frame(Choices=row.names(prop.table(table(x[2:length(x)]))),
                       Count=as.numeric(table(x[2:length(x)])),
                       Proportion=as.numeric(prop.table(table(x[2:length(x)])))))
})

#### Reporting above function ####
reporting <- function(x){
  for(i in 1:length(names(x))){
    cat("### Details for ",names(x[i]))
    cat("\n\n")
    cat("####",x[[i]]$Question)
    cat("\n\n")
    cat("*Number of Valid Responses:* ",x[[i]]$Valid_Response)
    cat("\n\n")
    cat("*Number of Missing Responses:* ",x[[i]]$Missing_Response)
    cat("\n\n")
    print(knitr::kable(x[[i]]$Table, digits = 2))
    cat("\n\n\n")
  }
}
#### Two-way item response table ####
Joint_Response <- function(data,group,item1,item2){
  
  JR <- data %>% group_by_(.dots=c(group,item1,item2)) %>% 
    summarize(n=n()) %>% ungroup() %>%
    group_by_(.dots=group) %>% mutate(n_group=sum(n)) %>%
    mutate(Percent = round((n/n_group)*100)) %>% ungroup() %>%
    mutate(Percent = ifelse(n_group < 15,NA,Percent))
  
  names(JR)[names(JR)==group] <- "Group"
  
  return(JR) 
}

#### Computing of correlation matrix ####
## adapted from http://www.sthda.com/english/wiki/correlation-matrix-an-r-function-to-do-all-you-need
rquery.cormat<-function(x,
                        type=c('lower', 'upper', 'full', 'flatten'),
                        digits=2,
                        usena="complete.obs",
                        graph=TRUE,
                        graphType=c("correlogram", "heatmap"),
                        col=NULL,
                        alpha = .05, ...)
{
  library(corrplot)
  # Helper functions
  #+++++++++++++++++
  # Compute the matrix of correlation p-values
  cor.pmat <- function(x, ...) {
    mat <- as.matrix(x)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  # Get lower triangle of the matrix
  getLower.tri<-function(mat){
    upper<-mat
    upper[upper.tri(mat)]<-""
    mat<-as.data.frame(upper)
    mat
  }
  # Get upper triangle of the matrix
  getUpper.tri<-function(mat){
    lt<-mat
    lt[lower.tri(mat)]<-""
    mat<-as.data.frame(lt)
    mat
  }
  # Get flatten matrix
  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  =(cormat)[ut],
      p = pmat[ut]
    )
  }
  # Define color
  if (is.null(col)) {
    col <- colorRampPalette(
      c("#67001F", "#B2182B", "#D6604D", "#F4A582",
        "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
        "#4393C3", "#2166AC", "#053061"))(200)
    col<-rev(col)
  }
  
  ## Correlation matrix
  cormat<-format(round(cor(x, use = usena, ...),digits),nsmall=digits)
  pmat<-format(round(cor.pmat(x, ...),digits),nsmall=digits)
  # Reorder correlation matrix
  # ord<-corrMatOrder(cormat, order="hclust")
  # cormat<-cormat[ord, ord]
  # pmat<-pmat[ord, ord]
  # Replace correlation coeff by symbols
  # sym<-symnum(cormat, abbr.colnames=FALSE)
  
  ## Correlogram
  if(graph & graphType[1]=="correlogram"){
    corrplot(cormat, type=ifelse(type[1]=="flatten", "lower", type[1]),
             tl.col="black", tl.srt=45,col=col,...)
  }
  else if(graphType[1]=="heatmap")
    heatmap(cormat, col=col, symm=TRUE)
  
  ## Get lower/upper triangle or flatten
  if(type[1]=="lower"){
    cormat<-getLower.tri(cormat)
    pmat<-getLower.tri(pmat)
    return(list(r=cormat, p=pmat)) #,sym=sym
  }
  else if(type[1]=="upper"){
    cormat<-getUpper.tri(cormat)
    pmat<-getUpper.tri(pmat)
    # sym=t(sym)
    return(list(r=cormat, p=pmat)) #,sym=sym
  }
  else if(type[1]=="flatten"){
    cormat<-flattenCorrMatrix(cormat, pmat) %>%
      mutate(CorSig = paste0(cor,ifelse(as.numeric(as.character(p)) < alpha, "*", " ")))
    return(cormat)
  }
  
}


#### Calculate the association between a set of variables or between a set of predictors and a dv ####
Get_Associations <- function(data,variables,type=c("nominal","polychoric","polyserial","hetcor","continuous"),
                             dv=NULL,meth="pearson"){
  
  ## For nominal-nominal associations
  # Only compares predictor to dv
  if(type=="nominal"){
    
    AllCorrs <- data.frame()
    
    for(i in variables){
      
      Vvals <- data.frame(Predictor = i,
                          Association = DescTools::CramerV(data[[i]],data[[dv]],conflev = .095))
      AllCorrs <- AllCorrs %>% bind_rows(Vvals)
      
    }
    # For ordinal-ordinal associations
  } else if(type=="polychoric"){
    
    AllCorrs <- data.frame()
    
    for(i in variables){
      
      Vvals <- data.frame(Predictor = i,
                          Association = polycor::polychor(data[[i]],data[[dv]]))[,]
      AllCorrs <- AllCorrs %>% bind_rows(Vvals)
      
      
    }
    # For ordinal-continuous associations (or vice versa)
  } else if(type=="polyserial"){
    
    AllCorrs <- data.frame()
    
    for(i in variables){
      
      Vvals <- data.frame(Predictor = i,
                          Association = polychor::polyserial(data[[i]],data[[dv]]))[,]
      AllCorrs <- AllCorrs %>% bind_rows(Vvals)
      
    }
    # for a mixture of the above as dictated by the column type
    # (i.e., some columns are numeric whereas others are characters and others are ordered factors)
  } else if(type=="hetcor"){
    
    cordata <- data %>% select(one_of(variables))
    AllCorrs <- as.data.frame(hetcor(cordata,use="pairwise.complete.obs"))
    
    # For continuous-continuous associations
  } else if(type=="continuous"){
    
    cordata <- data %>% select(one_of(variables))
    
    if(!is.null(dv)){
      
      AllCorrs <- cor(x=cordata,y=cordata[[dv]],method=meth,use="pairwise.complete.obs") %>%
        as.data.frame() %>% rownames_to_column(.,var="Predictor")
      
    } else{
      
      AllCorrs <- cor(x=cordata,method=meth,use="pairwise.complete.obs") %>%
        as.data.frame() %>% rownames_to_column(.,var="Predictor")
      
    }
    
  } else{
    
    stop("Not a valid entry for type argument.")
  }
  
  return(AllCorrs)
  
}

#### Get a frequency table, proportion table, run chisquare test, or CramersV correlation for two variables ####
## items must be in quotes
Get_Comparison <- function(data,item1,item2,type="table",usena="no",margin=NULL,conflev=NA){
  
  elbat <- table(data[[item1]],data[[item2]],useNA=usena)
  
  if(type=="table"){
    comp <- elbat
  } else if(type=="prop"){
    comp <- prop.table(elbat,margin=margin)
  } else if(type=="xsq"){
    comp <- chisq.test(elbat)
  } else if(type=="V"){
    comp <- DescTools::CramerV(elbat,conf.level=conflev)
  } else if(type=="all"){
    comp <- list(Table = elbat,
                 Prop = prop.table(elbat,margin=margin),
                 Xsq = chisq.test(elbat),
                 CramersV = DescTools::CramerV(elbat,conf.level=conflev))
  }
  return(comp)
}
##########################################################################################


####################################
#### Various Printing Functions ####

#### bolding rows in pander ####
emphasize.strong.rows(which(data$TestRIT$SPED_Category %in% c("All SPED","Non-SPED","All Students"), arr.ind = TRUE))

#### Printing with extra line breaks (not sure how well it works) and column justification with pander ####
Print_Stuff <- function(table,lab,q,comment=NULL,plot=NULL){
  
  panderOptions("knitr.auto.asis", FALSE)
  panderOptions('table.split.table', Inf)
  
  cat("\n\n")
  cat("Question ",q,". ",lab, sep="")
  cat("\n\n")
  pander(table,justify=c('left',rep('center',times=length(table)-1)))
  
  if(is.null(comment)==FALSE){
    cat("\n  &nbsp; \n")
    cat("\n\n")
    cat("Comments on Question ",q)
    cat("\n\n")
    pander(comment,justify='left')
  } else{ cat("\n&nbsp;\n")}
  
  
  if(is.null(plot)==FALSE){
    cat("\n&nbsp;\n")
    cat("\n\n")
    cat("The following bar chart compares the responses to Question ",q," for years 2016 and 2017 by respondent
        type. Note that due to rounding the numbers may not exactly sum up to 100%.")
    cat("\n\n")
    cat("\n  &nbsp; \n")
    print(plot$Plot)
    cat("\n\n")
    cat(plot$Caption)
    cat("\n  &nbsp; \n")
  } else{ cat("\n  &nbsp; \n")}
  
}

#### Printing effect size from object created from propes() in compute.es package ####
Print_EffectSize <- function(data){
  cat("**Effect size:** dprobit = ",data[["d"]],"; 95% CI = [",data[["l.d"]],", ",data[["u.d"]],"]")
}

#### Common specifications for printing kable tables ####
printkable <- function(table, digs){
  print(kable(table,digits=digs,align=c("l",rep("c",length(table)-1))))
}
###############################################################



############################
#### Plotting Functions ####
#### ggplot APA theme ####
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'),
        legend.title=element_blank())
#### Automate breaks ####
ESplot <- ESplot +
  scale_y_continuous(limits=c(ifelse(min(data[["lowerCI"]]) < labloc,min(data[["lowerCI"]]),labloc-.2),
                              ifelse(max(data[["upperCI"]]) > labloc,max(data[["upperCI"]]),labloc+.2)),
                     breaks=seq(ceiling(min(data[["lowerCI"]]) * 2) / 2, 
                                floor(max(data[["upperCI"]] * 2) / 2),.5),name=ylabel)

#### Getting position for printing percent (typically for a stacked bar chart) ####
# calculating position to print proficiency
mutate(pos = cumsum(Percent) - 0.5*Percent)
#### Plotting Achievement Level Percents ####
Plot_AchievementLevels <- function(data,xvar,yvar,fillvar,txtlabel=yvar,labelpos=NULL,
                                   xlab=xvar,ylab=yvar,filllab=fillvar,facR,facC,title=facC){
  
  ALplot <- ggplot(data,aes_string(x=xvar,y=yvar,fill=fillvar)) +
    geom_bar(stat="identity",position="stack")
  
  if(!is.null(labelpos)){
    
    ALplot <- ALplot +
      geom_text(aes_string(label=txtlabel,y=labelpos),color = "white")
  }
  
  ALplot <- ALplot +
    scale_fill_manual(values=c("#b2df8a","#33a02c","#a6cee3","#1f78b4"),name=filllab) +
    xlab(xlab) + ylab(ylab) +
    theme_bw(base_size = 16) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          plot.title = element_text(size=16,hjust = 0.5))
  
  
  
  if(!missing(facC) & !missing(facR)){
    ALplot <- ALplot +
      facet_grid(reformulate(facC,facR)) +
      ggtitle(title)
  }
  
  return(ALplot)
}


#### Creates QQ, fitted vs residual, and fitted vs observed plots ####
## See ModelCheckPlots.R for documented version with (g)lm and (g)lmer, but this one has mutlinomial and Imputed capabilities
ModelCheckPlots <- function(model, smoother = "loess",
                            observed = NULL, multinomial = FALSE, byImp = FALSE){
  
  
  if(multinomial == TRUE){
    
    Residual <- resid(model) %>% data.frame() %>%
      tibble::rownames_to_column("ID") %>% gather(Level, Residuals, -ID)
    
    FR <- fitted(model) %>% data.frame() %>%
      tibble::rownames_to_column("ID") %>% gather(Level, Fitted, -ID) %>%
      left_join(Residual, by = c("ID", "Level"))
    
  } else {
    
    FR <- data.frame(Fitted = fitted(model),
                     Residuals = resid(model)) %>%
      tibble::rownames_to_column("ID")
  }
  
  ## QQ Plot - appears I no longer need to do the calculation manually with the addition of stat_qq_line()
  # y <- quantile(FR$Residuals[!is.na(FR$Residuals)], c(0.25, 0.75))
  # x <- qnorm(c(0.25, 0.75))
  # slope <- diff(y)/diff(x)
  # int <- y[1L] - slope * x[1L]
  
  qq <- ggplot(FR, aes(sample = Residuals)) +
    stat_qq(shape = 1) + stat_qq_line() +
    labs(x = "Theoretical", y = "Observed") +
    theme_bw(base_size = 18) #+geom_abline(slope = slope, intercept = int, color="black")
  
  ## Fitted vs. Residuals Plot
  frplot <- FR %>%
    ggplot(aes(x = Fitted, y = Residuals)) +
    geom_point(shape = 1) +
    geom_hline(aes(yintercept = 0), color = "grey", size = 1, linetype = 1) +
    geom_smooth(method = smoother, se = FALSE, color = "black", size = 2, linetype = 1) +
    theme_bw(base_size = 18)
  
  if(multinomial==TRUE){
    qq <- qq + facet_wrap(~Level)
    frplot <- frplot + facet_wrap(~Level)
  }
  
  if(byImp==TRUE){
    qq <- qq + facet_wrap(~`.imp`,ncol=2) + theme(strip.background = element_blank())
    frplot <- frplot + facet_wrap(~`.imp`,ncol=2) + theme(strip.background = element_blank())
  }
  
  MCPlots <- list(QQPlot = qq,
                  FittedResidualPlot = frplot)
  
  
  if(!is.null(observed)){
    ## Fitted vs. Observed Plot
    afplot <- FR %>%
      ggplot(aes(x=Fitted,y=Observed)) +
      geom_point(shape=1) +
      geom_smooth(method="lm",se=FALSE,color="black",size=2,linetype=1) +
      theme_bw(base_size = 18)
    
    if(multinomial==TRUE){
      afplot <- afplot + facet_wrap(~Level)
    }
    
    if(byImp==TRUE){
      afplot <- afplot + facet_wrap(~`.imp`,ncol=2) + theme(strip.background = element_blank())
    }
    
    MCPlots <- list(QQPlot = qq,
                    FittedResidualPlot = frplot,
                    ActualFittedPlot = afplot)
  }
  
  if(class(model)=="lmerMod"){
    
    ## QQ Plot
    L2res <- data.frame(Residuals = ranef(model)[[1]][[1]])
    L2qq <- ggplot(L2res, aes(sample = Residuals)) +
      stat_qq(shape=1) + stat_qq_line() +
      labs(x = "Theoretical", y = "Observed") +
      theme_bw(base_size = 18)
    
    MCPlots <- list(QQPlot = qq,
                    L2QQPlot = L2qq,
                    FittedResidualPlot = frplot)
  }
  
  return(MCPlots)
}

#### Plot ROC curve and accuracy ####
## ROCR is a prerequisite
Get_roccurve <- function(data,type="rate",multi=NULL,facet=NULL){
  if(type=="rate"){
    if(!is.null(multi)){
      
      rocplot <- ggplot(data=data,aes_string(x=quote(`False positive rate`),y=quote(`True positive rate`),
                                             color=multi)) +
        geom_line(size=1.5) +
        scale_color_brewer(palette = "Dark2")
      
    } else {
      
      rocplot <- ggplot(data=data,aes(x=`False positive rate`,y=`True positive rate`)) +
        geom_line(size=1.5, color="#76BE43")
    }
    
    rocplot <- rocplot +
      scale_y_continuous(limits=c(0,1),breaks=seq(0,1,.2)) +
      scale_x_continuous(limits=c(0,1),breaks=seq(0,1,.2)) +
      labs(x = "False Positive Rate (1-Specificity)", 
           y = "True Positive Rate (Sensitivity)") +
      theme_bw(base_size = 16)
    
    if(!is.null(facet)){
      rocplot <- rocplot + facet_wrap(facet)
    }
    
  } else{
    if(!is.null(multi)){
      
      rocplot <- ggplot(data=data,aes_string(x=quote(Cutoff),y=quote(Accuracy),color=multi)) +
        geom_line(size=1.5) +
        scale_color_brewer(palette = "Dark2")
      
    } else{
      
      rocplot <- ggplot(data=data,aes(x=Cutoff,y=Accuracy)) +
        geom_line(size=1.5, color="#76BE43")
    }
    rocplot <- rocplot +
      scale_y_continuous(limits=c(0,1),breaks=seq(0,1,.2)) +
      labs(x = "Cutoff", 
           y = "Accuracy") +
      theme_bw(base_size = 16)
    
    if(!is.null(facet)){
      rocplot <- rocplot + facet_wrap(facet)
    }
    
  }
  return(rocplot)
}

#### Plot trend data with various display options ####
Get_TrendPlot <- function(data,xvar,yvar,colorvar,type,facetvar=NULL,numcol=1,points=FALSE,coninf95=FALSE,numericx=FALSE,sumoother=TRUE,
                               xlabel=xvar,ylabel=yvar,legend=FALSE,colorlabel=colorvar){
  
  regplot <- ggplot(data,aes_string(x=xvar,y=yvar,color=colorvar))
  
  if(points!=FALSE){
    
    regplot <- regplot + geom_point()
  }
  
  if(type=="smooth"){
    
    regplot <- regplot + 
      geom_smooth(se=coninf95,method="loess") +
      scale_color_discrete(guide=legend)
    
  } else if(type=="line"){
    
    regplot <- regplot + 
      geom_line(se=coninf95,method="loess") +
      scale_color_discrete(guide=legend)
    
    if(coninf95!=FALSE){
      regplot <- regplot +
        geom_errorbar()
    }
  } else{
    stop("Valid entries for the type argument are 'smooth' or 'line'.")
  }
  
  if(smoother==TRUE){
    
    regplot <- regplot + 
      stat_smooth(aes(group=1),se=TRUE,method="loess",color="black") +
      stat_summary(aes(group = 1), fun.y = mean, geom = "point",shape = 17, size = 3)
  }
  
  if(numericx==FALSE){
    
    regplot <- regplot +
      xlab(xlabel)
    
  } else{
    
    regplot <- regplot +
      scale_x_continuous(name=xlabel,limits=c(min(data[[xvar]],na.rm=TRUE),max(data[[xvar]],na.rm=TRUE)),
                         breaks=seq(min(data[[xvar]],na.rm=TRUE),max(data[[xvar]],na.rm=TRUE),1))
  }
  
  regplot <- regplot +
    scale_y_continuous(name=ylabel,limits=c(min(data[[yvar]],na.rm=TRUE),max(data[[yvar]],na.rm=TRUE)),
                       breaks=seq(min(data[[yvar]],na.rm=TRUE),max(data[[yvar]],na.rm=TRUE),1)) +
    theme_bw(base_size = 18)
  
  if(!is.null(facetvar)){
    regplot <- regplot + facet_wrap(facetvar,ncol=numcol)
  }
  
  return(regplot)
}


#### Std. Mean Diff plots ####
## By Respondent or Characteristic
Scale_Stats_Plot <- function(data,ana=NULL,char=FALSE,all=FALSE){
  
  if(char==FALSE){
    
    sub_plot <- ggplot(data=data, aes(x=Respondent,y=Std_m_diff)) +
      geom_bar(stat= "identity", fill="#007cb7") +
      geom_hline(yintercept=0) +
      # geom_label(data=ana,aes(label=Text)) +
      # geom_text(data=ana,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=Text)) +
      scale_x_discrete(name="") +
      scale_y_continuous(name="Standardized Mean Difference",limits=c(min(data$Std_min),max(data$Std_max)),
                         breaks=c(-.6,-.4,-.2,0,.2,.4,.6),
                         labels =c("-.6","-.4","-.2","0",".2",".4",".6")) +
      coord_flip() +
      theme_bw(base_size = 16)
    
    if(all==FALSE){
      
      sub_plot <- sub_plot +
        ggtitle(ana) +
        theme(plot.title = element_text(hjust = 0.5,size=12))
      
    } else {
      
      sub_plot <- sub_plot +
        facet_wrap(~Scale)
    }
  } else {
    
    sub_plot <- ggplot(data=data, aes(x=Scale,y=Std_m_diff)) +
      geom_bar(stat= "identity", fill="#007cb7") +
      # geom_text(aes(y=PerPos,label=paste0(round(Percent_Y*100),"%")),size=4,color="black") +
      geom_hline(yintercept=0) +
      scale_x_discrete(name="") +
      scale_y_continuous(name="Standardized Mean Difference",limits=c(min(data$Std_min),max(data$Std_max)),
                         breaks=c(-.6,-.4,-.2,0,.2,.4,.6),
                         labels =c("-.6","-.4","-.2","0",".2",".4",".6")) +
      # scale_fill_manual(values = "#007cb7") +
      coord_flip() +
      theme_bw(base_size = 24) +
      facet_wrap(~Facet_Label)
    
  }
  
  return(sub_plot)
}



#### Stacked bar for comparing item response by group ####
Freq_Comp_Plot <- function(data,numGroup,g2=FALSE,g1i1=FALSE,leglr=FALSE){
  
  if(g2==FALSE){
    FC_plot <- ggplot(data,aes(x=Group,y=Percent,fill=Response)) +
      geom_bar(stat="identity",position="stack") +
      geom_text(aes(label=ifelse(Percent >= 7,Percent,""),y=pos),color = "white",size=5) +
      scale_fill_brewer(palette="Dark2") +
      xlab("") + coord_flip() +
      theme_bw(base_size = 20)
    
    if(g1i1==FALSE){
      FC_plot <- FC_plot + facet_wrap(~Item,ncol=2)
    }
  } else{
    FC_plot <- ggplot(data,aes(x=Group2,y=Percent,fill=Response)) +
      geom_bar(stat="identity",position="stack") +
      geom_text(aes(label=ifelse(Percent >= 7,Percent,""),y=pos),color = "white",size=5) +
      scale_fill_brewer(palette="Dark2") +
      xlab("") + coord_flip() +
      theme_bw(base_size = 20) + 
      facet_wrap(~Group,ncol=2)
  }
  
  if(leglr==FALSE | numGroup %in% c(2,4,6,8)){
    FC_plot <- FC_plot
  } else{
    
    FC_plot <- FC_plot + theme(legend.justification=c(1,0), legend.position=c(1,leglr))
  }
  
  if(g2==FALSE & g1i1==FALSE & numGroup==1){
    FC_plot <- ggplot(data,aes(x=Item,y=Percent,fill=Response)) +
      geom_bar(stat="identity",position="stack") +
      geom_text(aes(label=ifelse(Percent >= 7,Percent,""),y=pos),color = "white",size=5) +
      scale_fill_brewer(palette="Dark2") +
      xlab("") + coord_flip() +
      theme_bw(base_size = 20)
  }
  
  return(FC_plot)
}

#### Heat map for two-way item response; also works for Correlations ####
Joint_Response_Plot <- function(data,xlab,ylab){
  
  JR_plot <- ggplot(data,aes(x=Item1,y=Item2,fill=Percent)) +
    geom_tile() +
    scale_fill_continuous(high = "#132B43", low = "#56B1F7",na.value = "white") +
    labs(x=xlab,y=ylab) +
    theme(axis.text.x=element_text(angle=-45,vjust=.8,hjust=.3)) +
    facet_wrap(~Group)
  
  return(JR_plot)
  
}
#### Depicting association between predictors and an outcome via bars ####
CorrPlot <- function(data,xvar,facet=NULL,fill=facet,legend="Y"){
  
  corrplot <- ggplot(data,aes_string(x=xvar,y="Correlation",fill=xvar)) +
    geom_bar(stat="identity",position = "dodge") +
    scale_fill_brewer(palette="Set1") +
    theme_bw(base_size=16)
  
  if(!is.null(facet)){
    
    corrplot <- corrplot +
      facet_wrap(facet)
  }
  
  if(fill==facet){
    corrplot <- corrplot + guides(fill=FALSE)
  } else if(legend=="N"){
    corrplot <- corrplot + guides(fill=FALSE)
  }
  return(corrplot)
}
#### Fixed Effects Plot ####
## To be used after getting data with the Get_Fixed function
FixedEffPlot <- function(data,sigonly=TRUE,stdor=FALSE,xlab="Predictor",ylab="Estimate",
                         colorvar="Model",facetvar=NULL,yint=0){
  
  ## Keep only significant estimates
  if(sigonly==TRUE){
    data <- data %>% filter(Sig=="Yes")
  }
  
  ## Use standardized estimates or odds ratios
  if(stdor==TRUE){
    data <- data %>% select(Model,Predictor,Level,ends_with("_std"),ends_with("_or"))
    names(data) <- c("Model","Predictor","Level","Estimate","Std.Error","Interval","Lower","Upper","Sig")
  }
  
  feplot <- data %>% ggplot(aes_string(x="Predictor",y="Estimate",
                                       ymin="Lower",ymax="Upper",colour=colorvar)) +
    geom_hline(yintercept=yint,colour="grey",size=1) +
    geom_pointrange(size=1) +
    labs(x=xlab,y=ylab) +
    coord_flip() +
    theme_bw(base_size = 20)
  
  if(!is.null(facetvar)){
    feplot <- feplot + facet_wrap(facetvar)
  }
  
  return(feplot)
  
}

## Version 2 (from SCRED project)
## Data prep before plotting
FixedEffPrep <- function(data,mod,ignorevars,ordervar,contvars,predvars){
  
  ov <- enquo(ordervar)
  
  prepped <- data %>% filter(Model==mod & !(Predictor %in% ignorevars)) %>%
    mutate(Predictor = factor(Predictor,levels=c(predvars,contvars)), #fct_reorder(Predictor,!!ov),
           ControlVars = ifelse(Predictor %in% contvars,"Control","Predictor"),
           ControlVars = factor(ControlVars,levels=c("Predictor","Control")))
  
  return(prepped)
  
}
FixedEffPlot <- function(data,yvar,ymin,ymax,sigonly=TRUE,xlab="Predictor",ylab="Estimate",
                         colorvar=NULL,shapevar=NULL,facL,facT=".",fwscale="fixed",yint=0,colorlab=colorvar){
  
  ## Keep only significant estimates
  if(sigonly==TRUE){
    data <- data %>% filter(Sig=="Yes")
  }
  
  feplot <- data %>% ggplot(aes_string(x="Predictor",y=yvar,ymin=ymin,ymax=ymax,colour=colorvar,shape=shapevar)) +
    geom_hline(yintercept=yint,colour="grey",size=1) +
    geom_pointrange(size=1) +
    labs(x=xlab,y=ylab) +
    scale_color_brewer(palette = "Paired",name=colorlab) +
    coord_flip() +
    theme_bw(base_size = 20)
  
  if(!missing(facL)){
    feplot <- feplot +
      facet_grid(reformulate(facT,facL),scales = fwscale,space="free",switch = "y") +
      theme(strip.placement = "outside",
            strip.text.y = element_text(face = "bold"), #angle = 180,vjust=1, 
            strip.background = element_blank(),
            panel.spacing = unit(0,"cm"),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor=element_blank(),
            axis.line = element_line(colour = "black"))
  }
  
  return(feplot)
  
}

#### Forest Plot (very similar to fixed effects plot from above) ####
ForestPlot <- function(data,xvrbl,yvrbl,lbl,labloc=0,facL,facT,xlabel=xvrbl,ylabel=""){
  
  xvrbl <- quo_name(enquo(xvrbl))
  yvrbl <- quo_name(enquo(yvrbl))
  
  ESplot <- data %>% mutate(nsy = labloc) %>%
    ggplot(aes_string(x=xvrbl,y=yvrbl,ymin="lowerCI",ymax="upperCI")) +
    geom_hline(yintercept=0,colour="black",size=1,linetype=2) +
    geom_pointrange(size=1)
  
  if(!missing(lbl)){
    lbl <- quo_name(enquo(lbl))
    ESplot <- ESplot +
      geom_text(aes_string(y="nsy",label=lbl),color="black",hjust = "inward", vjust = "middle")
  }
  
  ESplot <- ESplot +
    scale_y_continuous(limits=c(ifelse(min(data[["lowerCI"]]) < labloc,min(data[["lowerCI"]]),labloc-.2),
                                ifelse(max(data[["upperCI"]]) > labloc,max(data[["upperCI"]]),labloc+.2)),
                       breaks=seq(ceiling(min(data[["lowerCI"]]) * 2) / 2, 
                                  floor(max(data[["upperCI"]] * 2) / 2),.5),name=ylabel) +
    xlab(xlabel) +
    coord_flip()
  
  if(!missing(facT) & !missing(facL)){
    facL <- quo_name(enquo(facL))
    facT <- quo_name(enquo(facT))
    ESplot <- ESplot +
      facet_grid(reformulate(facT,facL),scales = "free",space="free",switch = "y")
  }
  
  ESplot <- ESplot +
    theme_bw(base_size = 20) +
    theme(strip.placement = "outside",
          strip.text.y = element_text(angle = 180,vjust=1, face = "bold"),
          strip.background = element_blank(),
          panel.spacing = unit(0,"cm"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor=element_blank(),
          axis.line = element_line(colour = "black"))
  
  return(ESplot)
}
#### Rasch Plots Extracted from mirt object ####
## Extracting data from subpar lattice plot
# Works for 'trace','rxx','info','itemscore', and 'SE' plot types
MirtPlotData <- function(mirtplot,item_order=NULL){
  
  dat <- mirtplot
  
  if(dat$condlevels[[1]][[1]]=="1"){
    
    tldat <- dat$panel.args %>% map_df(~data.frame(.x)) %>%
      bind_cols(data.frame(Item=dat$panel.args.common$groups))
    
  } else{
    
    times <- map_int(dat$panel.args,~nrow(data.frame(.x)))
    tldat <- dat$panel.args %>% map_df(~data.frame(.x)) %>%
      bind_cols(data.frame(Response=dat$panel.args.common$groups)) %>%
      bind_cols(data.frame(Item=rep(dat$condlevels$item,times=times)))
  }
  
  
  if(!is.null(item_order)){
    
    tldat <- tldat %>%
      mutate(Item=factor(Item,levels=item_order))
  }
  
  return(tldat)
}

## Creating the plot with ggplot2
# MirtPlotData is a prerequisite
# Example: plot(mirtobject,type="rxx") %>% MirtPlotData() %>%
#          MirtPlot(colorvar="Model",ylab=expression('r'['xx']~(theta)),facet=FALSE,linesize=2)
MirtPlot <- function(tldata, groupvar, color = TRUE, legendpos = "top",
                     yname = expression(P(theta)), xname = expression(theta),
                     facet = TRUE, linesize = 1){
  
  if(color == TRUE){
    
    tlplot <- ggplot(tldata,aes_string(x = "x",y = "y", color = groupvar)) +
      geom_line(size = linesize) +
      scale_color_brewer(palette = "Dark2")
    
  } else {
    
    types <- unique(tldata[[groupvar]])
    
    tlplot <- ggplot(tldata,aes_string(x = "x",y = "y", linetype = groupvar)) +
      geom_line(size = linesize) +
      scale_linetype_manual(values = c(1:length(types)))
    
  }
  
  tlplot <- tlplot +
    scale_x_continuous(name = xname, breaks = seq(-5, 5, 1)) +
    theme_bw(base_size = 20) +
    theme(legend.position = legendpos)
  
  if(max(tldata$y,na.rm=TRUE) < 1){
    
    tlplot <- tlplot +
      scale_y_continuous(name = yname, limits = c(0,1), breaks = seq(0,1,.2))
    
  } else {
    
    tlplot <- tlplot +
      ylab(yname)
  }
  
  if(facet == TRUE){
    tlplot <- tlplot +
      facet_wrap(~Item)
  }
  return(tlplot)
}

#### Plotting Thresholds in a Wright Map type manner ####
Rasch_Threshold_Plot <- function(data,multiscale=FALSE){
  
  if(multiscale==TRUE){
    deplot <- data %>% ggplot(aes(x=Item,y=par,group=Scale,colour=Scale))
  } else {
    deplot <- data %>% ggplot(aes(x=Item,y=par))
  }
  
  deplot <- deplot +
    geom_text(aes(label=Threshold),position=position_dodge(width=.7),size=6) +
    theme_bw(base_size=20) +
    scale_color_brewer(palette="Dark2") +
    scale_y_continuous(name=expression(theta),limits=c(-5.3,4),breaks=seq(-5,4,1))
  
  return(deplot)
}

#### Plotting Person Density in a Wright Map type manner ####
## Probably a way to combine with Rasch_Threshold_Plot but haven't done so yet
## This version is still relatively project-specific
Rasch_PersonDensity_Plot <- function(data,xvar,xlabel=xvar,adj=1,alph=.1,format.data=TRUE,fitlimits=FALSE){
  
  if(format.data==TRUE){
    data <- data %>%
      separate(Scale,c("Version","Scale"),sep = "-",fill="left") %>%
      mutate(Scale = as_factor(Scale),
             Version = ifelse(is.na(Version),"All",Version),
             Version = as_factor(Version))
  }
  
  deplot <- data %>% ggplot(aes_string(x=xvar,fill="Version",colour="Version")) +
    geom_density(adjust=adj,alpha=alph)
  
  if(fitlimits==TRUE){
    
    deplot <- deplot +
      geom_vline(xintercept = .5,color="grey",size=2) +
      geom_vline(xintercept = 1.5,color="grey",size=2)
  }
  
  deplot <- deplot +
    scale_x_continuous(name=xlabel,position="top") +
    scale_y_reverse() +
    coord_flip() +
    theme_bw(base_size=16) +
    facet_wrap(~Scale)
  
  return(deplot)
  
}

#### Generating Wright Maps ####
Get_WrightMap <- function(ThresholdData,PersonData,ScaleName){
  
  ThreshDf <- ThresholdData %>%
    filter(Scale == ScaleName)
  
  PersonDf <- PersonData %>%
    filter(Scale == ScaleName)
  
  maxlogit <- ifelse(max(ThreshDf$par) > max(PersonDf$F1), max(ThreshDf$par), max(PersonDf$F1))
  maxlogit <- ifelse(maxlogit > 4, maxlogit + .3, 4)
  minlogit <- ifelse(min(ThreshDf$par) < min(PersonDf$F1), min(ThreshDf$par), min(PersonDf$F1))
  minlogit <- ifelse(minlogit < -4, minlogit - .3, -4)
  
  ItemSide <- ThreshDf %>%
    ggplot(aes(x = Item, y = par)) +
    geom_text(aes(label = Threshold),position = position_dodge(width = .7), size = 6) +
    scale_x_discrete(name = paste(ScaleName, "Item")) +
    scale_y_continuous(name = "Logits", position = "right",
                       limits=c(minlogit, maxlogit), breaks = seq(trunc(minlogit), trunc(maxlogit), 1)) + #expression(theta)
    theme_bw(base_size = 20)
  
  PersonSide <- PersonDf %>%
    ggplot(aes(x = F1)) +
    geom_histogram(binwidth = .5, colour = "black", fill = "grey50") +
    scale_y_reverse(name = "Frequency") +
    scale_x_continuous(limits = c(minlogit, maxlogit), breaks = seq(trunc(minlogit), trunc(maxlogit), 1)) +
    theme_bw(base_size = 20) +
    coord_flip() +
    theme(axis.line=element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank())
  
  wrightmap <- cowplot::plot_grid(PersonSide, NULL, ItemSide, rel_widths = c(1, -0.05, 1), align = "h", nrow = 1)
  
  return(wrightmap)
}

#### Plotting Item infit and outfit ####
Rasch_ItemFit_Plot <- function(data){
  
  deplot <- data %>% ggplot(aes(x=Item,y=Mean_Square,shape=Fit)) +
    geom_hline(yintercept = 1,color="black",size=2) +
    geom_hline(yintercept = .5,color="grey",size=2) +
    geom_hline(yintercept = 1.5,color="grey",size=2) +
    geom_point(size=4) +
    scale_y_continuous(name="Mean Square",limits = c(0,2.1), breaks = seq(0,2,.5)) +
    scale_shape_manual(values=c(4,18)) + 
    theme_bw(base_size=20) +
    theme(legend.justification = c(1, 0), legend.position = c(.8, 0)) +
    facet_wrap(~Scale,ncol=2,as.table = TRUE)
}


############################


##########################################
#### Functions for handling MCA data ####

#### Loading MCA/MOD/MTAS data systematically ####
mca_load <- function(path){
  
  data <- read_excel(path)
  
  ## Standardizing column names
  # The column values and order are the same across years, but the names differ
  mca_cols <- c("TESTNAME","TESTDATE","TESTSUBJECT","TESTFORM","FORMNUMBER","FILLER_A","TESTINGDISTRICTNUMBER","TESTINGDISTRICTTYPE",
                "TESTINGSCHOOLNUMBER","GRADE","REPORTGROUPINGNUMBER","BASEITEMMACHINESCORECOUNT","BASEITEMHANDSCORECOUNT","BASEITEMMACHINESCOREPOINTS","BASEITEMHANDSCOREPOINTS","LEXILESCORELOWERRANGE",
                "LEXILESCOREUPPERRANGE","LOCALSTUDENTID","FILLER_B","ADM97","STATUSGROUP","PRIORSCOREREASONNOTSCORED","YEAROFPRIORSCORE","NAMEOFTESTTAKENFORPRIORYEAR",
                "TESTEDGRADEFORPRIORSCORE","ACCOMMODATION18","ACCOMMODATION24","FILLER_C","ACCOMMODATIONBR","ACCOMMODATIONHC","ACCOMMODATIONSC","ACCOMMODATIONHM",
                "ACCOMMODATIONSO","ACCOMMODATIONSP","ACCOMMODATIONVT","ACCOMMODATIONOL","ACCOMMODATIONCA","ACCOMMODATIONMC","ACCOMMODATIONMS","ACCOMMODATIONMT",
                "ACCOMMODATIONOA","ACCOMMODATIONSS","ACCOMMODATIONTD","ACCOMMODATION12","ACCOMMODATIONAT","LEXILESCORE","DOCUMENTCHARACTERISTIC1","DOCUMENTCHARACTERISTIC2",
                "DOCUMENTCHARACTERISTIC3","DOCUMENTCHARACTERISTIC4","DOCUMENTCHARACTERISTIC5","FILLER_D","MTASPASSAGEACCESSALL","SECONDARYDISTRICTNUMBER","SECONDARYDISTRICTTYPE","SECONDARYSCHOOLNUMBER",
                "HOMESCHOOLINDICATOR","ADULTBASICEDUCATIONINDICATOR","LEARNINGLOCATOR","TESTCODE","ACHIEVEMENTLEVEL","SCORE1LABEL","SCORE1SIGN","SCORE1",
                "SCORE1CHARACTERISTIC","SCORE1MAXIMUM","SCORE1MINIMUM","SCORE2LABEL","SCORE2SIGN","SCORE2","SCORE2CHARACTERISTIC","SCORE2MAXIMUM",
                "SCORE2MINIMUM","SCORE3LABEL","SCORE3SIGN","SCORE3","SCORE3CHARACTERISTIC","SCORE3MAXIMUM","SCORE3MINIMUM","SCORE4LABEL",
                "SCORE4SIGN","SCORE4","SCORE4CHARACTERISTIC","SCORE4MAXIMUM","SCORE4MINIMUM","SCORE5LABEL","SCORE5SIGN","SCORE5",
                "SCORE5CHARACTERISTIC","SCORE5MAXIMUM","SCORE5MINIMUM","SCORE6LABEL","SCORE6SIGN","SCORE6","SCORE6CHARACTERISTIC","SCORE6MAXIMUM",
                "SCORE6MINIMUM","SCORE7LABEL","SCORE7SIGN","SCORE7","SCORE7CHARACTERISTIC","SCORE7MAXIMUM","SCORE7MINIMUM","SCORE8LABEL",
                "SCORE8SIGN","SCORE8","SCORE8CHARACTERISTIC","SCORE8MAXIMUM","SCORE8MINIMUM","SCORE9LABEL","SCORE9SIGN","SCORE9",
                "SCORE9CHARACTERISTIC","SCORE9MAXIMUM","SCORE9MINIMUM","SCORE10LABEL","SCORE10SIGN","SCORE10","SCORE10CHARACTERISTIC","SCORE10MAXIMUM",
                "SCORE10MINIMUM","SCORE11LABEL","SCORE11SIGN","SCORE11","SCORE11CHARACTERISTIC","SCORE11MAXIMUM","SCORE11MINIMUM","SCORE12LABEL",
                "SCORE12SIGN","SCORE12","SCORE12CHARACTERISTIC","SCORE12MAXIMUM","SCORE12MINIMUM","SCORE13LABEL","SCORE13SIGN","SCORE13",
                "SCORE13CHARACTERISTIC","SCORE13MAXIMUM","SCORE13MINIMUM","SCORE14LABEL","SCORE14SIGN","SCORE14","SCORE14CHARACTERISTIC","SCORE14MAXIMUM",
                "SCORE14MINIMUM","SCORE15LABEL","SCORE15SIGN","SCORE15","SCORE15CHARACTERISTIC","SCORE15MAXIMUM","SCORE15MINIMUM","SCORE16LABEL",
                "SCORE16SIGN","SCORE16","SCORE16CHARACTERISTIC","SCORE16MAXIMUM","SCORE16MINIMUM","SCORE17LABEL","SCORE17SIGN","SCORE17",
                "SCORE17CHARACTERISTIC","SCORE17MAXIMUM","SCORE17MINIMUM","SCORE18LABEL","SCORE18SIGN","SCORE18","SCORE18CHARACTERISTIC","SCORE18MAXIMUM",
                "SCORE18MINIMUM","SCORE19LABEL","SCORE19SIGN","SCORE19","SCORE19CHARACTERISTIC","SCORE19MAXIMUM","SCORE19MINIMUM","SCORE20LABEL",
                "SCORE20SIGN","SCORE20","SCORE20CHARACTERISTIC","SCORE20MAXIMUM","SCORE20MINIMUM","SCORE21LABEL","SCORE21SIGN","SCORE21",
                "SCORE21CHARACTERISTIC","SCORE21MAXIMUM","SCORE21MINIMUM","SCORE22LABEL","SCORE22SIGN","SCORE22","SCORE22CHARACTERISTIC","SCORE22MAXIMUM",
                "SCORE22MINIMUM","SCORE23LABEL","SCORE23SIGN","SCORE23","SCORE23CHARACTERISTIC","SCORE23MAXIMUM","SCORE23MINIMUM","SCORE24LABEL",
                "SCORE24SIGN","SCORE24","SCORE24CHARACTERISTIC","SCORE24MAXIMUM","SCORE24MINIMUM","SCORE25LABEL","SCORE25SIGN","SCORE25",
                "SCORE25CHARACTERISTIC","SCORE25MAXIMUM","SCORE25MINIMUM","FILLER9","NEWTOCOUNTRYFLAG","SIGNIFICANTGAP","PRIOR2YEARLEPFLAG","PRIOR2YEARSPEFLAG",
                "FOREIGNEXCHANGESTUDENT","MTASOPERATIONALPASSAGE1","MTASOPERATIONALPASSAGE2","MTASOPERATIONALPASSAGE3","RESERVED","UIN")
  names(data) <- tolower(mca_cols)
  
  ## Remove first row that was added so column types would be read in properly
  data <- data %>% filter(testname!="Test Row")
  
  ## Changing ID variable to join with demographic data
  names(data)[names(data)=="localstudentid"] <- "StudentID"
  data$StudentID <- as.character(data$StudentID)
  
  ## Creating Year variable
  data$testdate <- as.character(data$testdate)
  data$year <- substring(data$testdate,1,4)
  
  ## Establishing consistent column types
  data$testingdistrictnumber <- as.character(data$testingdistrictnumber)
  data$testingdistricttype <- as.character(data$testingdistricttype)        
  data$testingschoolnumber <- as.character(data$testingschoolnumber)
  data$grade <- as.numeric(data$grade)
  data$yearofpriorscore <- as.character(data$yearofpriorscore)
  data$testedgradeforpriorscore <- as.character(data$testedgradeforpriorscore)
  data$priorscorereasonnotscored <- as.character(data$priorscorereasonnotscored)
  data$score1characteristic <- as.character(data$score1characteristic)
  
  
  return(data)
}

#### Returns observations and variables necessary for analysis ####
## and a separate list for observations without a valid test in case these becomes relevant
mca_short <- function(data,subject,MOD=FALSE,zeros=FALSE){
  
  ## Separating valid tests from invalid tests
  data_short <- data %>% filter(is.na(score1)==FALSE)
  no_test <- data %>% filter(is.na(score1)==TRUE)
  
  ## For valid tests, selecting only the variables relevant for analysis
  # Math
  if(subject=="M"){
    
    ## MCA Grade 11
    m_mca_11 <- data_short %>% filter(grade %in% c(9:12) & testname=="MCA-III") %>%
      select(Student:SPED_Category,instructional_setting,testname,year,testsubject,testingdistrictnumber:grade,StudentID,
             priorscorereasonnotscored:testedgradeforpriorscore,
             testcode,achievementlevel,SSCO=score1,Growth=score1characteristic,
             RSCO=score3,SES=score9)
    m_mca_11$VSS <- NA
    m_mca_11$SEV <- NA
    
    ## MCA Grades 3-8
    m_mca_38 <- data_short %>% filter(grade %in% c(3:8) & testname=="MCA-III") %>%
      select(Student:SPED_Category,instructional_setting,testname,year,testsubject,testingdistrictnumber:grade,StudentID,
             priorscorereasonnotscored:testedgradeforpriorscore,
             testcode,achievementlevel,SSCO=score1,Growth=score1characteristic,
             RSCO=score3,VSS=score8,SES=score11,SEV=score16)
    
    ## MTAS Grade 11
    m_mtas_11 <- data_short %>% filter(grade %in% c(9:12) & testname=="MTAS-III") %>%
      select(Student:SPED_Category,instructional_setting,testname,year,testsubject,testingdistrictnumber:grade,StudentID,
             priorscorereasonnotscored:testedgradeforpriorscore,
             testcode,achievementlevel,SSCO=score1,Growth=score1characteristic,
             RSCO=score2,SES=score6)
    m_mtas_11$VSS <- NA
    m_mtas_11$SEV <- NA
    
    ## MTAS Grades 3-8
    m_mtas_38 <- data_short %>% filter(grade %in% c(3:8) & testname=="MTAS-III") %>%
      select(Student:SPED_Category,instructional_setting,testname,year,testsubject,testingdistrictnumber:grade,StudentID,
             priorscorereasonnotscored:testedgradeforpriorscore,
             testcode,achievementlevel,SSCO=score1,Growth=score1characteristic,
             RSCO=score2,SES=score7)
    m_mtas_38$VSS <- NA
    m_mtas_38$SEV <- NA
    
    ## Binding all rows
    data_reformed <- bind_rows(m_mca_11,m_mca_38,m_mtas_11,m_mtas_38)
    
    if(MOD==TRUE){
      ## MOD Grade 11
      m_mod_11 <- data_short %>% filter(grade %in% c(9:12) & testname=="MOD-III") %>%
        select(Student:SPED_Category,instructional_setting,testname,year,testsubject,testingdistrictnumber:grade,StudentID,
               priorscorereasonnotscored:testedgradeforpriorscore,
               testcode,achievementlevel,SSCO=score1,Growth=score1characteristic,
               RSCO=score2,SES=score6)
      m_mod_11$VSS <- NA
      m_mod_11$SEV <- NA
      
      ## MOD Grades 3-8
      m_mod_38 <- data_short %>% filter(grade %in% c(3:8) & testname=="MOD-III") %>%
        select(Student:SPED_Category,instructional_setting,testname,year,testsubject,testingdistrictnumber:grade,StudentID,
               priorscorereasonnotscored:testedgradeforpriorscore,
               testcode,achievementlevel,SSCO=score1,Growth=score1characteristic,
               RSCO=score2,SES=score7)
      m_mod_38$VSS <- NA
      m_mod_38$SEV <- NA
      
      data_reformed <- bind_rows(data_reformed,m_mod_11,m_mod_38)
      
    } else {data_reformed <- data_reformed}
    
    ## Reading
  } else if(subject=="R"){
    
    ## MCA Grade 10
    r_mca_10 <- data_short %>% filter(grade %in% c(9:12) & testname=="MCA-III") %>%
      select(Student:SPED_Category,instructional_setting,testname,year,testsubject,testingdistrictnumber:grade,StudentID,
             priorscorereasonnotscored:testedgradeforpriorscore,
             testcode,achievementlevel,SSCO=score1,Growth=score1characteristic,
             RSCO=score3,SES=score8)
    r_mca_10$VSS <- NA
    r_mca_10$SEV <- NA
    
    ## MCA Grades 3-8
    r_mca_38 <- data_short %>% filter(grade %in% c(3:8) & testname=="MCA-III") %>%
      select(Student:SPED_Category,instructional_setting,testname,year,testsubject,testingdistrictnumber:grade,StudentID,
             priorscorereasonnotscored:testedgradeforpriorscore,
             testcode,achievementlevel,SSCO=score1,Growth=score1characteristic,
             RSCO=score3,VSS=score6,SES=score9,SEV=score12)
    
    
    ## MTAS Grade 10
    r_mtas_10 <- data_short %>% filter(grade %in% c(9:12) & testname=="MTAS-III") %>%
      select(Student:SPED_Category,instructional_setting,testname,year,testsubject,testingdistrictnumber:grade,StudentID,
             priorscorereasonnotscored:testedgradeforpriorscore,
             testcode,achievementlevel,SSCO=score1,Growth=score1characteristic,
             RSCO=score2,SES=score7)
    r_mtas_10$VSS <- NA
    r_mtas_10$SEV <- NA
    
    ## MTAS Grades 3-8
    r_mtas_38 <- data_short %>% filter(grade %in% c(3:8) & testname=="MTAS-III") %>%
      select(Student:SPED_Category,instructional_setting,testname,year,testsubject,testingdistrictnumber:grade,StudentID,
             priorscorereasonnotscored:testedgradeforpriorscore,
             testcode,achievementlevel,SSCO=score1,Growth=score1characteristic,
             RSCO=score2,SES=score7)
    r_mtas_38$VSS <- NA
    r_mtas_38$SEV <- NA
    
    ## Binding all rows
    data_reformed <- bind_rows(r_mca_10,r_mca_38,r_mtas_10,r_mtas_38)
    
    if(MOD==TRUE){
      ## MOD Grade 10
      r_mod_10 <- data_short %>% filter(grade %in% c(9:12) & testname=="MOD-III") %>%
        select(Student:SPED_Category,instructional_setting,testname,year,testsubject,testingdistrictnumber:grade,StudentID,
               priorscorereasonnotscored:testedgradeforpriorscore,
               testcode,achievementlevel,SSCO=score1,Growth=score1characteristic,
               RSCO=score2,SES=score5)
      r_mod_10$VSS <- NA
      r_mod_10$SEV <- NA
      
      ## MOD Grades 3-8
      r_mod_38 <- data_short %>% filter(grade %in% c(3:8) & testname=="MOD-III") %>%
        select(Student:SPED_Category,instructional_setting,testname,year,testsubject,testingdistrictnumber:grade,StudentID,
               priorscorereasonnotscored:testedgradeforpriorscore,
               testcode,achievementlevel,SSCO=score1,Growth=score1characteristic,
               RSCO=score2,SES=score5)
      r_mod_38$VSS <- NA
      r_mod_38$SEV <- NA
      
      data_reformed <- bind_rows(data_reformed,r_mod_10,r_mod_38)
      
    } else {data_reformed <- data_reformed}
    
    ## Science  
  } else{
    
    ## MCA Grade 10
    s_mca_10 <- data_short %>% filter(grade %in% c(9:12) & testname=="MCA-III") %>%
      select(Student:SPED_Category,instructional_setting,testname,year,testsubject,testingdistrictnumber:grade,StudentID,
             priorscorereasonnotscored:testedgradeforpriorscore,
             testcode,achievementlevel,SSCO=score1,Growth=score1characteristic,
             RSCO=score3,SES=score13)
    s_mca_10$VSS <- NA
    s_mca_10$SEV <- NA
    
    ## MCA Grades 3-8
    s_mca_38 <- data_short %>% filter(grade %in% c(3:8) & testname=="MCA-III") %>%
      select(Student:SPED_Category,instructional_setting,testname,year,testsubject,testingdistrictnumber:grade,StudentID,
             priorscorereasonnotscored:testedgradeforpriorscore,
             testcode,achievementlevel,SSCO=score1,Growth=score1characteristic,
             RSCO=score3,SES=score8)
    s_mca_38$VSS <- NA
    s_mca_38$SEV <- NA
    
    ## MTAS Grade 10
    s_mtas_10 <- data_short %>% filter(grade %in% c(9:12) & testname=="MTAS-III") %>%
      select(Student:SPED_Category,instructional_setting,testname,year,testsubject,testingdistrictnumber:grade,StudentID,
             priorscorereasonnotscored:testedgradeforpriorscore,
             testcode,achievementlevel,SSCO=score1,Growth=score1characteristic,
             RSCO=score2,SES=score5)
    s_mtas_10$VSS <- NA
    s_mtas_10$SEV <- NA
    
    ## MTAS Grades 3-8
    s_mtas_38 <- data_short %>% filter(grade %in% c(3:8) & testname=="MTAS-III") %>%
      select(Student:SPED_Category,instructional_setting,testname,year,testsubject,testingdistrictnumber:grade,StudentID,
             priorscorereasonnotscored:testedgradeforpriorscore,
             testcode,achievementlevel,SSCO=score1,Growth=score1characteristic,
             RSCO=score2,SES=score7)
    s_mtas_38$VSS <- NA
    s_mtas_38$SEV <- NA
    
    ## Binding all rows
    data_reformed <- bind_rows(s_mca_10,s_mca_38,s_mtas_10,s_mtas_38)
    data_reformed$GradeEnrolled <- ifelse(data_reformed$GradeEnrolled %in% c("09","10","11","12"),
                                          "HS",data_reformed$GradeEnrolled)
  }
  
  data_reformed$SSCO <- as.numeric(data_reformed$SSCO)
  data_reformed$RSCO <- as.numeric(data_reformed$RSCO)
  data_reformed$SES <- as.numeric(data_reformed$SES)
  data_reformed$VSS <- as.numeric(data_reformed$VSS)
  data_reformed$SEV <- as.numeric(data_reformed$SEV)
  data_reformed$grade <- as.character(data_reformed$grade)
  
  if(zeros==TRUE){
    data_reformed$SSCO <- data_reformed$SSCO/100
    data_reformed$RSCO <- data_reformed$RSCO/100
    data_reformed$SES <- data_reformed$SES/100
    data_reformed$VSS <- data_reformed$VSS/100
    data_reformed$SEV <- data_reformed$SEV/100
  } else{data_reformed <- data_reformed}
  
  if(nrow(no_test)==0){
    short <- data_reformed
  } else {
    short <- list(valid_tests=data_reformed,
                  no_test=no_test)
  }
  
  return(short)
}


#### MCA Gap Calculations ####

## Aggregating by various groups
MCA_Aggregate <- function(data,schoolID,...,sch_n=1,group_n=10,MCAdigs=0,SDdigs=2){
  
  ## Filtering schools with at least the minimum number of students
  data <- data %>% filter(!is.na(MEMBER) & MEMBER >= sch_n)
  
  ## Gathering grouping variables
  schid <- enquo(schoolID)
  gpvars <- quos(...)
  
  ## Number of schools within group
  SchoolCount <- data %>% group_by(!!!gpvars,!!schid) %>%
    summarize(nStudents = sum(!is.na(SSCO))) %>%
    summarize(nSchools = n())
  
  ## Calculating average MCA score by grouping variables,
  # number of schools within grouping variables
  GroupScores <- data %>% group_by(!!!gpvars) %>%
    summarize(nStudents = sum(!is.na(SSCO)),
              MCA = round(mean(SSCO, na.rm=TRUE),MCAdigs),
              sd_MCA = round(sd(SSCO,na.rm=TRUE),SDdigs)) %>% ungroup() %>%
    mutate(MCA = ifelse(nStudents < group_n,NA,MCA)) %>%
    left_join(SchoolCount)
  
  return(GroupScores) 
  
}

## Calculates gap in MCA scores that have been aggregated using MCA_Aggregate
MCAGapCalc <- function(data,gapvar,reference,refsec=FALSE){
  
  ## Defining a function for adding or subtracting columns which allows for dynamic
  # inputs when using mutate_at
  plusminus <- function(first,second,pm,switch=refsec){
    if(pm=="p"){
      first + second
    } else if(pm=="m"){
      if(switch==FALSE){
        first - second
      } else{
        second - first
      }
    } else {
      stop("whatever")
    }
  }
  
  ## Defining quosures
  gapv <- enquo(gapvar)
  gapvn <- quo_name(gapv)
  ref <- enquo(reference)
  
  ## Saving levels of gap variable
  levs <- levels(data[[quo_name(gapv)]])
  
  ## Putting data in wide format to calculate MCA gap and average score
  gapdat <- data %>% select(-nStudents, -sd_MCA, -nSchools) %>% spread_(gapvn, "MCA")
  avgdat <- data %>% select(-nStudents, -sd_MCA, -nSchools) %>% spread_(gapvn, "MCA")
  
  ## Calculating gaps
  gapdat <- gapdat %>%
    mutate_at(levs,funs(gap = plusminus(!!ref,.,pm="m"))) %>%
    gather_(gapvn,"Gap",paste0(levs,"_gap")) %>%
    mutate(!!gapvn := str_replace(!!gapv,"_gap","")) %>%
    select(-one_of(levs))
  
  ## Calculating average
  avgdat <- avgdat %>%
    mutate_at(levs,funs(avg = plusminus(!!ref,.,pm="p")/2)) %>%
    gather_(gapvn,"Avg",paste0(levs,"_avg")) %>%
    mutate(!!gapvn := str_replace(!!gapv,"_avg","")) %>%
    select(-one_of(levs))
  
  ## Joining with original data, coding reference group gap as NA, and
  # ## Creating location for school n or student n display
  data <- data %>% left_join(gapdat) %>%
    left_join(avgdat) %>%
    mutate(Gap = ifelse((!!gapv)==quo_name(ref),NA,Gap),
           nloc = ifelse((!!gapv)==quo_name(ref),MCA+6,MCA-6),
           !!gapvn := factor(!!gapv,levels=levs))
  
  return(data)
}

##########################################

##############################################################
#### Regression Results Functions ####
## Mostly for lmer and glmer, but also lm, glm, multinom (from nnet package)
#### Calculate ICC from lmer or glmer object ####
## Original version
Get_ICC <- function(model,groupingvar,logistic=FALSE){
  if(logistic==FALSE){
    ICC <- VarCorr(model)[[groupingvar]][1]/(VarCorr(model)[[groupingvar]][1] + attr(VarCorr(model),"sc")^2)
  } else {
    ICC <- VarCorr(model)[[groupingvar]][1]/(VarCorr(model)[[groupingvar]][1] + (pi^2/3))
  }
  return(ICC)
}

## Updated varsion, which includes calculating the design effect
Get_ICC <- function(model, type = c("1","2","DE"), logistic = FALSE){
  
  vc <- VarCorr(model)
  
  if(logistic == FALSE){
    residvar <- attr(vc,"sc")^2
  } else {
    residvar <- pi^2/3
  }
  
  ICC <- vc[[1]][[1]] / (vc[[1]][[1]] + residvar)
  
  if(type == "1"){
    return(ICC)
  } else {
    k <- getME(model,"n") / getME(model,"l_i")
    attributes(k) <- NULL
  }
  
  if(type == "2") {
    ICC2 <- (k*ICC) / (1 + (k - 1)*ICC)
    return(ICC2)
  } else if(type == "DE"){
    DE <- 1 + (k - 1)*ICC
    return(DE)
  }
}

#### Calculate Variance Explained for lmer or glmer object from an unconditional model ####
Variance_Explained <- function(unc.model, model, logistic = FALSE){
  
  vc.unc <- VarCorr(unc.model)
  vc.mod <- VarCorr(model)
  
  tau00 <- (vc.unc[[1]][[1]] - vc.mod[[1]][[1]]) / vc.unc[[1]][[1]]
  
  if(logistic ==  FALSE){
    Components <- list(
      tau00 = tau00,
      sigma2 = (attr(vc.unc,"sc")^2 - attr(vc.mod,"sc")^2) / attr(vc.unc,"sc")^2
    )
    return(Components)
    
  } else {
    return(tau00)
  }
}


#### Get fitted values from lmer or glmer object ####
## Values are added to original dataframe or additional models can be specified in ... and the results from the models
## are combined in long format
# requires lme4, broom
Get_Fitted_Values <- function(data,model,groupingvar,...,logistic=FALSE){
  
  gpvar <- enquo(groupingvar)
  gpname <- quo_name(gpvar)
  
  allmod <- list(model,...)
  allmodeldata <- data.frame()
  
  for(i in 1:length(allmod)){
    
    ## Extract predicted values from model object
    modeldata <- suppressWarnings(augment(allmod[[i]])) # get model values in a dataframe
    outvar <- names(modeldata)[[2]] # get name of outcome variable
    modeldata <- modeldata %>%
      select(.rownames,!!(gpvar),.fitted:.fixed) %>% # selected all variables in the model along with .fitted and .resid
      mutate(!!gpname := as.character(!!gpvar)) # convert grouping variable from factor to character
    
    if(logistic==TRUE){
      
      modeldata <- modeldata %>%
        mutate(Probability_Correct = exp(.fitted)/(exp(.fitted)+1)) # change fitted values from logodds to probabilities
    }
    
    ## Add random effects to fitted values and fixed effects
    reffs <- ranef(allmod[[i]])[[1]] %>% rownames_to_column(var=gpname)
    REnames <- paste0(rep("RE_",times=length(names(reffs)[-1])),names(reffs)[-1])
    names(reffs) <- c(gpname,REnames)
    modeldata <- modeldata %>% left_join(reffs)
    
    modeldata <- modeldata %>% mutate(Model = outvar) %>%
      select(Model,everything())
    
    allmodeldata <- allmodeldata %>% bind_rows(modeldata)
  }
  
  if(missing(...)){
    
    allmodeldata <- allmodeldata %>% select(-Model)
    
    ## Join model data with original data
    datawithfittedvals <- data %>% rownames_to_column(var=".rownames") %>%
      left_join(allmodeldata) %>% unique()
    
  } else{
    
    datawithfittedvals <- allmodeldata
    
  }
  
  return(datawithfittedvals)
  
}


#### Get regression coefficiencts ####
## Quick and simple version when interested in details from a single model from lmer or glmer object
Get_Coefficients <- function(model,logistic=FALSE,partialsd=TRUE){
  
  if(logistic==FALSE){
    ## Standardized coefficients
    standardized <- std.coef(model,partial.sd=partialsd) %>% data.frame() %>%
      rownames_to_column("term") %>% rename(Standardized=Estimate.,Standardized_SE=Std..Error.)
    
    ## Adding standardized to the unstandardized coefficients
    coefficients <- tidy(model) %>%
      left_join(standardized,by="term")
    colnames(coefficients) <- str_to_title(colnames(coefficients))
    
  } else {
    
    ## Extracting logodds
    coefficients <- tidy(model)
    colnames(coefficients) <- str_to_title(colnames(coefficients))
    
    ## Calculating Odds Ratio and Probability from the logodds estimates
    coefficients <- coefficients %>%
      mutate(Odds_Ratio = ifelse(Group=="fixed",exp(Estimate),NA),
             Probability_Correct = Odds_Ratio/(Odds_Ratio+1))
  }
  
  return(coefficients)
  
}


## More complex, but more publish ready results that can be used in CombineToPrint type functions
## for comparing multiple models (glmer has 2 columns per model whereas lmer as 1)
# requires broom, MuMIn
Get_HLM_Coefs <- function(model,logistic=FALSE,standardized=FALSE,partialsd=TRUE,digits=5,modelname="Model",alpha=.05){
  
  ## critical value for significance
  critval <- qnorm((alpha/2),lower.tail = FALSE)
  
  ## Create table category labels
  fixed <- data.frame(term="Fixed Effects",Estimate=NA)
  random <- data.frame(term="Random Effects",Estimate=NA)
  
  ## Get unstandardized coefficients and standard errors; combine into a single column
  coefficients <- tidy(model)
  
  ## Extract random effects and convert SDs to Variance
  varest <- coefficients %>% filter(str_detect(term,"sd_")) %>%
    mutate(term=str_replace(term,"sd_","Var_"),
           Estimate = sprintf(paste0("%.",(digits+1),"f"),round(estimate^2,(digits+1)))) %>%
    select(term,Estimate)
  
  if(standardized==FALSE){
    
    ## Keep only fixed effects
    coefficients <- coefficients %>% filter(group=="fixed") %>%
      mutate(Interval = std.error*critval,
             Sig = ifelse((abs(estimate)-abs(Interval))>0,"*"," "),
             Estimate = paste0(sprintf(paste0("%.",digits,"f"),round(estimate,digits)),
                               " [",sprintf(paste0("%.",digits,"f"),round((estimate-Interval),digits)),
                               ", ",sprintf(paste0("%.",digits,"f"),round((estimate+Interval),digits)),"]",Sig))
    
    if(logistic==TRUE){
      ## Add odds ratio
      coefficients <- coefficients %>%
        mutate(OR = sprintf(paste0("%.",digits,"f"),round(exp(estimate),digits))) %>%
        select(term,Estimate,OR)
      
    } else {
      
      coefficients <- coefficients %>%
        select(term,Estimate)
    }
    
    
  } else{
    
    ## Standardized coefficients
    coefficients <- std.coef(model,partial.sd=partialsd) %>% data.frame() %>%
      rownames_to_column("term") %>%
      mutate(Interval = Std..Error.*critval,
             Sig = ifelse((abs(Estimate.)-abs(Interval))>0,"*"," "),
             Estimate = paste0(sprintf(paste0("%.",digits,"f"),round(Estimate.,digits)),
                               " [",sprintf(paste0("%.",digits,"f"),round((Estimate.-Interval),digits)),
                               ", ",sprintf(paste0("%.",digits,"f"),round((Estimate.+Interval),digits)),"]",Sig)) %>%
      select(term,Estimate)
    
  }
  
  ## Bind fixed and random effect estimates along with category labels
  asone <- fixed %>% bind_rows(coefficients,random,varest)
  
  if(logistic==TRUE){
    names(asone) <- c("Predictor",paste("B [CI]",modelname),paste("OR",modelname))
  } else{
    names(asone) <- c("Predictor",modelname)  
  }
  
  return(asone)
  
}


#### Extracting fixed effects from single or multilevel, linear or logistic models
# requires broom, MuMIn
# Look into incorporating confint() rather than my own calculations, although so far I haven't found problems with my versions
Get_Fixed <- function(model, modelname, multilevel = FALSE, logistic = c("binary","multinomial","poisson"),
                      partialsd = TRUE, alpha = .05, digits = Inf, pnames = NULL){
  
  if(length(logistic)>1){
    logistic <- "linear"
  }
  
  ## critical value for significance from normal distribution
  critval <- qnorm((alpha/2),lower.tail = FALSE)
  
  ## Get unstandardized coefficients and standard errors and keep only fixed effects
  coefficients <- broom::tidy(model)
  # CI <- confint_tidy(model)
  
  if(multilevel==TRUE){
    coefficients <- coefficients %>% filter(group=="fixed")
  }
  
  if(logistic=="multinomial"){
    coefficients <- coefficients %>%
      mutate(estimate = log(estimate))
  }
  
  coefficients <- coefficients %>%
    mutate(Interval = std.error*critval,
           Lower = estimate-Interval,
           Upper = estimate+Interval,
           Sig = ifelse((abs(estimate)-abs(Interval))>0,"Yes","No")) %>%
    select(Predictor=term,Estimate=estimate,Std.Error=std.error,Interval:Sig,everything())
  
  if(logistic %in% c("binary","multinomial","poisson")){
    
    ## Add odds ratio and probability
    coefficients <- coefficients %>%
      mutate_at(vars(Estimate:Upper),list(OR=exp(.)))
    
    if(logistic=="multinomial"){
      coefficients <- coefficients %>% select(Level=y.level,everything())
    }
    
  } else {
    
    ## Standardized coefficients
    stdcoefs <- MuMIn::std.coef(model,partial.sd=partialsd) %>% data.frame() %>%
      rownames_to_column("term") %>%
      mutate(Interval_std = Std..Error.*critval,
             Lower_std = Estimate.-Interval_std,
             Upper_std = Estimate.-Interval_std,
             Sig_std = ifelse((abs(Estimate.)-abs(Interval_std))>0,"Yes","No")) %>%
      select(Predictor=term,Estimate_std=Estimate.,Std.Error_std=Std..Error.,ends_with("_std"),df)
    
    ## Joining unstandardized and standardized
    coefficients <- coefficients  %>%
      left_join(stdcoefs,by="Predictor")
  }
  
  ## Joining unstandardized and standardized
  coefficients <- coefficients  %>%
    mutate(Model=modelname) %>%
    select(Model,everything()) %>%
    mutate_if(is.numeric,~round(.,digits=digits))
  
  ## Changing Predictor names from variable codes to presentable labels
  if(!is.null(pnames)){
    coefficients <- coefficients %>%
      mutate(Predictor = pnames)
  }
  
  return(coefficients)
  
}


#### HLM Model Overview ####
Get_HLM_ModOverview <- function(model,modelname,digits,logistic=FALSE){
  
  n <- getME(model,"n")
  g <- getME(model,"l_i")
  
  MO <- data.frame(Predictor = c("n (groups)","Intraclass. Corr.","Design Effect"),
                   Value = c(paste0(n," (",g,")"),
                             sprintf(paste0("%.",digits,"f"),round(Get_ICC(model,type="1",logistic),digits)),
                             sprintf(paste0("%.",digits,"f"),round(Get_ICC(model,type="DE",logistic),digits))))
  names(MO) <- c("Predictor",modelname)
  
  return(MO)
}


#### Extracting AIC, BIC, and residual DF ####
## requires broom
Get_HLM_Fit <- function(model,digits=1,modelname="Model"){
  
  fits <- glance(model) %>%
    select(`Residual df`=df.residual,AIC,BIC) %>%
    t() %>% as.data.frame() %>%
    rownames_to_column("Predictor") %>%
    mutate(V1=as.character(round(V1,digits)))
  
  names(fits) <- c("Predictor",modelname)
  
  return(fits)
}


#### R2 ####
# produces the same as broom.mixed::tidy(., effects = "ran_pars", scales = "vcov"); uses formulas from Lorah (2018)
HLM_R2 <- function(unc.model, model){
  
  vc.unc <- VarCorr(unc.model)
  vc.mod <- VarCorr(model)
  
  R2 <- 1 - ((attr(vc.mod,"sc")^2 + vc.mod[[1]][[1]]) / (attr(vc.unc,"sc")^2 + vc.unc[[1]][[1]]))
  
  return(R2)
  
}
#### n, Variance Explained, R2 estimates, and deviance-based statistics from single or multilevel models ####
##  Get_ICC and Variance_Explained functions embedded
## Need to update given updates to embedded functions
## requires MuMIn and DescTools
All_R2 <- function(model,valuename="Value",unc.model,groupingvar,digits=3,logistic=FALSE,poisson=FALSE){
  
  ## ICC and Variance Explained Helper functions
  Get_ICC <- function(model,groupingvar,logistic=FALSE){
    if(logistic==FALSE){
      ICC <- VarCorr(model)[[groupingvar]][1]/(VarCorr(model)[[groupingvar]][1] + attr(VarCorr(model),"sc")^2)
    } else {
      ICC <- VarCorr(model)[[groupingvar]][1]/(VarCorr(model)[[groupingvar]][1] + (pi^2/3))
    }
    return(ICC)
  }
  
  ## Calculate Variance Explained for lmer or glmer object from an unconditional model ##
  Variance_Explained <- function(unc.model, model,groupingvar,logistic=FALSE){
    
    if(logistic==FALSE){
      Components <- list(
        tau00 = (VarCorr(unc.model)[[groupingvar]][1] - VarCorr(model)[[groupingvar]][1])/VarCorr(unc.model)[[groupingvar]][1],
        sigma2 = (attr(VarCorr(unc.model),"sc")^2 - attr(VarCorr(model),"sc")^2)/attr(VarCorr(unc.model),"sc")^2
      )
      return(Components)
    } else{
      tau00 = (VarCorr(unc.model)[[groupingvar]][1] - VarCorr(model)[[groupingvar]][1])/VarCorr(unc.model)[[groupingvar]][1]
      return(tau00)
    }
  }
  
  ## Gather deviance-based measures
  ABIC <- data.frame(Predictor = c("AIC","BIC","logLik"),
                     Value = c(AIC(model),BIC(model),logLik(model))) %>%
    mutate(Value = as.character(round(Value,1)))
  
  ## For single level models
  if(missing(unc.model)){
    
    ng <- data.frame(Predictor="n",Value=as.character(length(model[["fitted.values"]])))
    
    if(logistic==TRUE){
      
      ## Calculate R2 and put in a dataframe
      theR2s <- PseudoR2(model,which=c("McFadden","CoxSnell","Nagelkerke")) %>% data.frame() %>%
        rownames_to_column("Predictor") %>% select(.,Predictor,Value=`.`) %>%
        filter(!is.na(Value)) %>%
        mutate(Predictor = recode(Predictor,"'McFadden'='McFadden R2';'CoxSnell'='Cox-Snell R2';'Nagelkerke'='Nagelkerke R2'"))
      
    } else {
      
      theR2s <- data.frame(Predictor = c("R2","adj.R2"),
                           Value = c(summary(model)$r.squared,summary(model)$adj.r.squared))
    }
    ## For multilevel models
  } else{
    
    n <- length(summary(model)$residuals)
    g <- summary(model)$ngrps
    
    ng <- data.frame(Predictor = c("n","groups"),
                     Value = as.character(c(n,g)))
    
    
    ## Calculate reduction in Tau00 and Sigma2 and put in a dataframe
    ICC <- Get_ICC(unc.model,groupingvar,logistic=logistic)
    VarEx <- Variance_Explained(unc.model,model,groupingvar,logistic=logistic)
    
    if(logistic==FALSE){
      
      VarExDF <- data.frame(Predictor=c("ICC","% Reduction - Tau00","% Reduction - Sigma2"),Value=c(ICC,VarEx[[1]],VarEx[[2]]))
      
      ## Calculate Marginal and Conditional glmm R2 and put in a dataframe
      r2glmm <- r.squaredGLMM(model) %>% t() %>% data.frame() %>%
        rownames_to_column("Predictor") %>%
        mutate(Predictor = c("Marginal GLMM R2","Conditional GLMM R2"))
      names(r2glmm) <- c("Predictor","Value")
      
      ## Bind VarEx and R2
      theR2s <-  VarExDF %>% bind_rows(r2glmm)
      
    } else{
      
      VarExDF <- data.frame(Predictor=c("ICC","% Reduction - Tau00"),Value=c(ICC,VarEx[[1]]))
      
      ## Calculate likelihood ratio pseudo r2
      CS <- r.squaredLR(model,unc.model)
      Nag <- attributes(CS)[[1]]
      if(poisson==FALSE){
        McF <- 1-(logLik(model)/logLik(unc.model))
      } else{
        vpars <- function(m) {
          nrow(m)*(nrow(m)+1)/2
        }
        model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
        rdf <- nrow(model.frame(model))-model.df
        rp <- residuals(model,type="pearson")
        Pearson.chisq <- sum(rp^2)
        prat <- Pearson.chisq/rdf
        McF <- 1-((logLik(model)+(model.df*prat))/logLik(unc.model))
        
      }
      r2lrdf <- data.frame(Predictor=c("Cox-Snell R2","Nagelkerke R2","McFadden R2"),Value=c(CS,Nag,McF))
      
      ## Bind VarEx and R2
      theR2s <-  VarExDF %>% bind_rows(r2lrdf)
    }
  }
  
  ## Finalize dataframe
  theR2s <-  theR2s %>% mutate(Value=sprintf(paste0("%.",digits,"f"),round(Value,digits)))
  theR2s <- ng %>% bind_rows(theR2s,ABIC)
  names(theR2s) <- c("Predictor",valuename)
  
  return(theR2s)
}


#### Calculates eta or omega squared from single-level model ####
Get_EtaOmega <- function(model, eo = c("eta","omega"), part = TRUE, ci = .95, digs = 2){
  
  if(eo == "eta"){
    
    effdf <- sjstats::eta_sq(model, partial = part, ci.lvl = ci) %>%
      mutate_if(is.numeric, ~format(round(., digs), nsmall = digs))
    
  } else if(eo == "omega"){
    
    effdf <- sjstats::omega_sq(model, partial = part, ci.lvl = ci) %>%
      mutate_if(is.numeric, ~format(round(., digs), nsmall = digs))
    
  }
  return(effdf)
}

#### Combining Model Result Tables in a manner suitable for printing ####
## Combines results from All_HLM_R2, Get_HLM_Fit, and Get_HLM_Coefs for multiple models
## Currently set up for a specific project rather than being generalizable
CombineModelstoPrint <- function(modellist,standardized=FALSE,partialsd=TRUE,r2digits=3,coefdigits=2,fitdigits=1){
  
  ## Combine all models into a list and create sample dataframes for coefficients and fits
  allmodels <- list(modellist[[6]],modellist[[5]],modellist[[4]],
                    modellist[[3]],modellist[[2]],modellist[[1]])
  ModCombo <- data.frame(Predictor="Sample") %>% mutate_all(as.character)
  
  for(l in 1:length(allmodels)){
    
    if(l!=length(allmodels)){
      R2 <- All_HLM_R2(allmodels[[length(allmodels)]],allmodels[[l]],"SchID",valuename=as.character(l),digits=r2digits)
    } else {
      R2 <- data.frame(Predictor=c("Tau00 Reduction","Sigma2 reduction","Marginal GLMM R2","Conditional GLMM R2"),
                       Value=NA)
      names(R2) <- c("Predictor",as.character(l))
    }
    
    Fits <- Get_HLM_Fit(allmodels[[l]],digits=fitdigits,modelname=as.character(l))
    Coefs <- Get_HLM_Coefs(allmodels[[l]],standardized=standardized,partialsd=partialsd,
                           digits=coefdigits,modelname=as.character(l))
    
    TogetherNow <- R2 %>% bind_rows(Fits,Coefs)
    
    ModCombo <- ModCombo %>% full_join(TogetherNow,by="Predictor")
    
  }
  
  ## Remove Sample row
  ModCombo <- ModCombo %>% filter(Predictor!="Sample") %>%
    mutate(Predictor = str_replace(Predictor,"Var_","")) %>%
    mutate(Predictor = str_replace(Predictor,".SchID","")) %>%
    mutate(Predictor = str_replace(Predictor,"Observation.","")) %>%
    mutate(Predictor = str_replace(Predictor,"_cent","")) %>%
    mutate(Predictor = str_replace(Predictor,"(Intercept)","Intercept")) %>%
    mutate(Predictor = str_replace(Predictor,"_grand"," L2")) %>%
    select(Predictor,Unconditional=`6`,`FRL Only`=`5`,`Race Only`=`4`,
           `FRL and Race`=`3`,`All Level 1`=`2`,Full=`1`)
  
  return(ModCombo)
}
######################################################################
#### Example of using character string in regression model and using nest() and map() functions ####
## Defining regression model
RegVars <- ReportData %>% select(Year_2,Hmong,US_Born,Boy,Intact_Family,
                                 Fifth_Grade,Sixth_Grade,School_B,School_C,Age) %>% names()
RegMod <- paste0("Score ~ 1 + ",paste(RegVars,collapse = " + ")," + (1|MatchID)")
## Running unconditional and conditional models for each scale
HLMmods <- ReportData %>% select(MatchID,Parent_Involvement.Home,Parents_Availability,Positive_Parenting,
                                 Extended_Family_Support,School_Connection,School_Attachment,Q6_50_1,
                                 Fluency_and_Literacy.English,Fluency_and_Literacy.Parent_Home_Language,Academic_Competence,
                                 School_B:Year_2) %>%
  gather(Scale,Score,Parent_Involvement.Home:Academic_Competence) %>%
  group_by(Scale) %>% nest() %>%
  mutate(Unconditional = map(data,~lmer(Score ~ 1 + (1|MatchID),data=.,REML=FALSE)),
         Full = map(data,~lmer(as.formula(RegMod),data=.,REML=FALSE)))
## Extracting model results
## Model overview, fixed and random effects, and model fit
FullModels <- HLMmods %>%
  unnest(Unconditional %>% map(~Get_HLM_ModOverview(.,"Model",gpvar="MatchID",digits=2))) %>% # n, num of students, ICC, Design effect
  bind_rows(HLMmods %>% unnest(Full %>% map(~Get_HLM_Coefs(.,digits=2)))) %>%                 # fixed and random effects estimates
  bind_rows(HLMmods %>% unnest(Full %>% map(~All_R2(.,"Model",digits=1)))) %>%                # R2 estimates, AIC
  mutate(Scale = str_replace_all(Scale,"_"," "),
         Scale = str_replace(Scale,"\\."," - "),
         Scale = as_factor(Scale),
         Predictor = as_factor(Predictor)) %>%
  spread(Scale,Model) %>%
  mutate(Predictor = str_replace_all(Predictor,"_"," "))


## Model Predicted and Residual values
ModelPredRes <- HLMmods %>% unnest(Full %>% map(augment))
##################################
#### Model Checking Functions ####
#### Creates QQ, fitted vs residual, and fitted vs observed plots ####
## see ModelCheckPlots in the Plotting section
#### Variance Inflation Factor of single or multilevel model put into a dataframe ####
## car::vif has a method for merMod now, so this function is no longer needed
# vif.table <- function (fit, modelname, multilevel = FALSE, digs = 3) {
#   
#   if(multilevel==TRUE){
#     ## adapted from rms::vif
#     
#     v <- vcov(fit)
#     nam <- names(fixef(fit))
#     
#     ## exclude intercepts
#     ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
#     if (ns > 0) {
#       v <- v[-(1:ns), -(1:ns), drop = FALSE]
#       nam <- nam[-(1:ns)]
#     }
#     
#     d <- diag(v)^0.5
#     v <- diag(solve(v/(d %o% d)))
#     names(v) <- nam
#     v
#   } else{
#     v <- car::vif(fit)
#   }
#   
#   v <- v  %>% as.data.frame() %>%
#     tibble::rownames_to_column("Predictor") %>%
#     mutate_if(is.numeric, ~round(., digits = digs))
#   names(v) <- c("Predictor", modelname)
#   
#   return(v)
# }


#### Check for overdispersion in a poisson model ####
overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

##############################################################

######################################
#### CFA (i.e., lavaan) Functions ####
#### Results are a list where each element is a dataframe with paired item codes and label attribute (i.e. item stem) ####
ItemFrame <- function(ItemList,data,missing=TRUE){
  
  ScaleList <- list()
  for(s in names(ItemList)){
    
    ItemDF <- data.frame()
    for(i in ItemList[[s]]){
      
      if(missing==TRUE){
        
        miss <- prop.table(table(data[[i]],useNA = "always")) %>% as.data.frame() %>% filter(is.na(Var1))
        
        thedf <- data.frame(Item_Code = i,
                            Item = attr(data[[i]],"label"),
                            Levels = paste(levels(data[[i]]),collapse=", "),
                            Percent_Missing = paste0(round(miss[[2]]*100,1),"%"))
      } else{
        
        thedf <- data.frame(Item_Code = i,
                            Item = attr(data[[i]],"label"),
                            Levels = paste(levels(data[[i]]),collapse=", "))
      }
      
      ## add each item as a new row in the dataframe for the scale
      ItemDF <- suppressWarnings(bind_rows(ItemDF,thedf))
      
    }
    ## add each scale dataframe to the list
    ScaleList[[s]] <- ItemDF
  }
  return(ScaleList)
}




#### Gather fit statistics from a CFA or multiple CFAs that have been saved in a list into a single dataframe ####
Get_Fits <- function(list,fittype="scaled",pooled=FALSE,digs=3){
  #fittype accepts "naive","scaled","robust"
  #if pooled=TRUE then list must be the vector of fit statistics obtained from semTools::anova(model,test="D2",indices=TRUE,...)
  #Note: ... above could include pool.robust=TRUE,scale.W=FALSE
  
  if(fittype=="scaled"){
    indices <- c('chisq.scaled','df.scaled','pvalue.scaled','cfi.scaled', 'rmsea.scaled', 'srmr')
  } else if(fittype=="robust"){
    indices <- c('chisq.scaled','df.scaled','pvalue.scaled','cfi.robust', 'rmsea.robust', 'srmr')
  } else if(fittype=="naive"){
    indices <- c('chisq','df','pvalue','cfi', 'rmsea', 'srmr')
  }
  
  if(pooled==TRUE){
    indices[6] <- "srmr_bentler"
  }
  
  InternalFits <- function(mod,ind){
    fit <- as.data.frame(round(t(c(fitMeasures(mod,ind))),digs))
    names(fit) <- c("chisq","df","pvalue","CFI","RMSEA","SRMR")
    fit <- fit %>% mutate(n=inspect(mod,"nobs"),
                          X2 = paste0(chisq," (",round(df),")")) %>%
      select(n,X2,CFI,RMSEA,SRMR)
    
    if(!is.na(mod@loglik$loglik)){
      fit <- fit %>%
        mutate(AIC = round(AIC(mod),1),
               BIC = round(BIC(mod),1))
      
    }
    return(fit)
  }
  
  if(!is.list(list)){
    if(pooled==FALSE){
      
      AllFits <- InternalFits(list,indices)
      
    } else {
      
      AllFits <- list %>%
        as.data.frame() %>% rownames_to_column("Index") %>%
        filter(Index %in% indices) %>%
        mutate(Index = c("chisq","df","pvalue","CFI","RMSEA","SRMR"),
               Value = round(`.`,digs)) %>%
        select(Index,Value) %>% spread(Index,Value) %>%
        mutate(X2 = paste0(chisq," (",round(df),")")) %>%
        select(X2,CFI,RMSEA,SRMR)
    }
    
  } else {
    
    AllFits <- data.frame()
    for(i in names(list)){
      
      if(pooled==FALSE){
        
        fit <- InternalFits(list[[i]],indices) %>%
          mutate(Model=i) %>%
          select(Model,everything())
        
      } else {
        
        fit <- list[[i]] %>%
          as.data.frame() %>% rownames_to_column("Index") %>%
          filter(Index %in% indices) %>%
          mutate(Model = i,
                 Index = c("chisq","df","pvalue","CFI","RMSEA","SRMR"),
                 Value = round(`.`,digs)) %>%
          select(Model,Index,Value) %>%
          spread(Index,Value) %>%
          mutate(X2 = paste0(chisq," (",round(df),")")) %>%
          select(Model,X2,CFI,RMSEA,SRMR)
        
      }
      
      AllFits <- bind_rows(AllFits,fit)
    }
  }
  
  return(AllFits)
}



#### Compare Fit of two nested models ####
Delta_Fit <- function(mod1,mod2,fittype="scaled",modnames=NULL,digits=2){
  #fittype accepts "naive","scaled","robust"
  
  #if model names are not provided, this creates them from the model objects
  if(is.null(modnames)){
    modnames <- c(quo_name(enquo(mod1)),quo_name(enquo(mod2)))
  }
  
  # Defines whether the naive, scaled, or robust test and fit statistics are extracted
  if(fittype=="scaled"){
    indices <- c('cfi.scaled', 'rmsea.scaled', 'srmr')
  } else if(fittype=="robust"){
    indices <- c('cfi.robust', 'rmsea.robust', 'srmr')
  } else if(fittype=="naive"){
    indices <- c('cfi', 'rmsea', 'srmr')
  }
  
  # # Calculates naive chisquare difference
  # X2 <- round(fitmeasures(mod1,"chisq") - fitmeasures(mod2,"chisq"),digits)
  # # saves degrees of freedom
  # d0 <- fitmeasures(mod1,"df")
  # d1 <- fitmeasures(mod2,"df")
  # 
  # # Calculates the Santorra-Bentler (2001) scaled chisquare difference test statistic
  # if(fittype %in% c("scaled","robust")){
  #   c0 <- fitmeasures(mod1,"chisq.scaling.factor")
  #   c1 <- fitmeasures(mod2,"chisq.scaling.factor")
  #   cd <- (d0*c0 - d1*c1)/(d0 - d1)
  #   X2 <- round(X2/cd,digits)
  #   # adjX2 <- round((fitmeasures(mod1,"chisq")*c0 - fitmeasures(mod2,"chisq")*c1)/cd,digits)
  #   # The adjX2 version is from https://www.statmodel.com/chidiff.shtml, but I'm not sure why they
  #   # multiply by the scaling factors as this doesn't seem to be the case in the original paper, nor lavaan's source code,
  #   # which is what is currently used to calculate X2
  # }
  
  lavtable <- lavaan::anova(mod1,mod2,method="satorra.bentler.2001")
  
  CompTable <- data.frame(Model1 = modnames[[1]],
                          Model2 = modnames[[2]],
                          # Delta_adjX2 = adjX2,
                          Delta_X2 = paste0(round(lavtable$`Chisq diff`[[2]],1)," (",round(lavtable$`Df diff`[[2]]),")"),         #X2,
                          # Delta_df = round(lavtable$`Df diff`[[2]]),               #d0 - d1,
                          Delta_pvalue = round(lavtable$`Pr(>Chisq)`[[2]],digits), #round(pchisq(X2,(d0 - d1),lower.tail = FALSE),5),
                          Delta_CFI = round(fitmeasures(mod1,indices[[1]]) - fitmeasures(mod2,indices[[1]]),digits),
                          Delta_RMSEA = round(fitmeasures(mod1,indices[[2]]) - fitmeasures(mod2,indices[[2]]),digits),
                          Delta_SRMR = round(fitmeasures(mod1,indices[[3]]) - fitmeasures(mod2,indices[[3]]),digits))
  
  if(!is.na(mod1@loglik$loglik)){
    CompTable <- CompTable %>%
      mutate(Delta_AIC = round(AIC(mod1) - AIC(mod2),1),
             Delta_BIC = round(BIC(mod1) - BIC(mod2),1))
  }
  
  rownames(CompTable) <- NULL
  
  return(CompTable)
}
#### Get alpha, 3 versions of omega, and average variance extracted for the scale for each factor and total ####
# Requires semTools, tibble, and dplyr
Get_ScaleReliabilities <- function(model,multigroup=FALSE,digits=3){
  
  modrel <- suppressWarnings(semTools::reliability(model))
  
  if(multigroup==TRUE){
    
    reli <- data.frame()
    for(i in names(modrel)){
      
      interim <- modrel[[i]] %>%
        data.frame() %>% tibble::rownames_to_column(var="Coefficient") %>%
        rename(Total=total) %>% mutate_if(is.numeric,round,digits=digits) %>%
        mutate(Coefficient=c("alpha","conditional omega","unconditional omega",
                             "hierarchical omega","avg. var. extracted"),
               Group=i)
      reli <- bind_rows(reli,interim)
    } 
  } else {
    
    reli <- modrel %>%
      data.frame() %>% tibble::rownames_to_column(var="Coefficient") %>%
      rename(Total=total) %>% mutate_if(is.numeric,round,digits=digits) %>%
      mutate(Coefficient=c("alpha","conditional omega","unconditional omega",
                           "hierarchical omega","avg. var. extracted"))
    
  }
  return(reli)
}



#### Gather reliabilities from multiple scales into a single dataframe ####
## Requires loading Get_ScaleReliabilities
## Only compares Total reliability, but not separate scales
## If subscales are desired, instead of Get_Relis, use purrr::map_dfr(MMun,~Get_ScaleReliabilities(.,multigroup=TRUE),.id="name")
Get_Relis <- function(list,multigroup=FALSE,digs=3){
  
  AllRelis <- data.frame()
  
  if(multigroup==TRUE){
    
    newlist <- list %>% purrr::map(.,~Get_ScaleReliabilities(.,multigroup=TRUE,digits=digs))
    
    for(i in names(newlist)){
      
      reli <- newlist[[i]] %>% 
        select(Group,Coefficient,Total) %>%
        spread(Coefficient,Total) %>% mutate(Model=i) %>%
        select(Model,Group,alpha,`conditional omega`,`unconditional omega`,
               `hierarchical omega`,`avg. var. extracted`) 
      
      AllRelis <- bind_rows(AllRelis,reli)
      
    }
  } else {
    
    for(i in names(list)){
      
      reli <- list[[i]] %>% Get_ScaleReliabilities(digits=digs) %>% 
        select(Coefficient,Total) %>%
        spread(Coefficient,Total) %>% mutate(Model=i) %>%
        select(Model,alpha,`conditional omega`,`unconditional omega`,
               `hierarchical omega`,`avg. var. extracted`)
      
      AllRelis <- bind_rows(AllRelis,reli)
      
    }    
  }
  
  return(AllRelis)
  
}

#### Simplified Fit statistics and reliability ####
## Getting Reliabilities
TotalRelis <- function(model, digits=3){
  
  modrel <- suppressWarnings(semTools::reliability(model))
  
  reli <- modrel %>%
    data.frame() %>% tibble::rownames_to_column(var = "Coefficient") %>%
    select(Coefficient, Total = total) %>%
    mutate_if(is.numeric, round, digits = digits) %>%
    spread(Coefficient, Total)
  
  return(reli)
}

## Fit and reliability
# If multiple models are need, especially if saved in a list, use purrr::map_dfr(LavFitList, ~Get_FitRels(.x), .id = "Model")
Get_FitRels <- function(cfamod, fittype = "scaled", digs = 2, relis = TRUE){
  
  if(fittype=="scaled"){
    indices <- c('chisq.scaled','df.scaled','pvalue.scaled','cfi.scaled', 'rmsea.scaled', 'srmr')
  } else if(fittype=="robust"){
    indices <- c('chisq.scaled','df.scaled','pvalue.scaled','cfi.robust', 'rmsea.robust', 'srmr')
  } else if(fittype=="naive"){
    indices <- c('chisq','df','pvalue','cfi', 'rmsea', 'srmr')
  }
  
  fit <- as.data.frame(round(t(c(fitMeasures(cfamod,indices))),digs))
  names(fit) <- c("chisq","df","pvalue","CFI","RMSEA","SRMR")
  fit <- fit %>% mutate(n = inspect(cfamod,"ntotal"),
                        X2 = paste0(chisq," (",round(df),")")) %>%
    select(n,X2,pvalue,CFI,RMSEA,SRMR)
  
  if(!is.na(cfamod@loglik$loglik)){
    
    fit <- fit %>%
      mutate(AIC = round(AIC(cfamod),1),
             BIC = round(BIC(cfamod),1))
    
  }
  
  if(relis == TRUE) {
    
  fit <- fit %>%
    bind_cols(TotalRelis(cfamod, digits = digs))
  
  }
  
  
  return(fit)
}

#### Print detailed results of a CFA or SEM model ####
PrintSEM <- function(model,reliabilities=FALSE,fittype="scaled",r2=FALSE,items=NULL){
  ## May require Get_ScaleReliabilities and Get_Fits functions to be loaded.
  # Dataframes can be supplied to reliabilities, fittype (for fit indices), r2, and items arguments.
  # For lavaan.mi models, supplying dataframes to reliabilities and fittype is the only option.
  
  if(class(model)=="lavaan"){
    
    ## Reliabilities
    if(reliabilities==TRUE){
      reli <- Get_ScaleReliabilities(model,digits=3)
    } else if(reliabilities!=FALSE){
      reli <- reliabilities
    }
    
    ## R2
    if(r2==TRUE){
      rsq <- inspect(model,"r2") %>% data.frame() %>%
        tibble::rownames_to_column(var="Item") %>%
        rename(R2 = `.`) %>% mutate(R2 = round(R2,3))
    } else if(r2!=FALSE){
      rsq <- r2 
    }
    
    ## Fit indices
    fitin <- Get_Fits(model,fittype=fittype,pooled=FALSE)
    
    ## Model Parameters
    params.std <- standardizedSolution(model)
    params <- parameterestimates(model)
    
    ## Factor Loadings
    lv.std <- params.std %>% filter(op=="=~") %>% select(Factor=lhs,Item_Code=rhs,Std.Est=est.std)
    lv <- params %>% filter(op=="=~") %>%
      select(Factor=lhs,Item_Code=rhs,Estimate=est,SE=se,z,pvalue) %>%
      left_join(lv.std,by=c("Factor","Item_Code"))
    
    ## Variances and Covariances
    vari <- params %>% filter(op=="~~") %>%
      select(ItemOrFactor1=lhs,ItemOrFactor2=rhs,Estimate=est,SE=se,z,pvalue)
    
    ## Regressions
    reg.std <- params.std %>% filter(op=="~") %>% select(Outcome=lhs,Predictor=rhs,Std.Est=est.std)
    reg <- params %>% filter(op=="~") %>%
      select(Outcome=lhs,Predictor=rhs,Estimate=est,SE=se,z,pvalue) %>%
      left_join(reg.std,by=c("Outcome","Predictor"))
    
  } else if(class(model)=="lavaan.mi"){
    
    ## Reliabilities
    if(reliabilities!=FALSE){
      reli <- reliabilities
    }
    
    ## Fit indices - must have previously run Get_Fits and provided results in fittype argument
    fitin <- fittype
    
    ## Model Parameters
    Allparams <- summary(model,standardized=TRUE,add.attributes=FALSE,rsquare=TRUE)
    
    ## Factor Loadings
    lv <- Allparams %>% filter(op=="=~") %>%
      select(Factor=lhs,Item_Code=rhs,Estimate=est,SE=se,t,pvalue,Std.Est=std.all)
    
    ## Variances and Covariances
    vari <- Allparams %>% filter(op=="~~") %>%
      select(ItemOrFactor1=lhs,ItemOrFactor2=rhs,Estimate=est,SE=se,Std.Est=std.all)
    
    ## Regressions
    reg <- Allparams %>% filter(op=="~") %>%
      select(Outcome=lhs,Predictor=rhs,Estimate=est,SE=se,t,pvalue,Std.Est=std.all)
    
    ## R2
    if(r2==TRUE){
      rsq <- Allparams %>% filter(op=="r2") %>%
        select(Outcome=lhs,R2=est)
    } else if(r2!=FALSE){
      rsq <- r2 
    }
    
  } else {
    stop("Can't do that yet")
  }
  
  
  ## Table formatting
  panderOptions('knitr.auto.asis', FALSE)
  panderOptions('keep.trailing.zeros', TRUE)
  panderOptions('round', 3)
  panderOptions('table.split.table',Inf)
  
  ## Printing
  cat("\n\n")
  if(!is.null(items)){
    cat("#### Items\n\n")
    pander(items)
    cat("\n\n")
  }
  if(reliabilities!=FALSE){
    cat("#### Scale Reliabilities\n\n")
    pander(reli)
    cat("\n\n")
  }
  cat("#### Fit Measures\n\n")
  pander(fitin)
  cat("\n\n")
  cat("#### Factor Loadings\n\n")
  pander(lv)
  cat("\n\n")
  cat("#### Variances\n\n")
  pander(vari)
  cat("\n\n")
  if(nrow(reg)>0){
    cat("#### Regression\n\n")
    pander(reg)
    cat("\n\n")
  }
  if(r2!=FALSE){
    cat("R-squared\n\n")
    pander(rsq)
    cat("\n\n")
  }
}

#### Shortcut for extracting parameters from lavaan object ####
## To get standard errors for standardized parameters, specify std = c("std.all","std.lv")
## can include multiple parameters using c()
ExtractParams <- function(object, parameter, name, include.groups=FALSE, std = FALSE, r2 = TRUE, digits=2){
  
  name <- quo_name(enquo(name))
  
  if(is.character(std) == FALSE){
    
    TheParams <- parameterEstimates(object, standardized = std, rsquare = r2) %>%
      filter(op %in% parameter) %>%
      rename(!!name:=est)
    
  } else {
    
    TheParams <- standardizedSolution(object, type = std) %>%
      filter(op %in% parameter) %>%
      rename(!!name:=est.std)
  }
  
  ## Need to double check if the section below functions as intended
  if(include.groups==TRUE){
    
    TheParams <- TheParams %>%
      select(Outcome=lhs,Predictor=rhs,group,!!name, everything()) %>% 
      mutate_if(is.numeric,~round(.,digits)) %>% unique()
    
  } else {
    
    TheParams <- TheParams %>%
      select(Outcome = lhs,Predictor = rhs, !!name, everything()) %>% 
      mutate_if(is.numeric,~round(.,digits)) %>% unique()
  }
  return(TheParams)
}

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

#### Convert output from lavInspect to a dataframe ####
## currently only works for a multigroup analysis
Inspect2Df <- function(model,thewhat,multigroup=TRUE){
  
  if(multigroup==TRUE){
    
    TheDf <- lavInspect(model,thewhat) %>% tibble::enframe("Group","Value") %>% 
      mutate(Variable = purrr::map(Value,~attr(.,"names"))) %>% unnest()
    
  } else{
    
    
  }
  
  return(TheDf)
} 

######################################

####################################################
#### Latent Class Analysis with poLCA Functions ####

#### Calculate Entropy ####
## relative entropy similar to MPlus
lca.entropy <- function(lc){
  ##Numerator:
  nume.E <- -sum(lc$posterior * log(lc$posterior))
  ##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
  deno.E <- lc$Nobs*log(length(lc$P))
  ##Relative Entropy
  Entro <- 1-(nume.E/deno.E)
  return(Entro)
}

r2entropy <- function(lc){
  
  entropy<-function (p) sum(-p*log(p))
  error_prior <- entropy(lc$P) # Class proportions
  error_post <- mean(apply(lc$posterior, 1, entropy))
  R2_entropy <- (error_prior - error_post) / error_prior
  return(R2_entropy)
}

#### Compare models ####
lca.comp <- function(...){
  
  mods <- list(...)
  
  fits <- map_df(mods,~mutate(glance(.),Classes=length(.$P),N=.$N,Nobs=.$Nobs,num_LL=length(unique(round(.$attempts,3))),
                              Entropy=poLCA.entropy(.),Relative_Entropy=lca.entropy(.),R2_Entropy=r2entropy(.))) %>%
    select(Classes,N,Nobs,num_LL,everything())
  
  return(fits)
}

####################################################


#####################################################
#### Item Response Theory (i.e., mirt) Functions ####
## Have only been tested on Rasch model thus far
#### Gather Item statistics from mirt object ####
Rasch_Item_Stats <- function(mirtobject,thresholdsOnly=FALSE,reliability=FALSE,modname=NULL){
  
  Itemdf <- coef(mirtobject,printSE=TRUE,IRTpars=TRUE,as.data.frame=TRUE) %>%
    as.data.frame() %>% rownames_to_column("Threshold") %>%
    filter(!(str_detect(Threshold,"\\.([aug])") | str_detect(Threshold,"GroupPars.")))
  
  if(reliability==TRUE){
    
    # adapted from https://stats.stackexchange.com/questions/132738/reliability-in-irt-style
    # This conversation is also worth noting: https://groups.google.com/forum/#!topic/mirt-package/kfTOKXEdBls
    
    ## equivalent to empirical reliability
    e <- mean(Itemdf$SE^2)
    s <- var(Itemdf$par)
    itemreli <- 1-(e/(s+e))
    
    if(!is.null(modname)){
      attr(itemreli,"Model") <- modname
    }
    
    return(itemreli) # returns a single value
    
  } else if(thresholdsOnly==TRUE){
    
    return(Itemdf)
    
  } else {
    
    itemloc <- Itemdf %>% separate(Threshold,c("Item","Parameter"),"\\.") %>%
      group_by(Item) %>%
      summarize(Location = mean(par),
                SE = sqrt(mean(SE^2))) %>% # Not sure I've done this right
      inner_join(itemfit(mirtobject,fit_stats="infit"),by=c("Item"="item"))
    
    return(itemloc) # returns a dataframe with location, outfit, and infit for all items
    
  }
}


#### Gather person statistics from mirt object ####
Rasch_Person_Stats <- function(mirtobject){
  
  Persondf <- fscores(mirtobject,full.scores.SE = TRUE) %>% as.data.frame() %>%
    bind_cols(personfit(mirtobject))
}

#### Summarize scale fit from person and item statistics ####
## Get_Item_Stats and/or Get_Person_Stats are prerequisites
Summary_Rasch_Fit <- function(persondf=NULL,itemdf=NULL,modelname=NULL,group=NULL){
  
  grps <- enquo(group)
  
  if(!is.null(persondf) & !is.null(itemdf)){
    
    ## Calculating mean item outfit and infit
    idf <- itemdf %>% gather(Variable,Value,outfit,infit) %>%
      Get_Descriptives(Value,Variable) %>% mutate(Type="Item")
    ## Calculating mean person outfit and infit and binding item stats
    pdf <- persondf %>% gather(Variable,Value,outfit,infit) %>%
      Get_Descriptives(Value,Variable) %>% mutate(Type="Person") %>%
      bind_rows(idf) %>% select(Type,everything())
    
  } else {
    pdf <-  bind_rows(persondf,itemdf) %>% gather(Variable,Value,outfit,infit) %>%
      Get_Descriptives(Value,Variable,!!grps)
  }
  
  if(!is.null(modelname)){
    pdf <- pdf %>% mutate(Model=modelname) %>%
      select(Model,everything())
  }
  
  return(pdf)
}


#####################################################


####################################################
#### Multiple Imputation (i.e., mice) Functions ####
#### (Semi-)Automatic way of determining the method of imputation for each variable ####
## Exports a dataframe with columns for Variable, Class, # of Levels, # Missing, and MI method to use
Get_MIMethod <- function(data){
  
MIMethod <- data.frame(Class = purrr:::map_chr(dat,class),
                       Levels= purrr:::map_int(data,~length(levels(.))),
                       Number_Missing = purrr:::map_int(data,~sum(is.na(.)))) %>%
  tibble:::rownames_to_column("Variable") %>%
  mutate(Method = ifelse(Number_Missing==0,"",
                         ifelse(Class=="numeric","norm",
                                ifelse(Class=="factor"&Levels==2,"logreg",
                                       ifelse(Class=="factor"&Levels>2,"polr","")))))
return(MIMethod)
}

#### Gather pooled information - Estimates, R-squared, AIC,and Wald test comparing models ####
# Note: The degrees of freedom in the Wald test contain decimals because it uses the Welch-Satterthwaite approximation
# in order to account for unequal between-imputation variances
Get_Pooled <- function(MIdata,MIdata0=NULL,digits=10,adjR2=FALSE,modelname=deparse(substitute(MIdata))){
  
  ## Gathering pooled regression estimates
  estimates <- pool(MIdata) %>% summary() %>%
    round(digits=digits) %>% as.data.frame() %>%
    rownames_to_column("Variable") %>%
    mutate(Model = modelname) %>%
    select(Model,everything())
  
  ## Gathering R2 estimates
  r2 <- pool.r.squared(MIdata,adjusted=FALSE) %>%
    round(digits=digits) %>% as.data.frame() %>%
    rownames_to_column("Type") %>%
    mutate(Model = modelname) %>%
    select(Model,everything())
  
  if(adjR2==TRUE){
    
    r2adj <- pool.r.squared(MIdata,adjusted=TRUE) %>%
      round(digits=digits) %>% as.data.frame() %>%
      rownames_to_column("Type") %>%
      mutate(Model = modelname) %>%
      select(Model,everything())
    
    r2 <- bind_rows(r2,r2adj)
  }
  
  ## AIC
  allAIC <- data.frame(AIC=map_dbl(MIdata$analyses,AIC)) %>%
    Get_Descriptives(AIC) %>%
    mutate(lo95 = Mean - (SE*1.96),
           hi95 = Mean + (SE*1.96)) %>%
    round(digits=digits) %>%
    mutate(Model = modelname) %>%
    select(Model,everything())
  
  
  ## Comparing models (if selected)
  if(!is.null(MIdata0)){
    comp <- pool.compare(MIdata,MIdata0)
    
    compdf <- data.frame(WaldStatistic = comp$Dm[[1]],
                         NonresponseRelVarInc = comp$rm,
                         Df1 = comp$df1,
                         Df2 = comp$df2,
                         Pvalue = comp$pvalue[[1]]) %>%
      round(digits=digits) %>%
      mutate(Model = modelname) %>%
      select(Model,everything())
    
    Pooled <- list(FixedEffects = estimates,
                   R2 = r2,
                   AIC = allAIC,
                   Comparison = compdf)
  } else{
    
    Pooled <- list(FixedEffects = estimates,
                   R2 = r2,
                   AIC = allAIC)
  }
  return(Pooled)
}


#### Extracting fitted and residual values from each imputation and creating a pooled summary ####
Get_Fitted_MI <- function(MIModel){
  
  AllMdf <- data.frame()
  
  for(i in 1:length(MIModel$analyses)){
    
    OFRdf <- data.frame(.imp = as.character(i),
                        Observed = MIModel$analyses[[i]]$model[[1]],
                        Fitted = MIModel$analyses[[i]]$fitted.values,
                        Residuals = MIModel$analyses[[i]]$residuals) %>%
      rownames_to_column(".id") %>%
      mutate(`.id` = as.character(`.id`))
    
    AllMdf <- bind_rows(AllMdf,OFRdf)
  }
  
  PooledMdf <- AllMdf %>% group_by(`.id`) %>%
    summarize(Observed = mean(Observed),
              Fitted = mean(Fitted),
              Residuals = mean(sqrt(Residuals^2)))
  
  
  FitDF <- list(All = AllMdf,
                Pooled = PooledMdf)
  
  return(FitDF)
}

#### Calculating correlation or covariance for each imputed dataset ####
## Requires using mice::complete to first create a list of imputed datasets
MI_CorCov <- function(MIlist,corcov=c("cor","cov","polychor"),
                      usena="pairwise.complete.obs",meth="pearson"){
  allimps <- data.frame()
  for(i in 1:length(MIlist)){
    if(corcov=="polychor"){
      allcorrs <- psych::polychoric(MIlist[[i]])
      rho <- allcorrs$rho
    } else if(corcov=="cov"){
      rho <- cov(MIlist[[i]],use=usena,method=meth)
    } else if(corcov=="cor"){
      rho <- cor(MIlist[[i]],use=usena,method=meth)
    }
    
    rho[upper.tri(rho,diag=TRUE)] <- NA
    rholong <- as.data.frame(rho) %>% tibble::rownames_to_column("Var1") %>%
      gather(Var2,Cor,-Var1) %>% filter(!is.na(Cor)) %>%
      mutate(Imp=i)
    allimps <- bind_rows(allimps,rholong)
  }
  return(allimps)
}

#### Converting md.pattern output to dataframe ####
mdpatternToDF <- function(data,plot=FALSE){
  
  MissingDF <- mice::md.pattern(data,plot=plot) %>% as.data.frame() %>%
    tibble::rownames_to_column(.,var="Counts") %>%
    mutate(Counts=stringr::str_remove(Counts,"X"),
           Counts=stringr::str_remove(Counts,"\\..*"),
           Counts=as.numeric(Counts))
  names(MissingDF)[[length(MissingDF)]] <- "Total_Missing"
  
  ## Percent of all cells and all cases with missing
  PercentCellsMissing <- MissingDF[is.na(MissingDF$Counts),"Total_Missing"][[1]]/(nrow(data)*(ncol(data)))
  PercentCasesMissing <- (nrow(data)-(MissingDF[MissingDF$Total_Missing==0,"Counts"][[1]]))/nrow(data)
  # Adding as attributes of dataframe
  attr(MissingDF,"Percent_Cells_Missing") <- PercentCellsMissing
  attr(MissingDF,"Percent_Cases_Missing") <- PercentCasesMissing
  
  return(MissingDF)
  
}


#######################################

#########################################
#### Functions related to attributes ####
## saves all attributes to a list
atts <- map(data,~attributes(.))
#### Use mutate_at while maintaining attributes ####
KeepAttrMutateAt <- function(data,variables,functions,attribute){
  
  atts <- map_chr(.x=data[ ,variables],.f=function(x) attr(x,attribute))
  
  data <- data %>%
    mutate_at(vars(variables),funs_(functions))
  
  for(v in 1:length(variables)){
    
    attr(data[[variables[[v]]]],attribute) <- atts[[v]]
  }
  
  return(data)
}

#### Extracting labels from a factor and/or character or labelled variable ####
## labelled returns both the label and the value, otherwise only the label is returned
Get_Labels <- function(data,type=c("factor","character","labelled","factor_character")){
  
  if(type=="labelled"){
    
    ## Select labelled variables, extract labels, and transform to long format dataframe
    all_labels <- data %>% select_if(is.labelled) %>%
      purrr::map(~attr(.,"labels")) %>% tibble::enframe("Item","Value") %>% 
      mutate(Label = purrr::map(Value,~attr(.,"names"))) %>% unnest(cols = c(Value, Label))
    
  } else {
    
    ## Select variables
    if(type=="factor"){
      data <- data %>% select_if(is.factor)
    } else if(type=="character"){
      data <- data %>% select_if(is.character) %>%
        mutate_if(is.character,forcats::as_factor)
    } else if(type=="factor_character"){
      data <- data %>% mutate_if(is.character,forcats::as_factor) %>%
        select_if(is.factor)
    }
    ## Extract labels and transform to long format dataframe
    all_labels <- data %>% select_if(is.factor) %>%
      purrr::map(~levels(.)) %>% tibble::enframe("Item","Label") %>%
      unnest()
    
  }
  
  return(all_labels)
}


#### Save label, remove label, and add label ####
## Save label
# list of label (Add %>% tibble::enframe() to convert to dataframe)
Alllabel <- purrr::map(data,~attr(.,"label"))
# Or named vector
Alllabel2 <- sjlabelled::get_label(hcls18clean)

## Remove
# zap_ already exists for formats, missing, labels, and widths in the haven package
zap_label <- function(x){
  
  attr(x, "label") <- NULL
  
  return(x)
}
# data <- data %>% mutate_all(zap_label)
# data <- data %>% mutate_all(funs(zap_label,zap_widths,zap_format)) # doesn't work for some reason; do separate calls to mutate_all

## Add back
# label is a list and data is a dataframe
# Currently only works if length(list)==ncol(data), but order doesn't matter
set_label <- function(data,label){
for(i in names(label)){
  attr(data[[i]],"label") <- label[[i]]
}
  return(data)
}

# label saved as named vector in same order as the columns in the dataframe
newdata<- data %>% sjlabelled::set_label(Alllabel2)

#### Creating and Exporting the Codebook ####
## Requires Get_Labels function from above; only tested thus far for type="labelled"
## if export_type = "none" & keep_R_Object = FALSE, the function does not output anything
Create_Codebook <- function(OrigData, export_type = c("excel","csv","none"), export_name = "MyData",
                            label_type = "labelled", keep_R_Object = TRUE){
  
  ## Extract Values and Labels
  ValueLabels <- OrigData %>% Get_Labels(type = label_type)
  
  ## Putting in wide format
  VLW <- ValueLabels %>% mutate(VL = paste(Value, Label, sep=" = ")) %>%
    group_by(Item_Name) %>% summarize(Value_Label = paste(VL, collapse="; ")) %>%
    ungroup()
  
  ## Extract Item Stem (i.e. label), then joining values and labels
  Stems <- purrr::map(OrigData, ~attr(., "label")) %>% tibble::enframe("Item_Name", "Item_Stem")
  
  ## Joining Stems, Values, and Labels (gets us 90% of the way to the final codebook)
  Codebook <- left_join(Stems, VLW, by = "Item_Name") %>%
    mutate(Item_Stem = as.character(Item_Stem))
  
  
  if(export_type == "excel"){
    
    openxlsx::write.xlsx(x = Codebook, file = paste0(export_name, ".xlsx"), asTable = FALSE)
    
    message("Codebook exported to file ", export_name, ".xlsx")
    
  } else if(export_type == "csv"){
    
    ## Exporting to csv file
    write.csv(Codebook, file = paste0(export_name, ".csv"), row.names = FALSE, na = "")
    
    message("Codebook exported to file ", export_name, ".csv")
    
  } else if(export_type != "none"){
    
    stop("Must specify 'excel', 'csv', or 'none' for export_type argument.")
    
  }
  
  if(keep_R_Object == TRUE){
    return(Codebook)
  }
  
}
################################


#########################################


############################################
#### Automated Interpretation Functions ####

#### Scale score difference effect size ####
EffectSize_Interpretation <- function(data){
  
  if(nrow(data)!=0){
    
    SomeHigh <- data %>% filter(Std_m_diff >= .2 & Std_m_diff < .4) %>% select(Respondent); SomeHigh <- as.character(SomeHigh$Respondent)
    ConHigh <- data %>% filter(Std_m_diff >= .4 & Std_m_diff < .7) %>% select(Respondent); ConHigh <- as.character(ConHigh$Respondent)
    ExtHigh <- data %>% filter(Std_m_diff >= .7) %>% select(Respondent); ExtHigh <- as.character(ExtHigh$Respondent)
    SomeLow <- data %>% filter(Std_m_diff > -.4 & Std_m_diff <= -.2) %>% select(Respondent); SomeLow <- as.character(SomeLow$Respondent)
    ConLow <- data %>% filter(Std_m_diff > -.7 & Std_m_diff <= -.4) %>% select(Respondent); ConLow <- as.character(ConLow$Respondent)
    ExtLow <- data %>% filter(Std_m_diff <= -.7) %>% select(Respondent); ExtLow <- as.character(ExtLow$Respondent)
    
    ES_Interp <- list(`extremely higher` = ExtHigh,
                      `considerably higher` = ConHigh,
                      `somewhat higher` = SomeHigh,
                      `somewhat lower` = SomeLow,
                      `considerably lower` = ConLow,
                      `extremely lower` = ExtLow)
  } else {
    ES_Interp <- NULL
  }
  
  return(ES_Interp)
  
}

#### Correlations ####
Corr_Interpretation <- function(corr){
  interp <- ifelse(corr < -.7, "a strong negative",
                   ifelse(corr >= -.7 & corr < -.5, "a moderately strong negative",
                          ifelse(corr >= -.5 & corr < -.3, "a moderately weak negative",
                                 ifelse(corr >= -.3 & corr < -.1, "a weak negative",
                                        ifelse(corr >= -.1 & corr < .1, "no",
                                               ifelse(corr >= .1 & corr < .3, "a weak positive",
                                                      ifelse(corr >= .3 & corr < .5, "a moderately weak positive",
                                                             ifelse(corr >= .5 & corr < .7, "a moderately strong positive",
                                                                    ifelse(corr >= .7,"a strong positive")))))))))
  
  mln <- ifelse(interp %in% c("a strong negative","a moderately strong negative","a moderately weak negative","a weak negative"),"the less",
                ifelse(interp %in% c("a strong positive","a moderately strong positive","a moderately weak positive","a weak positive"),"the more","has no relationship with how"))
  corrlist <- list(interp=interp,
                   mln=mln)
  
  return(corrlist)
}

#### Printing Interpretations ####
## Standard function
Print_Interpretation <- function(interplist){
  
  for(i in 1:length(interplist)){
    
    cat("\n\n")
    cat("- ",interplist[[i]])
    cat("\n\n")
    
  }
}

#### Printing Effect Size interpretation ####
Print_ESInterp <- function(ES_Interp,scl,comparison){
  
  if(length(ES_Interp[[1]])==0 & length(ES_Interp[[2]])==0 & length(ES_Interp[[3]])==0 &
     length(ES_Interp[[4]])==0 & length(ES_Interp[[5]])==0 & length(ES_Interp[[6]])==0){
    
    cat("\n\n")
    cat("- There are no meaningful differences in",scl,"from",comparison)
    cat("\n\n")
    
  } else {
    
    for(i in 1:length(names(ES_Interp))){
      
      len <- length(ES_Interp[[i]])
      
      if(len==1){
        
        cat("\n\n")
        cat("- On average, ",ES_Interp[[i]]," report ",names(ES_Interp[i])," ",scl," than ",comparison,sep="")
        cat("\n\n")
        
      } else if(len==2){
        
        cat("- On average, ",ES_Interp[[i]][1]," and ",ES_Interp[[i]][2]," report ",names(ES_Interp[i])," ",scl," than ",comparison,sep="")
        
      } else if(len==3){
        
        cat("\n\n")
        cat("- On average, ",ES_Interp[[i]][1],", ",ES_Interp[[i]][2],", and ",ES_Interp[[i]][3]," report ",names(ES_Interp[i])," ",scl," than ",comparison,sep="")
        cat("\n\n")
        
      } else if(len==4){
        
        cat("\n\n")
        cat("- On average, ",ES_Interp[[i]][1],", ",ES_Interp[[i]][2],", ",ES_Interp[[i]][3],", and ",ES_Interp[[i]][4]," report ",names(ES_Interp[i])," ",scl," than ",comparison,sep="")
        cat("\n\n")
        
      } else if(len==5){
        
        cat("\n\n")
        cat("- On average, ",ES_Interp[[i]][1],", ",ES_Interp[[i]][2],", ",ES_Interp[[i]][3],",  ",ES_Interp[[i]][4],", and ",ES_Interp[[i]][5]," report ",names(ES_Interp[i])," ",scl," than ",comparison,sep="")
        cat("\n\n")
      }
    }
  }
}
############################################
################################################################################
#### Effect Size related functions (not including automated interpretation) ####

#### Cohen's h, difference for proportions ####
Get_CohensH <- function(p1,p2){
  
  phi1 <- 2*asin(sqrt(p1))
  phi2 <- 2*asin(sqrt(p2))
  h <- phi1 - phi2
  return(h)
}


#### Calculate Cohen's D from two independent or paired groups ####
## SD can be pooled by providing sd1 and sd2
Get_CohensD <- function(m1, m2, sd1, sd2 = NULL, n1, n2, sample = "ind", proportion = FALSE){
  
  
  # raw mean difference
  md <- (m1 - m2)
  
  # Sigma for continuous variables
  if(proportion == FALSE){
    
    # Use only SD from group 1 or an overall SD
    if(is.null(sd2)){
      
      sigma <- sd1
      
    } else {
      
      # sigma for independent groups
      if(sample == "ind"){
        
        sigmanum <- (n1 - 1) * (sd1^2) + (n2 - 1) * (sd2^2)
        sigmadenom <- (n1 + n2 - 2)
        sigma <- sqrt(sigmanum / sigmadenom)
        
      } else{ 
        
        # sigma for paired groups
        sigma <- sqrt((sd1^2 + sd2^2) / 2)
        
      }
    }
    # Sigma for dichotomous variables NOTE: NEED TO REVIEW THESE FORMULAS
  } else {
    
    # Can provide overall proportion to sd2
    if(!is.null(sd2)){
      
      sigma <- sqrt(sd2 * (1 - sd2) / n1)
      
    } else if(sample == "ind"){
      
      # for unequal independent groups 
      sigma <- sqrt((m1 * (1 - m1) / n1) + (m2 * (1 - m2) / n2))
      
    } else {
      
      # for paired groups or groups of equal size
      sigma <- sqrt((m1 * (1 - m1) + m2 * (1 - m2)) / 2)
      
    }
  }
  
  # Calculating d
  d <- md / sigma
  
  return(d)
}

#### Variance for Cohen's D from above calculation ####
Get_DVar <- function(d, n1, n2){
  
  left <- (n1 + n2) / (n1 * n2)
  right <- (d^2) / (2* ((n1 + n2)))
  dvar <- left + right
  return(dvar)
  
}

#### Converts correlations, eta2, or log odds ratio to cohen's d ####
## For more eta calculations see http://www-01.ibm.com/support/docview.wss?uid=swg21476421
## Formulas from: https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf
ConvertToD <- function(x,type=c("r","eta2","logOR","t"),levels,n1,n2){
  if(type=="r"){
    d <- (2*x)/sqrt((1-x^2))
  } else if(type=="eta2"){
    d <- sqrt(x^2/(1-x^2))*sqrt(2*levels)
  } else if(type=="logOR"){
    d <- x*(sqrt(3)/pi)
  } else if(type=="t"){
    d <- (2*x)/sqrt(n1+n2-2)
  }
}

#### Calculates variance of Cohen's D when converted from other effect size ####
ConverToD_DVar <- function(x,type=c("r","eta2","logOR")){
  if(type=="r"){
    dv <- (4*x)/((1-x^2)^3)
  } else if(type=="eta2"){
    dv <- (4*x)/((1-x^2)^3)
  } else if(type=="logOR"){
    dv <- x*(3/pi^2)
  }
}

DtoG <- function(d,n1,n2){
  n2 <- ifelse(is.na(n2),0,n2)
  df <- n1+n2-2
  J <- 1 - (3/(4*df-1))
  g <- J*d
  return(g)
}

DvarToGvar <- function(dvar,n1,n2){
  n2 <- ifelse(is.na(n2),0,n2)
  df <- n1+n2-2
  J <- 1 - (3/(4*df-1))
  gvar <- J^2*dvar
  return(gvar)
}


#### Calculating Standardized Mean Difference ####
## Use Get_CohensD as the primary function
## Necessitates first calculating Mean_group and Mean_ref
# SMD <- function(data){
#   data <- data %>% mutate(Std_m_diff = ifelse(n_group > 14 & n_ref >14,round((Mean_group-Mean_ref)/sqrt(SD_group^2+SD_ref^2),3),NA)) %>%
#     filter(!is.na(Std_m_diff)) %>% ungroup() %>%
#     group_by(Scale) %>% mutate(Std_min = ifelse(min(Std_m_diff,na.rm=TRUE) < -.8,min(Std_m_diff,na.rm=TRUE),-.8),   # Getting maximum and minimum std mean differences
#                                Std_max = ifelse(max(Std_m_diff,na.rm=TRUE) > .8,max(Std_m_diff,na.rm=TRUE),.8)) %>% ungroup() # to be used for plotting
# }
################################################################################

##############################
#### Simulation functions ####
#### Function for 3PL model ####
threePL <- function(a,b,c,t) c + (1 - c) * (exp(-a*(t-b)) / (1 + exp(-a*(t-b))))
twoPL <- function(a,b,t) 1 / (1 + exp(-a*(t-b)))

###################################################
#################################
#### Uncategorized functions ####
#### Calculate Coefficient Alpha for a set of items ####
## Keeps only raw alpha
## can also select items outside of function instead of using ...
Get_Alpha <- function(data,...,scalename="Scale",digits=3){
  
  items <- quos(...)
  
  alphadf <- data %>% select(!!!items) %>% psych::alpha() %>%
    summary() %>% data.frame() %>% mutate(Scale=scalename) %>%
    select(Scale,raw_alpha) %>% mutate(raw_alpha = round(raw_alpha,digits=digits))
}

#### Classical Test Theory Summary from psych package ####
## Currently intended for correct/incorrect items
## Which information is needed seems to vary greatly by project
Get_CTT <- function(data,...,omega = FALSE){
  
  items <- quos(...)
  
  RunAlpha <- data %>% select(!!!items) %>% psych::alpha()
  
  if(omega = TRUE){
    RunOmega <- data %>% select(!!!items) %>% psych::omega()
  }
  
  
}

#### function for subsetting the data, running one sample t-test, and producing results in a table ####
rn_ttest <- function(data, type, group, variable,mu){
  
  reduced <- data[which(data[[type]]==group), ]
  
  group_ttest <- t.test(reduced[[variable]], mu=mu)
  
  results <- as.data.frame(cbind(group_ttest$estimate, group_ttest$statistic, group_ttest$p.value, t(group_ttest$conf.int)))
  colnames(results) <- c("Mean", "t_value", "p_value", "conf_int_low","conf_int_high")
  results$NumStories <- group
  results$Variable <- variable
  row.names(results) <- NULL
  
  return(results)
}


#### Extracts results from a MANOVA ####
## Includes Pillai's Trace and Box's M test
Get_MANOVA_Results <- function(mlm, digs = 2){
  
  ## Summarizing MANOVA results
  manova.summary <- summary(mlm)                           # saves the results for each domain
  Pillai <- summary(manova(mlm))$stats %>% as.data.frame() # saves manova statistics
  
  ## Extracts relevant results from each domain into a single dataframe
  Feta <- map_dfr(names(manova.summary), ~data.frame(DV = .x,
                                                     F_stat = manova.summary[[.x]]$fstatistic[[1]],
                                                     df = paste0("(",manova.summary[[.x]]$fstatistic[[2]],", ",manova.summary[[.x]]$fstatistic[[3]],")"),
                                                     p = pf(manova.summary[[.x]]$fstatistic[[1]], manova.summary[[.x]]$fstatistic[[2]], manova.summary[[.x]]$fstatistic[[3]], lower.tail = FALSE),
                                                     eta_square = manova.summary[[.x]]$r.squared))
  
  
  ## Extracts Pillai's Trace into a dataframe
  PillaiDF <- data.frame(DV = "Pillai",
                         F_stat = Pillai$`approx F`[[1]],
                         df = paste0("(", Pillai$`num Df`[[1]], ", ", Pillai$`den Df`[[1]], ")"),
                         p = Pillai$`Pr(>F)`[[1]],
                         eta_square = heplots::etasq(mlm)[[1]],
                         Pillai = Pillai$Pillai[[1]])
  #https://stats.stackexchange.com/questions/240751/why-is-pillais-trace-equal-to-partial-eta-squared-in-a-repeated-measures-manova
  
  
  ## Runs Box's M test for homogeneity of covariances across the domains
  Boxes <- heplots::boxM(mlm)  # function also calculates mean values for each factor level across each DV if needed; Boxes$Means
  BoxesDF <- data.frame(Statistic = Boxes$statistic[[1]],
                        df = Boxes$parameter[[1]],
                        p = Boxes$p.value[[1]]) 
  
  ## Creating output
  manovaResults <- list(Table = bind_rows(Feta, PillaiDF) %>%
                          mutate_if(is.numeric,~round(., digs)), # Combines domain specific and Pillai's Trace information
                        BoxM = BoxesDF %>% mutate_if(is.numeric,~round(., digs)))          
  #https://www.statisticshowto.datasciencecentral.com/boxs-m-test/
  
  return(manovaResults)
}


#### Combining Response options columns so there is one column for each item ####
Sheet_17 <- Sheet_17_combo %>% mutate(Q1 = coalesce(X10,X11,X12,X13,X14),
                                      Q2 = coalesce(X16,X17,X18,X19))







#### Calculate percentiles ####
## for the columns specified in ... and add percentiles to the original data frame
Add_Percentiles <- function(data, ...){
  
  ## gather all variables into a vector and create blank dataframe to populate
  columns <- c(...)
  new_data <- data
    
    for(c in columns){
      
      ## Remove any NAs from the colum of interest
      csym <- sym(c)
      c_data <- new_data %>% filter(!is.na(UQ(csym)))
      
      ## get percentiles
      ptiles <- quantile(c_data[[c]],na.rm=TRUE,type=8,probs=seq(.01,.99,by=.01))
      
      ## Find percentile for each State_ID for column of interest; rename resulting column to be unique
      c_data <- c_data %>% select(State_ID,UQ(csym)) %>%
        mutate(Percentile = findInterval(UQ(csym),ptiles))
      names(c_data)[names(c_data)=="Percentile"] <- paste0(c,"_Ptile")
      
      ## Join student percentile column ("Ptile") to existing dataframe by State_ID and the column of interest
      new_data <- new_data %>% left_join(c_data,by=c("State_ID",c))

  }
  
  return(new_data)
}


#### Create sheet in Excel file ####
## Creating the Excel workbook is a prerequisite and saving is a postrequisite
## workbook <- openxlsx::createWorkbook() # various arguments can be added if needed
## openxlsx::saveWorkbook(workbook, file = "example.xlsx",overwrite = TRUE)
MakeExcelSheet <- function(workbook,data,sheet){
  openxlsx::addWorksheet(workbook,sheetName = sheet) # other asthetic arugments available
  openxlsx::writeDataTable(workbook, sheet, data, rowNames = FALSE, colNames = TRUE,
                           tableStyle = "none", withFilter = FALSE, keepNA = FALSE)
}


#### Running ROC analysis ####
ROCR <- function(data,season,achlev,measure,measure.x="cutoff"){
  
  new_data <- data %>% filter(Season==season)
  
  SeasonALpred <- prediction(new_data$Score,new_data[[achlev]])
  SeasonALperf <- performance(SeasonALpred,measure,measure.x)
  
  x <- SeasonALperf@x.name
  y <- SeasonALperf@y.name
  
  if(SeasonALperf@alpha.name=="none"){
    rocdata <- data.frame(sea = season,
                          al = achlev,
                          xn = SeasonALperf@x.values[[1]],
                          yn = SeasonALperf@y.values[[1]])
    
    names(rocdata) <- c("Season","AchievementLevel",x,y)
    
  } else {
    
    alpha <- SeasonALperf@alpha.name
    
    rocdata <- data.frame(sea = season,
                          al = achlev,
                          xn=SeasonALperf@x.values[[1]],
                          yn=SeasonALperf@y.values[[1]],
                          alphan=SeasonALperf@alpha.values[[1]])
    names(rocdata) <- c("Season","AchievementLevel",x,y,alpha)
  }
  
  return(rocdata)
  
}

#### Converting logits to probabilities and vice versa ####
## same as stats::plogis
LogitToProb <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

## Converting Probability to Logit
## same as stats::qlogis
ProbToLogit <- function(prob){
  odds <- prob / (1 - prob)
  logit <- log(odds)
  return(logit)
}

#### Run logistic regression and extract cutoff scores for low, medium, and high risk ####
Get_RiskCutoffs <- function(data,season){
  
  new_data <- data %>% filter(Season==season)
  
  glm.mod <- glm(Meets ~ Score, data=new_data,family = "binomial")
  
  new_data <- new_data %>% mutate(Predicted_Prob=LogitToProb(predict(glm.mod)))
  
  LR <- min(new_data[new_data$Predicted_Prob>=.75,]$Score)
  MR <- min(new_data[new_data$Predicted_Prob>=.5,]$Score)
  HR <- min(new_data[new_data$Predicted_Prob>=.25,]$Score)
  
  new_data <- new_data %>% mutate(Low_Risk = LR,
                                  Moderate_Risk = MR,
                                  High_Risk = HR)
  return(new_data)
}



#### Filtering Function ####
FilterIt <- function(data,filter_var=NULL,filtcrit=NULL,not=FALSE){
  
  if(!is.null(filter_var) & !is.null(filtcrit)){
    
    filtervar <- sym(filter_var)
    
    if(not==TRUE){
      data <- data %>% filter(!(UQ(filtervar) %in% filtcrit))
    } else{
      data <- data %>% filter(UQ(filtervar) %in% filtcrit)
    }
    
  } else(data <- data)
  
  return(data)
}


#### Calculates Kory's Diversity Index ####
ShannonIndex <- function(x, base = exp(1), ...){
  
  if(class(x) != "table"){
    
    x <- table(x, ...)
  }
  
  prop <- as.matrix(prop.table(x))
  H <- -sum(prop * log(prop, base = base))
  
  return(H)
  
}

#### Minimum positive value ####
## For Example: Function for selecting Instructional setting date closest to, yet prior to, test date,
##  but if no instructional setting date exists prior to test date, selecting the date closest to test date.
minpositive <- function(x) ifelse(min(x[x > 0])==Inf,max(x),min(x[x > 0]))

#### Group and Grand Mean Centering variables ####
# for character vectors in ..., can use !!!syms(l1vars)
CGMCWC <- function(Data, CentType = c("CGM", "CWC"), ..., GroupVar, ReturnGroupMean = TRUE){
  
  # Creates quosures for use in dplyr functions
  variables <- quos(...)
  
  ## Centering at the Grand Mean
  if(CentType == "CGM"){
    
    # CGM each variable provided in ... and adds suffix _grand
    CentData <- Data %>%
      mutate_at(vars(!!!variables), list(grand = ~(. - mean(., na.rm = TRUE))))
    
    ## Centering Within Cluster (i.e., group mean centering)
  } else if(CentType=="CWC"){
    
    if(missing(GroupVar)){
      
      stop("GroupVar required for CWC (i.e., group mean centering)")
      
    } else {
      
      # Creates quosure for use in dplyr functions
      groupvar <- enquo(GroupVar)
    }
    
    # CWC each variable provided in ... and adds suffix _cent
    CentData <- Data %>% group_by(!!groupvar) %>%
      mutate_at(vars(!!!variables), list(cent = ~(. - mean(., na.rm = TRUE)))) %>%
      ungroup()
    
    if(ReturnGroupMean == TRUE){
      
      # Calculates group mean for each variable provided in ...
      CentData <- CentData %>% group_by(!!groupvar) %>%
        mutate_at(vars(!!!variables), list(gmean = ~mean(., na.rm = TRUE))) %>%
        ungroup()
    }
    
  } else {
    stop("Valid entries for CentType are 'CGM' for centering at the grand mean or 'CWC' for centering within cluster (i.e., group mean centering).")
  }
  
  return(CentData)
}
#### Total Occurances Summed from Multiple Columns ####
Scale_Score <- function(df,items,type = c("sum","count","mean"),NAs = TRUE){
  
  sum <- rowSums(df[items],na.rm = NAs)
  
  if(type=="sum"){
    return(sum)
  }
  
  count <- rowSums(!is.na(df[items]),na.rm = NAs)
  
  if(type=="count"){
    return(count)
  } else {
    
    mean <- sum/count
    return(mean)
  }
  
}
# testing <- salg %>% mutate(Sum = Scale_Score(.,GainThinkWork,"sum"),
#                            Count = Scale_Score(.,GainThinkWork,"count"),
#                            Mean = Scale_Score(.,GainThinkWork,"mean"))

## as for loop - this works
for(s in names(TheScales)) {
  hclsWithScales <- hclsWithScales %>%
    mutate(UQ(s) := rowSums(select(., one_of(TheScales[[s]])),na.rm=FALSE)/length(TheScales[[s]]))
}

#### Common Recoded MSS Variables ####
## only works with numeric inputs so far
RecodeMSS <- function(mssdata,input=c("numeric","string")){
  
  if(input=="numeric"){
  mssdata <- mssdata %>%
    mutate(Charter = ifelse(SType==7,1,0),
           Female = ifelse(Y1==2,1,0),
           Ninth = ifelse(Y2==9,1,0),
           Age = as.numeric(Y3),
           American_Indian = ifelse(racegraphs==1,1,0),
           Asian = ifelse(racegraphs==2,1,0),
           Black = ifelse(racegraphs==3,1,0),
           White = ifelse(racegraphs==5,1,0),
           Multiracial = ifelse(racegraphs==6,1,0),
           Latinx = ifelse(racegraphs==7,1,0),
           Somali = ifelse(racegraphs==8,1,0),
           Hmong = ifelse(racegraphs==9,1,0),
           Sports = ifelse(is.na(W33a)&is.na(Y34a)&is.na(Y34b),NA,
                           ifelse(Year=="2013"&Y34a==1&Y34b==1,0,
                                  ifelse(Year=="2016"&W33a==1,0,1))),
           School_Clubs = ifelse(Y34c==1,0,1),
           Community_Clubs = ifelse(Y34g==1,0,1),
           Tutoring = ifelse(Y34d==1,0,1),
           Leadership_Activities = ifelse(Y34e==1,0,1),
           Lessons = ifelse(is.na(Y34f)&is.na(W33e)&is.na(W33f),NA,
                            ifelse(Year=="2013"&Y34f==1,0,
                                   ifelse(Year=="2016"&W33e==1&W33f==1,0,1))),
                            Religious_Activities = ifelse(Y34h==1,0,1))
  
  } else {
    stop("Can't do that yet")
  }
  
  

}
#### Tailoring bal.tab output for my purposes ####
Get_BalanceTable <- function(btab,UnAdj){
  
  TheBT <- btab[[1]] %>% rownames_to_column("Variable") %>%
    filter(!str_detect(Variable,":<NA>")) %>%
    select(Variable,Type,ends_with(UnAdj),-starts_with("KS"),-contains("Threshold")) %>%
    mutate(Variable=str_remove(Variable,"_1|_sqrt|_log|_cent"),
           Variable=str_replace_all(Variable,"_"," "),
           Variable=ifelse(str_detect(Variable,"Year"),"Year",Variable)) %>%
    mutate_at(vars(starts_with("M.")),funs(if_else(Type=="Binary",1-.,.)))
  names(TheBT) <- c("Variable","Type","NonAthlete","Athlete","Std_Diff","V_Ratio")
  
  return(TheBT)
}
#### Select helpers ####
Items <- Data %>% select(matches('_1$|_3$|_5$|_8$|_11$|_12$|_13$|_16$|_18$|_20$')) %>% names() # $ signifies "ends with";https://community.rstudio.com/t/regex-within-dplyr-select-helpers/2616

#### Converting Percent to Letter Grades with case_when ####
mutate(Final_Letter = case_when(Final_Course_Percent >= 92 ~ "A",
                                Final_Course_Percent < 92 & Final_Course_Percent >= 90 ~ "A-",
                                Final_Course_Percent < 90 & Final_Course_Percent >= 87 ~ "B+",
                                Final_Course_Percent < 87 & Final_Course_Percent >= 83 ~ "B",
                                Final_Course_Percent < 83 & Final_Course_Percent >= 80 ~ "B-",
                                Final_Course_Percent < 80 & Final_Course_Percent >= 77 ~ "C+",
                                Final_Course_Percent < 77 & Final_Course_Percent >= 73 ~ "C",
                                Final_Course_Percent < 73 & Final_Course_Percent >= 70 ~ "C-",
                                Final_Course_Percent < 70 & Final_Course_Percent >= 65 ~ "D",
                                Final_Course_Percent < 65 ~ "F"))
