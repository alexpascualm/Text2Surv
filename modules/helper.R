source("modules/global.R", local = TRUE, encoding = "UTF-8")

### --- HELPER FUNCTIONS ---

en.intervalos <- function(x, sep="-") {
  sapply(1:(length(x)-1), function(i){ paste(x[i] + 1, sep, x[i + 1]); }); 
}

en.intervalos.duo <- function(x, sep="-") {
  sapply(1:(length(x)/2), function(i){ paste(x[(i*2 - 1)], sep, x[(i*2)]); }); 
}

create_group_borders <- function(ini, end, size){
  diff_years <- (end - ini)
  full_groups <- diff_years %/% size
  remainder_group <- diff_years %% size
  
  if(remainder_group == 0){ 
    return( (0:full_groups)*size + ini )
  } else { 
    return( c((0:full_groups)*size + ini, end+1) )
  }
}

create_highlight_colors <- function(df, var,
                                    base_color = "rgba(192, 192, 192, 0.7)",
                                    highlight_color = "rgba(96,5,113, 0.7)"){
  marker_colors <- rep(base_color, nrow(df))
  marker_colors[which(df[[var]] == max(df[[var]]))] <- highlight_color
  return(marker_colors)
}

get_plot_params <- function(new_params, default_params){
  # default_params :: X_LABEL, Y_LABEL, TITLE_LABEL
  ret <- list()
  ret[["X_LABEL"]] <- if ( !is.null(new_params[["X_LABEL"]]) ) new_params[["X_LABEL"]] else default_params[["X_LABEL"]]
  ret[["Y_LABEL"]] <- if ( !is.null(new_params[["Y_LABEL"]]) ) new_params[["Y_LABEL"]] else default_params[["Y_LABEL"]]
  ret[["TITLE_LABEL"]] <- if ( !is.null(new_params[["TITLE_LABEL"]]) ) new_params[["TITLE_LABEL"]] else default_params[["TITLE_LABEL"]]
  return(ret)
}

is.date <- function(x) inherits(x, 'Date')

is.convertible.to.date <- function(x) {
  bool.is.na <- is.na(x)
  bool.is.convertible <- !is.na(as.Date(as.character(x), format = '%d/%m/%Y'))
  return ( sum(xor(bool.is.na, bool.is.convertible)) == length(x) )
}

# elapsed_months <- function(end_date, start_date) {
#   ed <- as.POSIXlt(end_date)
#   sd <- as.POSIXlt(start_date)
#   return( 12 * (ed$year - sd$year) + (ed$mon - sd$mon) )
# }

get_time_scale <- function(time_scale){
  if(time_scale==TIME_SCALE_DAY) return("d")
  else if(time_scale==TIME_SCALE_MONTH) return("m")
  else if(time_scale==TIME_SCALE_YEAR) return("y")
}

elapsed_time <- function(end_date, start_date, unit = "m") {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  
  if(unit=="d")
    return( as.numeric(difftime(ed, sd, units="days")) )
  if(unit=="m")
    return( 12 * (ed$year - sd$year) + (ed$mon - sd$mon) )
  else if (unit=="y")
    return( (ed$year - sd$year) + (ed$mon - sd$mon)/12 )
}

survival_summary <- function(df, time, var = NULL){
  if(is.null(var)){
    return(
      df %>%
        summarise_(Mean=mean(time, na.rm = TRUE), Max=max(time, na.rm = TRUE), Min=min(time, na.rm = TRUE),
                   Median=median(time, na.rm = TRUE), Std=sd(time, na.rm = TRUE))
    )
  } else {
    return(
      df %>% 
        group_by_(var)%>% 
        summarise_(Mean=mean(time, na.rm = TRUE), Max=max(time, na.rm = TRUE), Min=min(time, na.rm = TRUE),
                   Median=median(time, na.rm = TRUE), Std=sd(time, na.rm = TRUE))
    )
  }
}

# Function to compute the correct statistical test based on types of variables (categorical or numerical)
# Input:
#       formula: expressi?n with dependences of class formula (in R format)
#       data: dataframe with patient data
bivariate_analysis_OLD <- function(formula, data, exclude = NA, threshold_pval = 0.05) {
  vars <- all.vars(formula)
  y <- data[,vars[1]]
  ny <- vars[1]
  x <- data[,vars[2]]
  nx <- vars[2]
  
  # Remove missing data
  data <- na.omit(data[,c(vars[1], vars[2])])
  # Check if categorical or continuos variables
  
  # ------
  # Both categorical variables
  if (is.factor(y) & is.factor(x)) {
    comparison_type <- "Categorical-Categorical"
    ctab <- xtabs(as.formula(paste("~", ny ,"+", nx)), data = data, exclude = exclude)
    if (any(ctab <= 5)) {
      # The number of observations in the contingency table does not fall into the range in which the asymptotic chi-square
      # approximation is valid. One means of overcoming this problem is to use a Monte Carlo simulation
      # (argument simulated.p.value = TRUE), in which contingency tables are generated whose entries are set of random numbers
      # such that their values add up to the marginal values of the contingency table being tested and the fraction of the
      # tables whose chi-squared statistic is more extreme is recorded.
      test <- "Chi-squared Monte Carlo sim"
      pval.chisMC <- chisq.test(ctab, simulate.p.value = T)$p.value
      # An alternative method, Fisher's exact test, relies on the fact that, under the assumption of independence of X and Y
      # and for fixed marginal values, the elements of the contingency table are distributed according to a hypergeometric
      # probability distribution.
      # if (fisher) {
      test <- "Fisher"
      # Exception management by using "try""
      pval.ok <- try(fisher.test(ctab, workspace = 2e+07)$p.value, silent = TRUE)
      if (class(pval.ok) ==  "try-error") {
        # if error with Fisher test, returns chi-q Monte Carlo results
        test <- "Chi-squared Monte Carlo sim"
        pvalue <- pval.chisMC
      }
      else pvalue <- pval.ok
      # }
      # res <- list(ctab = ctab, test = test, pvalue = pvalue, pval.chisMC = pval.chisMC, oddr = oddsratio(ctab)$measure["TRUE",])
      res <- list(ctab = ctab, test = test, pvalue = pvalue, pval.type = pval.chisMC, type = "Chi-squared MC sim")
    }
    else {
      test <- "Chi-squared"; pvalue <- chisq.test(ctab)$p.value
      # res <- list(ctab = ctab, test = test, pvalue = pvalue, oddr = oddsratio(ctab)$measure["TRUE",])
      res <- list(ctab = ctab, test = test, pvalue = pvalue)
    }
    # if (fig) {par(mfrow = c(1,1)); barplot(ctab, legend = TRUE, beside = TRUE, xlab = nx, ylab = ny,
    #                                        main = paste(ny, " vs ", nx, " (", test, " pvalue = ", ifelse(is.na(pvalue), round(pval.chisMC, 5), round(pvalue, 5)), ")", sep = ""))}       
  }
  # -----
  # Both numerical variables
  else if (is.numeric(y) & is.numeric(x)) {
    comparison_type <- "Numeric-Numeric"
    # If the samples follow independent normal distributions
    if ((shapiro.test(x)$p.value > threshold_pval) & (shapiro.test(y)$p.value > threshold_pval)) {
      # The test statistic is based on Pearson's product moment correlation coefficient cor(x, y) and follows a t distribution
      test <- "Pearson correlation"
      test_res <- cor.test(y, x, alternative="two.sided", method="pearson")
      pvalue <- test_res$p.value
      res <- list(test.norm = TRUE, test = test, pval = p.value)
    }
    # If the data do not necessarily come from a bivariate normal distribution
    else {
      # Kendall's tau or Spearman's rho statistic is used to estimate a rank-based measure of association
      # Spearmann when variable are both continuos
      test <- "Spearmann's rho statistic"
      
      test_res <- cor.test(y, x, alternative="two.sided", method="spearman")
      pvalue <- test_res$p.value
      # Kendall when any of variables are ordinal
    }
    
    res <- list(test.norm = (shapiro.test(x)$p.value > threshold_pval) & (shapiro.test(y)$p.value > threshold_pval),
                test = test, pvalue = pvalue, coef = test_res[["estimate"]])
    # if (fig) {par(mfrow = c(1,1)); plot(x, y, xlab = nx, ylab = ny,
    #                                     main = paste(ny, " vs ", nx, " (", test, " pvalue = ", round(pvalue, 5), ")", sep = ""))}   
  }
  # -----
  # Categorical and numerical variables
  else {
    comparison_type <- "Numeric-Categoric"
    # Swap x and y
    if (is.factor(x)) {x <- data[,vars[1]]; nx <- vars[1]; y <- data[,vars[2]]; ny <- vars[2]}
    if (shapiro.test(x)$p.value <= threshold_pval) {
      # Compute median for categories (not mean, it?s not normal distribution)
      mean.med <- tapply(x, y, median, na.rm=TRUE)
      if (nlevels(y) > 2) {
        # Not normal distribution.
        test <- "Kruskal-Wallis"
        # kruskal.test performs a Kruskal-Wallis rank sum test of the null that the location parameters of the distribution
        # x are the same in each group (sample). The alternative is that they differ in at least one.
        # The Wilcoxon rank sum test (wilcox.test) as the special case for two samples.
        pvalue <- kruskal.test(as.formula(paste(nx, "~", ny)), data = data)$p.value
        # Other way: Compute multinom models
        modelo0 <- multinom(as.formula(paste(ny, "~ 1")), data = data, trace = F)
        modelo1 <- multinom(as.formula(paste(ny, "~", nx)), data = data, trace = F)
        pval.mult <- anova(modelo0, modelo1)[2,"Pr(Chi)"]
        res <- list(test.norm = shapiro.test(x), mean.med = mean.med, test = test, 
                    pvalue = pvalue, pval.type = pval.mult, type = "Multinomial")
      } else {
        test <- "Wilcoxon rank sum" # Mann-Whitney test
        pvalue <- wilcox.test(as.formula(paste(nx, "~", ny)), data = data)$p.value
        pval.glm <- summary(glm(as.formula(paste(ny, "~", nx)), data = data,
                                family=binomial("logit")))$coefficients[2,"Pr(>|z|)"]
        res <- list(test.norm = shapiro.test(x), mean.med = mean.med, test = test, 
                    pvalue = pvalue, pval.type = pval.glm, type = "GLM")
      }
    } else {
      # Not normal distribution.
      test <- "Student's t test"
      # lm together with anova for performing one-way location analysis under normality assumptions.
      # Student's t test (t.test) as the special case for two samples
      mean.m <- tapply(x, y, mean, na.rm=TRUE)
      pvalue <- t.test(as.formula(paste(nx, "~", ny)), data = data)$p.value
      pval.aov <- unlist(summary(aov(as.formula(paste(nx, "~", ny)), data = data)))["Pr(>F)1"]
      res <- list(test.norm = shapiro.test(x), mean.med = mean.m, test = test, 
                  pvalue = pvalue, pval.type = pval.aov, type = "AOV")
    }
    # if (fig) {par(mfrow = c(1,1)); Boxplot(as.formula(paste(nx, "~", ny)), data = data, id.method="y",
    # main = paste(ny, " vs ", nx, " (", test, " pvalue = ", round(pvalue, 5), ")", sep = ""))}
  }
  # res <- data.frame(matrix(unlist(res), nrow=length(res), byrow=TRUE))
  res$cmp_type <- comparison_type
  return (res)
}

check_normality <- function(x, threshold_pval = 0.05){
  if(length(x) < 50){
    return(shapiro.test(x)$p.value > threshold_pval)
  } else {
    return(ad.test(x)$p.value > threshold_pval)
  }
}

check_homoscedasticity_numeric_numeric <-
  function(x, y, threshold_pval = 0.05) {
    return(var.test(x, y)$p.value > threshold_pval)
  }

check_homoscedasticity_numeric_categoric <-
  function(x, y, threshold_pval = 0.05) {
    return(leveneTest(x, y)[1, 3] > threshold_pval)
  }

# Function to compute the correct statistical test based on types of variables (categorical or numerical)
# Input:
#       formula: expressi?n with dependences of class formula (in R format)
#       data: dataframe with patient data


bivariate_analysis <-
  function(formula,
           data,
           exclude = NA,
           threshold_pval = 0.05) {
    vars <- all.vars(formula)
    y <- data[, vars[1]]
    ny <- vars[1]
    x <- data[, vars[2]]
    nx <- vars[2]
    
    # Remove missing data
    data <- na.omit(data[, c(vars[1], vars[2])])
    # Check if categorical or continuos variables
    
    # ------
    # Both categorical variables
    if (is.factor(y) & is.factor(x)) {
      comparison_type <- "Categorical-Categorical"
      ctab <-
        xtabs(as.formula(paste("~", ny , "+", nx)), data = data, exclude = exclude)
      if (any(ctab <= 5)) {
        # The number of observations in the contingency table does not fall into the range in which the asymptotic chi-square
        # approximation is valid. One means of overcoming this problem is to use a Monte Carlo simulation
        # (argument simulated.p.value = TRUE), in which contingency tables are generated whose entries are set of random numbers
        # such that their values add up to the marginal values of the contingency table being tested and the fraction of the
        # tables whose chi-squared statistic is more extreme is recorded.
        test <- "Chi-squared Monte Carlo sim"
        pval.chisMC <- chisq.test(ctab, simulate.p.value = T)$p.value
        # An alternative method, Fisher's exact test, relies on the fact that, under the assumption of independence of X and Y
        # and for fixed marginal values, the elements of the contingency table are distributed according to a hypergeometric
        # probability distribution.
        # if (fisher) {
        test <- "Fisher"
        # Exception management by using "try""
        pval.ok <-
          try(fisher.test(ctab, workspace = 2e+07)$p.value, silent = TRUE)
        if (class(pval.ok) ==  "try-error") {
          # if error with Fisher test, returns chi-q Monte Carlo results
          test <- "Chi-squared Monte Carlo sim"
          pvalue <- pval.chisMC
        }
        else
          pvalue <- pval.ok
        # }
        # res <- list(ctab = ctab, test = test, pvalue = pvalue, pval.chisMC = pval.chisMC, oddr = oddsratio(ctab)$measure["TRUE",])
        res <-
          list(
            ctab = ctab,
            test = test,
            pvalue = pvalue,
            pval.type = pval.chisMC,
            type = "Chi-squared MC sim"
          )
      }
      else {
        test <- "Chi-squared"
        pvalue <- chisq.test(ctab)$p.value
        # res <- list(ctab = ctab, test = test, pvalue = pvalue, oddr = oddsratio(ctab)$measure["TRUE",])
        res <- list(ctab = ctab,
                    test = test,
                    pvalue = pvalue)
      }
      # if (fig) {par(mfrow = c(1,1)); barplot(ctab, legend = TRUE, beside = TRUE, xlab = nx, ylab = ny,
      #                                        main = paste(ny, " vs ", nx, " (", test, " pvalue = ", ifelse(is.na(pvalue), round(pval.chisMC, 5), round(pvalue, 5)), ")", sep = ""))}
    }
    # -----
    # Both numerical variables
    else if (is.numeric(y) & is.numeric(x)) {
      comparison_type <- "Numeric-Numeric"
      # If the samples follow independent normal distributions
      normality_x <- check_normality(x, threshold_pval)
      normality_y <- check_normality(y, threshold_pval)
      homoscedasticity_check <-
        check_homoscedasticity_numeric_numeric(x, y)
      
      if (normality_x & normality_y & homoscedasticity_check) {
        # The test statistic is based on Pearson's product moment correlation coefficient cor(x, y) and follows a t distribution
        test <- "Pearson correlation"
        test_res <-
          cor.test(y, x, alternative = "two.sided", method = "pearson")
        pvalue <- test_res$p.value
        res <- list(test.norm = TRUE,
                    test = test,
                    pval = p.value)
      }
      # If the data do not necessarily come from a bivariate normal distribution
      else {
        # Kendall's tau or Spearman's rho statistic is used to estimate a rank-based measure of association
        # Spearmann when variable are both continuos
        test <- "Spearmann's rho statistic"
        
        test_res <-
          cor.test(y, x, alternative = "two.sided", method = "spearman")
        pvalue <- test_res$p.value
        # Kendall when any of variables are ordinal
      }
      
      res <-
        list(
          test.norm = (shapiro.test(x)$p.value > threshold_pval) &
            (shapiro.test(y)$p.value > threshold_pval),
          test = test,
          pvalue = pvalue,
          coef = test_res[["estimate"]]
        )
      # if (fig) {par(mfrow = c(1,1)); plot(x, y, xlab = nx, ylab = ny,
      #                                     main = paste(ny, " vs ", nx, " (", test, " pvalue = ", round(pvalue, 5), ")", sep = ""))}
    }
    # -----
    # Categorical and numerical variables
    else {
      comparison_type <- "Numeric-Categoric"
      # Swap x and y
      if (is.factor(x)) {
        x <-
          data[, vars[1]]
        nx <- vars[1]
        y <- data[, vars[2]]
        ny <- vars[2]
      }
      
      
      normality_x <- check_normality(x, threshold_pval)
      homoscedasticity_check <-
        check_homoscedasticity_numeric_categoric(x, y)
      
      if (!normality_x & !homoscedasticity_check) {
        # Compute median for categories (not mean, it?s not normal distribution)
        mean.med <- tapply(x, y, median, na.rm = TRUE)
        if (nlevels(y) > 2) {
          # Not normal distribution.
          test <- "Kruskal-Wallis"
          # kruskal.test performs a Kruskal-Wallis rank sum test of the null that the location parameters of the distribution
          # x are the same in each group (sample). The alternative is that they differ in at least one.
          # The Wilcoxon rank sum test (wilcox.test) as the special case for two samples.
          pvalue <-
            kruskal.test(as.formula(paste(nx, "~", ny)), data = data)$p.value
          # Other way: Compute multinom models
          modelo0 <-
            multinom(as.formula(paste(ny, "~ 1")),
                     data = data,
                     trace = F)
          modelo1 <-
            multinom(as.formula(paste(ny, "~", nx)),
                     data = data,
                     trace = F)
          pval.mult <- anova(modelo0, modelo1)[2, "Pr(Chi)"]
          res <-
            list(
              test.norm = normality_x,
              mean.med = mean.med,
              test = test,
              pvalue = pvalue,
              pval.type = pval.mult,
              type = "Multinomial"
            )
        } else {
          test <- "Wilcoxon rank sum" # Mann-Whitney test
          pvalue <-
            wilcox.test(as.formula(paste(nx, "~", ny)), data = data)$p.value
          pval.glm <- summary(glm(
            as.formula(paste(ny, "~", nx)),
            data = data,
            family = binomial("logit")
          ))$coefficients[2, "Pr(>|z|)"]
          res <-
            list(
              test.norm = normality_x,
              mean.med = mean.med,
              test = test,
              pvalue = pvalue,
              pval.type = pval.glm,
              type = "GLM"
            )
        }
      } else {
        # Not normal distribution.
        test <- "Student's t test"
        # lm together with anova for performing one-way location analysis under normality assumptions.
        # Student's t test (t.test) as the special case for two samples
        mean.m <- tapply(x, y, mean, na.rm = TRUE)
        pvalue <-
          t.test(as.formula(paste(nx, "~", ny)), data = data)$p.value
        pval.aov <-
          unlist(summary(aov(as.formula(
            paste(nx, "~", ny)
          ), data = data)))["Pr(>F)1"]
        res <-
          list(
            test.norm = normality_x,
            mean.med = mean.m,
            test = test,
            pvalue = pvalue,
            pval.type = pval.aov,
            type = "AOV"
          )
      }
      # if (fig) {par(mfrow = c(1,1)); Boxplot(as.formula(paste(nx, "~", ny)), data = data, id.method="y",
      # main = paste(ny, " vs ", nx, " (", test, " pvalue = ", round(pvalue, 5), ")", sep = ""))}
    }
    # res <- data.frame(matrix(unlist(res), nrow=length(res), byrow=TRUE))
    res$cmp_type <- comparison_type
    return (res)
  }


# x0 <- rnorm(50, mean = 0, sd = 1)
# x1 <- rnorm(30, mean = 1, sd = 1)
# x2 <- runif(200, 0, 100)
# y0 <- as.factor( ifelse(x0 > -0.5, "Y", "N") )
# y1 <- as.factor( ifelse(x1 > 0.5, "Y", "N") )
# y2 <- as.factor( ifelse(x2 > 25, "Y", "N") )
# check_normality(x0)
# check_normality(x1)
# check_normality(x2)
# var.test(x0,x1)$p.value
# var.test(x0,x2)$p.value
# var.test(x1,x2)$p.value
# check_homoscedasticity_numeric_numeric(x0,x1)
# check_homoscedasticity_numeric_numeric(x0,x2)
# check_homoscedasticity_numeric_numeric(x1,x2)
# check_homoscedasticity_numeric_categoric(x0, y0)
# check_homoscedasticity_numeric_categoric(x1, y1)
# check_homoscedasticity_numeric_categoric(x2, y2)

# Ejemplo de uso: bivariate_analysis(ra ~ Subtipo.IHQ, mydata, fig = T)

get_significance_level <- function(pval, char = '*'){
  if(pval > 0.05 || is.na(pval)) return("")
  else if(pval > 0.01) return(".")
  else if(pval > 0.005) return("*")
  else if(pval > 0.001) return("**")
  else return("***")
}

complete_cases_df <- function(data, desired_cols) {
  complete_vec <- complete.cases(data[, desired_cols])
  return(data[complete_vec, ])
}

filter_not_validated_patients <- function(df){
    filter_df <- df
    if(("ESTADO_PDF" %in% names(filter_df)) ){
      filter_df <- subset(df, ESTADO_PDF != 'Sin validar - Antiguo' & ESTADO_PDF != 'Sin validar - Nuevo')
    }
   
  return(filter_df)
}


