
#' Lisa's Summary Table Function 
#'
#' This function creates a nice looking summary table similar 
#' to the Mayo clinic SAS table macro. The function returns a dataframe. 
#' The function also prints an html table by default for use in R markdown documents.
#' @param df Dataframe object name (REQUIRED).
#' @param covs Vector of covariates to include in table (REQUIRED).
#' @param type Vector indicating type of each covariates (REQUIRED). 
#' @param by Variable to stratify by. Defaults to NA (no stratifying variable). No tests will be done.
#' @param warnmissby Whether to warn user that there are missing by variable values. Missing values will be excluded. 
#' Default = FALSE.
#' 1 = continuous, 2 = categorical, 
#' 3 = ordinal (factor levels must be set in ascending order prior to running). 
#' If only one is listed, the type will be repeated for each covariate.
#' @param orderfreq If TRUE, categorical variables will be ordered by descending frequency. Default = FALSE.
#' @param labels Labels for covariates. Default = NA in which case variable names will be used.
#' @param stats Statistics to display for continuous variables (mean_sd_median_range (default), mean_sd, mean_sem, median_range, or median_iqr). 
#' Can be a vector or length one to apply to all continuous variables.
#' @param statlabs If TRUE, continuous variable labels will include description of summary statistics. Default = FALSE.
#' @param tests Vector of tests to calculate p-values. If only one is entered it will apply to all covs. 
#' If NA (default), no tests will be done.
#' Parametric ("p": t-test, chi-squared, anova), 
#' Non-Parametric ("np": ranksum, Fisher's exact, kruskal-wallis), 
#' T-test ("ttest"), Chi-Squared ("chisq"), 
#' Kruskal-Wallis ("kw"), Mann-Whitney Rank Sum ("ranksum"), Anova ("anova"), 
#' Fisher's Exact test ("fe") are curretly supported.
#' @param paired Whether test should be paired (TRUE) or unpaired (FALSE = default). Only available for ttest and ranksum.
#' @param perc.dec Number of decimals for percentages (categorcal variables). Default = 1.
#' @param cont.dec Number of decimals for continuous variable summary stats (mean, median, sd, iqr). Default = 2.
#' @param pval.dec Number of significant figures for p-values. Default = 3.
#' @param allcol Whether to diplay the "All Data" column. Default = TRUE.
#' @param testcol Whether to display the test column (names of tests). Default = TRUE.
#' @param dispmiss Whether to display number missing for continuous variables. Default = FALSE.
#' @param dispN Whether to display number non-missing for continuous variables. Default = FALSE. 
#' dispN will overwrite dispmiss if both are TRUE.
#' @param printRMD Whether to print resulting table to Rmd via xtable. Default = TRUE.
#' @param htmlTable Whether to use htmlTable package to display table (instead of xtable). Default = FALSE.
#' @param color Hex color to use for htmlTable output. Default = "#EEEEEE" (grey).
#' @param blanks Should blank rows be used as variable separators? Default = TRUE.
#' @param percent Should row (1) or column (2, default) percents be used?
#' @param pvalcol Should a column be included for p-values? TRUE (default) or FALSE. 
#' @keywords summary table lisa
#' @importFrom doBy summaryBy
#' @importFrom xtable xtable
#' @import Hmisc 
#' @importFrom MASS polr
#' @importFrom htmlTable htmlTable
#' @export 
nicetable <- function(df,
                      covs,
                      type, 
                      by = NA,
                      warnmissby = FALSE,
                      orderfreq = FALSE,
                      labels = NA,
                      stats = "mean_sd_median_range",
                      statlabs = TRUE,
                      tests = NA,
                      percent = 2,
                      perc.dec = 1,
                      cont.dec = 2,
                      pval.dec = 3,
                      allcol = TRUE,
                      testcol = TRUE,
                      pvalcol = TRUE,
		              dispmiss = FALSE,
                      dispN = FALSE,
                      printRMD = TRUE,
                      paired = FALSE,
                      blanks = TRUE,
                      htmlTable = FALSE,
                      color = "#EEEEEE"){

#     ### load packages
#     require(doBy)
#     require(Hmisc)
#     require(xtable)
#     require(MASS)
#     require(htmlTable)
    
    if (htmlTable == TRUE) printRMD = FALSE
    
    noby <- 0
    noby[is.na(by)] <- 1
 
    if (is.na(by)){
        df$Allcol <- "All"
        by <- "Allcol"
        allcol <- FALSE
    }
    
    if (paired == TRUE){
        allcol <- FALSE
    }
    
    pvalf <- paste("%.", pval.dec, "f", sep="")
    percf <- paste("%.", perc.dec, "f", sep="")
    contf <- paste("%.", cont.dec, "f", sep="")
    
    ### define functions for continuous summary stats
    mean_sd <- function(x){
        paste(sprintf(contf, round(mean(x, na.rm=TRUE),cont.dec)), " (", 
              sprintf(contf, round(  sd(x, na.rm=TRUE),cont.dec)), ")", sep="")
    }
    
    median_iqr <- function(x){
        paste(sprintf(contf, round(median(x, na.rm=TRUE),cont.dec)), " (", 
              sprintf(contf, round(   IQR(x, na.rm=TRUE),cont.dec)), ")", sep="")
    }
    
    median_range <- function(x){
        paste(sprintf(contf, round(median(x, na.rm=TRUE),cont.dec)), " [", 
              sprintf(contf, round(   min(x, na.rm=TRUE),cont.dec)), ", ", 
              sprintf(contf, round(   max(x, na.rm=TRUE),cont.dec)), "]", sep="")
    }
    
    mean_sd_median_range <- function(x){
        paste(sprintf(contf, round(  mean(x, na.rm=TRUE), cont.dec)), " (",
              sprintf(contf, round(    sd(x, na.rm=TRUE), cont.dec)), "), ",
              sprintf(contf, round(median(x, na.rm=TRUE),cont.dec)), " [", 
              sprintf(contf, round(   min(x, na.rm=TRUE),cont.dec)), ", ", 
              sprintf(contf, round(   max(x, na.rm=TRUE),cont.dec)), "]", sep="")
    }
    
    mean_sd_median_iqr <- function(x){
        paste(sprintf(contf, round(  mean(x, na.rm=TRUE), cont.dec)), " (",
              sprintf(contf, round(    sd(x, na.rm=TRUE), cont.dec)), "), ",
              sprintf(contf, round(median(x, na.rm=TRUE),cont.dec)) , " (", 
              sprintf(contf, round(   IQR(x, na.rm=TRUE),cont.dec)), ")", sep="")
    }
    
    sem <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
    mean_sem <- function(x){
        paste(sprintf(contf, round(mean(x, na.rm=TRUE),cont.dec)), " (", 
              sprintf(contf, round( sem(x)            ,cont.dec)), ")", sep="")
    }
    
    ### if only one option is entered, repeat for all variables
    if (length(type) == 1) {
        type <- rep(type, length(covs))
    }
    if (length(stats) == 1){
        stats <- rep(stats, length(covs))
    }
    stats[type == 2] <- "NA"
    if (length(tests) == 1){
        tests <- rep(tests, length(covs))
    }
    if (length(percent) == 1){
        percent <- rep(percent, length(covs))
    }
    
    ### make all options lower case
    tests <- tolower(tests)
    stats <- tolower(stats)
    
    if (!is.na(tests[1]) & tests[1] == "p"){
        tests[type == 1] <- "ttest"
        tests[type == 2] <- "chisq"
    }
    
    if (!is.na(tests[1]) & tests[1] == "np"){
        tests[type == 1] <- "ranksum"
        tests[type == 2] <- "fe"
    }
    
    ## fix any obvious mistakes 
    ## if user selects ttest and the variables are categorical try a chisq test, etc.
    tests[which((tests == "ttest"   | tests == "anova") & type == 2)] <- "chisq"
    tests[which((tests == "ranksum" | tests == "kw")    & type == 2)] <- "fe"
    tests[which(tests == "chisq" & type == 1)] <- "ttest"
    tests[which(tests == "fe"    & type == 1)] <- "ranksum"
    
    if (paired == TRUE){
        testlabs[tests == "ttest"]    <- "Paired t-test"
        testlabs[tests == "ranksum"]    <- "Wilcoxon signed-rank"
    }
    
    testlabs <- tests
    testlabs[tests == "chisq"] <- "Chi-squared"
    testlabs[tests == "fe"]    <- "Fisher's exact"
    testlabs[tests == "ttest"]    <- "T-test"
    testlabs[tests == "ranksum"]    <- "Wilcoxon rank-sum"
    testlabs[tests == "olr"]    <- "Ordinal LR"
    
    ### if labels are NA, use variable names
    if (length(labels) == 1){
        labels <- rep(labels, length(covs))
    }
    labels[is.na(labels)] <- covs[is.na(labels)]
    
    ## if statlabs is TRUE add a note about which summary stats are displayed
#     if (statlabs == TRUE){
#         labels[stats == "mean_sd"] <- paste(labels[stats == "mean_sd"], ", Mean (SD)", sep="")
#         labels[stats == "mean_sem"] <- paste(labels[stats == "mean_sem"], ", Mean (SEM)", sep="")
#         labels[stats == "median_iqr"] <- paste(labels[stats == "median_iqr"], ", Median (IQR)", sep="")
#         labels[stats == "median_range"] <- paste(labels[stats == "median_range"], ", Median [Min, Max]", sep="")
#         labels[stats == "mean_median_range"] <- paste(labels[stats == "mean_median_range"], ", Mean, Median [Min, Max]", sep="")
#         labels[stats == "mean_sd_range"] <- paste(labels[stats == "mean_sd_range"], ", Mean (SD) [Min, Max]", sep="")
#         labels[stats == "mean_sd_median_range"] <- paste(labels[stats == "mean_sd_median_range"], sep="")
#     }
    
    ### if tests is blank do not calculate a p-value
    if (length(tests) == 1){
        tests <- rep(tests, length(covs))
    }
    pval <- rep(TRUE, length(covs))
    pval[is.na(tests)] <- FALSE
    
    ### treat the by variables as a factor if not already
    ### subset dataframe for non missing by values
    df[,by] <- as.factor(df[,by])
    total_levels <- 0
    if (sum(is.na(df[,by])) > 0){
        if (warnmissby == TRUE){
            cat("There were", sum(is.na(df[,by])), "entries missing", by, "which were excluded.")
        }
    }
    df <- df[!is.na(df[,by]),]


    ## save color x number of rows per variable
    rgroup <- NULL
    
    sum_table <- NULL
    
    for (k in 1:length(covs)){
        
        p <- NA
        
        if (type[k] == 2){
            
            df[grepl("Missing", df[,covs[k]]), covs[k]] <- NA
            
            ### check for missing covariate values 
            missing <- FALSE
            if (sum(is.na(df[,covs[k]])) > 0) missing <- TRUE
            
            ### treat categorical covariates as factors
            df[,covs[k]] <- factor(df[,covs[k]])
            ### if orderfreq = TRUE then reorder factor levels by descending frequency
            if (orderfreq == TRUE) {
                tb <- table(df[,covs[k]])
                df[,covs[k]] <- factor(df[,covs[k]], 
                                       levels = names(tb[order(tb, decreasing = TRUE)])) 
            }
            
            ### create a mini table for this variable alone
            ### the number of columns is the number of groups 
            ### + 1 names column + 1 pvalue column + 1 test name column
            ### the number of rows is the number of levels + 1 for varname/label
            ### if there are missing values add another row for missing totals
            if (missing == FALSE){
                tmp <- matrix(data = NA, 
                              ncol = nlevels(df[,by]) + 4, 
                              nrow = 1 + nlevels(df[,covs[k]]))
            }
            if (missing == TRUE){
                tmp <- matrix(data = NA, 
                              ncol = nlevels(df[,by]) + 4, 
                              nrow = 2 + nlevels(df[,covs[k]]))
            }
            
            ### add first column variable label and names of levels
            tmp[1,1] <- labels[k]
            tmp[2:(1+nlevels(df[,covs[k]])),1] <- paste("* ",levels(df[,covs[k]]), "\n", sep="")
            
            ### create temporary dataset with NA's removed
            df.complete <- df[!is.na(df[,covs[k]]),]
            
            freq.all <- table(df.complete[,covs[k]])                 
            perc.all <- 100*(table(df.complete[,covs[k]])/nrow(df.complete))
            
            tmp[2:(1+nlevels(df[,covs[k]])),2] <- 
                paste(freq.all, " (", sprintf(percf, round(perc.all,perc.dec)), "%)", sep="")
            
            ## calculate column percents
            if (percent[k] == 2){
                freq <- as.matrix(table(df.complete[,covs[k]], df.complete[,by]))
                coltots <- table(df.complete[,by])
                nmat <- NULL
                for (i in 1:nlevels(df.complete[,covs[k]])){
                    nmat <- rbind(nmat, coltots)
                }
                nvals <- as.matrix(nmat)
                perc <- (freq/nvals)*100
            }
            ## calculate row percents
            if (percent[k] == 1){
                freq <- as.matrix(table(df.complete[,covs[k]], df.complete[,by]))
                rowtots <- table(df.complete[,covs[k]])
                nmat <- NULL
                for (i in 1:nlevels(df.complete[,by])){
                    nmat <- cbind(nmat, rowtots)
                }
                nvals <- as.matrix(nmat)
                perc <- (freq/nvals)*100
            }
            
            perc2 <- perc
            perc2[!is.nan(perc2)] <- sprintf(percf, round(perc2[!is.nan(perc2)],perc.dec))
            perc2[perc2 == "NaN"] <- "--"
            
            tmp[2:(1+nlevels(df.complete[,covs[k]])), 3:(2+nlevels(df.complete[,by]))] <- 
                paste(freq, " (", perc2, "%)", sep="")
            
            if (pval[k] == TRUE){
                
                freq <- table(df.complete[,covs[k]], df.complete[,by])
                

                if (tests[k] == "fe") {
                    try_fe <- try(fisher.test(freq))
                    if (length(try_fe) >  1) {
                        p <- fisher.test(freq)$p.value
                        testlabs[k] <- "Fisher's exact"
                    }
                    ### if fisher's exact test will not run try chisq
                    if (length(try_fe) == 1) tests[k] <- "chisq" 
                }   
                if (tests[k] == "chisq") {
                    try_chisq <- try(chisq.test(freq))
                    if (length(try_chisq) >  1){
                        p <- chisq.test(freq)$p.value
                        testlabs[k] <- "Chi-squared"
                    } 
                    if (length(try_chisq) == 1){
                        p <- NA
                        testlabs[k] <- "NA"
                    } 
                }
                if (tests[k] == "olr"){
                    form <- as.formula(paste(covs[k], "~", by))
                    form.null <- as.formula(paste(covs[k], "~ 1"))
                    
                    t <- polr(form, data = df.complete)
                    null <- polr(form.null, data = df.complete)
                    
                    save <- lrtest(t, null)
                    p <- save[["Pr(>Chisq)"]][[2]]
                }
                ### if there is only one level do not do any tests
                if (nlevels(df[,covs[k]]) < 2){
                    p <- NA
                    testlabs[k] <- NA
                } 
                
                tmp[1,(3+nlevels(df[,by]))] <- sprintf(pvalf, round(p, pval.dec))
                if (is.na(p)) {
                    tmp[1,(3+nlevels(df[,by]))] <- "--"
                    p <- 99
                }
                if (pval.dec == 4 & p < 0.0001) tmp[1,(3+nlevels(df[,by]))] <- "< 0.0001"
                if (pval.dec == 3 & p < 0.001 ) tmp[1,(3+nlevels(df[,by]))] <- "< 0.001"
                if (pval.dec == 2 & p < 0.01  ) tmp[1,(3+nlevels(df[,by]))] <- "< 0.01"
                
                if (pval.dec == 4 & round(p,4) == 1) tmp[1,(3+nlevels(df[,by]))] <- "> 0.9999"
                if (pval.dec == 3 & round(p,3) == 1) tmp[1,(3+nlevels(df[,by]))] <- "> 0.999"
                if (pval.dec == 2 & round(p,2) == 1) tmp[1,(3+nlevels(df[,by]))] <- "> 0.99"
                
                tmp[1,(4+nlevels(df[,by]))] <- testlabs[k]
            }
            
            if (missing == TRUE){
                tmp[nrow(tmp),1] <- "* Unknown/Missing"
                tmp[nrow(tmp),2] <- sum(is.na(df[,covs[k]]))
                tmp[nrow(tmp),3:(2+nlevels(df[,by]))] <- 
                    table(is.na(df[,covs[k]]), df[,by])["TRUE",]
            }
            
            if (k %% 2 == 0) rgroup <- c(rgroup, rep("none", nrow(tmp))) 
            if (k %% 2 != 0) rgroup <- c(rgroup, rep(color, nrow(tmp)))
            if (blanks == TRUE){
                blank <- rep(NA, ncol(tmp))
                sum_table <- rbind(sum_table, blank, tmp)
            }
            
        }
        
        if (type[k] == 1){
            
            ### check for missing covariate values 
            missing <- FALSE
            if (sum(is.na(df[,covs[k]])) > 0) missing <- TRUE
            
            ### create temporary dataset with NA's removed
            df.complete <- df[!is.na(df[,covs[k]]),]
            
            ### create a mini table for this variable alone
            ### the number of columns is the number of groups 
            ### + 1 names column + 1 pvalue column + 1 test name column
            ### the number of rows is 1 for no missing values and 2 if missing values
            if (missing == FALSE | (missing == TRUE & dispmiss == FALSE & dispN == FALSE)){
                tmp <- matrix(data = NA, 
                              ncol = nlevels(df[,by]) + 4, 
                              nrow = 2)
                if (stats[k] == "mean_sd_median_range" | stats[k] == "mean_sd_median_iqr"){
                    tmp <- matrix(data = NA, 
                                  ncol = nlevels(df[,by]) + 4, 
                                  nrow = 3)
                }
            }
            if (missing == TRUE & (dispmiss == TRUE | dispN == TRUE)){
                tmp <- matrix(data = NA, 
                              ncol = nlevels(df[,by]) + 4, 
                              nrow = 3)
                if (stats[k] == "mean_sd_median_range" | stats[k] == "mean_sd_median_iqr"){
                    tmp <- matrix(data = NA, 
                                  ncol = nlevels(df[,by]) + 4, 
                                  nrow = 4)
                }
            }
            
            tmp[1,1] <- paste(labels[k], sep=" ") 
            
            if (stats[k] == "mean_sd"){
                tmp[2, 2] <- mean_sd(df[,covs[k]])
                tmp[2, 3:(2+nlevels(df[,by]))] <- 
                    as.character(summaryBy(as.formula(paste(covs[k], "~", by)), 
                                           data = df,
                                           FUN = mean_sd)[,2])
                tmp[2,1] <- "* Mean (SD)"
            } 
            if (stats[k] == "mean_sem"){
                tmp[2, 2] <- mean_sem(df[,covs[k]])
                tmp[2, 3:(2+nlevels(df[,by]))] <- 
                    as.character(summaryBy(as.formula(paste(covs[k], "~", by)), 
                                           data = df,
                                           FUN = mean_sem)[,2])
                tmp[2,1] <- "* Mean (SEM)"
            } 
            if (stats[k] == "median_iqr"){
                tmp[2, 2] <- median_iqr(df[,covs[k]])
                tmp[2, 3:(2+nlevels(df[,by]))] <- 
                    as.character(summaryBy(as.formula(paste(covs[k], "~", by)), 
                                           data = df,
                                           FUN = median_iqr)[,2])
                tmp[2,1] <- "* Median (IQR)"
            } 
            if (stats[k] == "median_range"){
                tmp[2, 2] <- median_range(df[,covs[k]])
                tmp[2, 3:(2+nlevels(df[,by]))] <- 
                    as.character(summaryBy(as.formula(paste(covs[k], "~", by)), 
                                           data = df,
                                           FUN = median_range)[,2])
                tmp[2,1] <- "* Mean [Min, Max]"
            } 
            if (stats[k] == "mean_sd_median_range"){
                tmp[2,2] <- mean_sd(df[,covs[k]])
                tmp[3,2] <- median_range(df[,covs[k]])
                tmp[2, 3:(2+nlevels(df[,by]))] <- 
                    as.character(summaryBy(as.formula(paste(covs[k], "~", by)), 
                                           data = df,
                                           FUN = mean_sd)[,2])
                tmp[3, 3:(2+nlevels(df[,by]))] <- 
                    as.character(summaryBy(as.formula(paste(covs[k], "~", by)), 
                                           data = df,
                                           FUN = median_range)[,2])
                tmp[2,1] <- "* Mean (SD)"
                tmp[3,1] <- "* Median [Min, Max]"
            }
            if (stats[k] == "mean_sd_median_iqr"){
                tmp[2,2] <- mean_sd(df[,covs[k]])
                tmp[3,2] <- median_iqr(df[,covs[k]])
                tmp[2, 3:(2+nlevels(df[,by]))] <- 
                    as.character(summaryBy(as.formula(paste(covs[k], "~", by)), 
                                           data = df,
                                           FUN = mean_sd)[,2])
                tmp[3, 3:(2+nlevels(df[,by]))] <- 
                    as.character(summaryBy(as.formula(paste(covs[k], "~", by)), 
                                           data = df,
                                           FUN = median_iqr)[,2])
                tmp[2,1] <- "* Mean (SD)"
                tmp[3,1] <- "* Median (IQR)"
            }
            
            if (pval[k] == TRUE){
                
                form <- as.formula(paste(covs[k], "~", by))
                
                if (nlevels(df[,by]) == 2){
                    
                    lev1 <- levels(df[,by])[1]
                    lev2 <- levels(df[,by])[2]
                    
                    v1 <- df[df[,by] == lev1, covs[k]]
                    v2 <- df[df[,by] == lev2, covs[k]]
                
                    if (tests[k] == "ttest" | tests[k] == "anova"){
                        
                        try_ttest <- try(t.test(form, data = df))
                        
                        if (length(try_ttest) > 1){
                            p <- t.test(form, data= df)$p.value
                            testlabs[k] <- "T-test"
                            if (paired == TRUE){
                                p <- t.test(v1,v2, paired = TRUE)$p.value
                                testlabs[k] <- "Paired t-test"
                            }                            
                        }
                        
                        if (length(try_ttest) == 1){
                            p <- NA
                            testlabs[k] <- NA
                        }

                    }   
                    if (tests[k] == "ranksum" | tests[k] == "kw"){
                        
                        try_wilcox <- try(wilcox.test(form, data = df))
                        
                        if (length(try_wilcox) > 1 & is.finite(wilcox.test(form, data= df)$p.value)){
                            p <- wilcox.test(form, data= df)$p.value
                            testlabs[k] <- "Wilcoxon rank-sum"
                            if (paired == TRUE){
                                p <- wilcox.test(v1,v2, paired = TRUE)$p.value
                                testlabs[k] <- "Wilcoxon signed-rank"
                            }
                        }
                        
                        if (length(try_wilcox) == 1 | !is.finite(wilcox.test(form, data= df)$p.value)){
                            p <- NA
                            testlabs[k] <- NA
                        }

                    } 
                
                    tmp[1,(3+nlevels(df[,by]))] <- sprintf(pvalf, round(p, pval.dec))
                    if (is.na(p)) {
                        tmp[1,(3+nlevels(df[,by]))] <- "--"
                        p <- 99
                    }
                        if (pval.dec == 4 & p < 0.0001) tmp[1,(3+nlevels(df[,by]))] <- "< 0.0001"
                        if (pval.dec == 3 & p < 0.001 ) tmp[1,(3+nlevels(df[,by]))] <- "< 0.001"
                        if (pval.dec == 2 & p < 0.01  ) tmp[1,(3+nlevels(df[,by]))] <- "< 0.01"
                        
                        if (pval.dec == 4 & round(p,4) == 1) tmp[1,(3+nlevels(df[,by]))] <- "> 0.9999"
                        if (pval.dec == 3 & round(p,3) == 1) tmp[1,(3+nlevels(df[,by]))] <- "> 0.999"
                        if (pval.dec == 2 & round(p,2) == 1) tmp[1,(3+nlevels(df[,by]))] <- "> 0.99"
                    
                    if (!is.finite(p)) tmp[1,(3+nlevels(df[,by]))] <- "--"
                    
                    tmp[1,(4+nlevels(df[,by]))] <- testlabs[k]
                }
                
                if (nlevels(df[,by]) > 2){
                    
                    if (tests[k] == "ttest" | tests[k] == "anova"){
                        
                        try_aov <- try(aov(form, data= df))
                        
                        if (length(try_aov) > 1){
                            anovamod <- aov(form, data= df)
                            p <- summary(anovamod)[[1]][[1,"Pr(>F)"]]
                            testlabs[k] <- "Anova"
                        }
                        if (length(try_aov) == 1){
                            p <- NA
                            testlabs[k] <- NA
                        }

                    }   
                    if (tests[k] == "ranksum" | tests[k] == "kw") {
                        
                        try_kw <- try(kruskal.test(form, data= df))
                        
                        if (length(try_kw) > 1 & is.finite(kruskal.test(form, data= df)$p.value)){
                            p <- kruskal.test(form, data= df)$p.value
                            testlabs[k] <- "Kruskal-Wallis"
                        }
                        if (length(try_kw) == 1 | !is.finite(kruskal.test(form, data= df)$p.value)){
                            p <- NA
                            testlabs[k] <- NA
                        }    

                    }
                    
                    tmp[1,(3+nlevels(df[,by]))] <- sprintf(pvalf, round(p, pval.dec))
                    if (is.na(p)) {
                        tmp[1,(3+nlevels(df[,by]))] <- "--"
                        p <- 99
                    }
                    if (pval.dec == 4 & p < 0.0001) tmp[1,(3+nlevels(df[,by]))] <- "< 0.0001"
                    if (pval.dec == 3 & p < 0.001 ) tmp[1,(3+nlevels(df[,by]))] <- "< 0.001"
                    if (pval.dec == 2 & p < 0.01  ) tmp[1,(3+nlevels(df[,by]))] <- "< 0.01"
                    tmp[1,(4+nlevels(df[,by]))] <- testlabs[k]
                }
            }
            if (missing == TRUE & (dispN == TRUE)){
                tmp[nrow(tmp),1] <- "* N (non-missing)"
                tmp[nrow(tmp),2] <- sum(!is.na(df[,covs[k]]))
                tmp[nrow(tmp),3:(2+nlevels(df[,by]))] <- 
                    table(!is.na(df[,covs[k]]), df[,by])["TRUE",]
            }
            if (missing == TRUE & (dispmiss == TRUE)){
                tmp[nrow(tmp),1] <- "* Freq Missing"
                tmp[nrow(tmp),2] <- sum(is.na(df[,covs[k]]))
                tmp[nrow(tmp),3:(2+nlevels(df[,by]))] <- 
                    table(is.na(df[,covs[k]]), df[,by])["TRUE",]
            }
            
            if (k %% 2 == 0) rgroup <- c(rgroup, rep("none", nrow(tmp))) 
            if (k %% 2 != 0) rgroup <- c(rgroup, rep(color, nrow(tmp))) 
            if (blanks == TRUE){
                blank <- rep(NA, ncol(tmp))
                sum_table <- rbind(sum_table, blank, tmp)
            }
            
        }
    }
    
    final_table <- data.frame(sum_table)
    names(final_table) <- c("Variable", 
                            paste("All (n = ", nrow(df), ")", sep=""),
                            paste(capitalize(levels(df[,by])), 
                                  " (n = ", table(df[,by]), ")", sep=""),
                            "p-value",
                            "Test")
    if (pvalcol != TRUE | sum(!is.na(tests)) == 0){
        final_table <- final_table[,which(names(final_table) %in% c("p-value", "Test") == FALSE)]
    }
    if (testcol != TRUE & pvalcol == TRUE){
        final_table <- final_table[,which(names(final_table) != "Test")]
    }
    if (allcol != TRUE){
        final_table <- final_table[,c(1,3:ncol(final_table))]
    } 

    if (printRMD == TRUE){
        print(xtable(final_table), type='html', include.rownames=F)
        
        return(final_table)
    }

    if (htmlTable == TRUE){
        
        final_html <- final_table
        
        ### stop htmlTable from treating everything as a factor
        for (i in 1:ncol(final_html)){
            final_html[,i] <- as.character(final_html[,i])
        }
        ### remove blanks 
            final_html <- final_html[!is.na(final_html[,1]),]
        ### get header rows
            head <- which(is.na(final_html[,2]))
        ### get non-header rows
            nohead <- which(!is.na(final_html[,2]))
        ### indent non-header rows and remove *
            final_html[nohead,"Variable"] <- paste("&nbsp; &nbsp; &nbsp;",
                                                    substring(final_html[nohead,"Variable"], 3))
        ### bold header rows   
            final_html[head,"Variable"] <- paste("<b>",
                                                final_html[head,"Variable"],
                                                "<b/>", sep="")
                
        ### create htmlTable
            if (noby == 0){
                htmlver <- htmlTable(x = final_html[,2:ncol(final_html)],
                                     rnames = final_html[,"Variable"],
                                     css.cell='border-collapse: collapse; padding: 4px;',
                                     col.rgroup=rgroup)
                print(htmlver)
                return(final_table)
            }
                    
            if (noby == 1){
                
                for (i in 1:ncol(final_html)){
                    final_html[,i] <- as.character(final_html[,i])
                }
                
                data <- data.frame(final_html[,2])
                names(data) <- paste("All (n = ", nrow(df), ")", sep="")
                
                 htmlver <- htmlTable(x = data,
                                      rnames = final_html[,"Variable"],
                                      css.cell='border-collapse: collapse; padding: 4px;',
                                      col.rgroup=rgroup)
                 print(htmlver)
                 return(final_table)
            }   
    } 
   
}
