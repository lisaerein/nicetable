#' Lisa's Summary Table Function
#'
#' This function creates a nice looking summary table.
#' It returns a dataframe and prints an html table by default for use in R markdown documents.
#' @param df Dataset, a dataframe object (REQUIRED).
#' @param covs Character vector of variable names to include in table (REQUIRED).
#' @param type Numeric vector indicating variable type for each covariate: 1 for continuous and 2 for categorical (REQUIRED).
#' @param labels Labels for covariates. Default = NA, variable names will be used.
#' @param by Variable name (character) to stratify by. Defaults to NA, no stratifying variable and no tests.
#' @param bylab Label (character) for stratification variable to use in warning statement. Default is NA.
#' @param warnmissby Indicator (logical) to warn user of missing values in stratification variable. Default = FALSE.
#' @param allcol Indicator (logical) to diplay the "All Data" column. Default = TRUE.
#' @param alllab Character label for "All" column. Default = "All (N = )".
#' @param orderfreq Indicator (logical) to order (non-factor) categorical variables by descending frequency.
#' Ordered factor variables will retain original ordering. Default = FALSE.
#' @param percent Should row or column percents be used: 2 for row and 1 for column percents. Default is row percentages (2).
#' @param perc.dec Number of decimal places for percentages (categorcal variables). Default = 1.
#' @param stats Statistics to display for continuous variables, a character vector of the following options
#' (Default = mean_sd, median_q1q3, minmax): mean, sd, median, iqr, q1, q3, q1q3, min, max, minmax, range, sem,
#' mean_sd, mean_sem, median_iqr, median_range, median_q1q3, median_minmax.
#' @param cont.dec Number of decimal placess for continuous variable summary stats (mean, median, sd, iqr, etc.). Default = 2.
#' @param dispmiss Indicator (logical) to display number of missing values. Default = TRUE.
#' @param dispN Indicator (logical) to display number of non-missing values. Default = FALSE.
#' @param tests Character vector of tests to calculate p-values. If only one is entered it will apply to all variables.
#' If NA (default), no tests will be done.
#' Parametric ("p": T-test, Chi-squared, Anova, etc.),
#' Non-Parametric ("np": Rank-sum, Fisher's exact, Kruskal-Wallis, etc.),
#' T-test ("ttest"), Wilcoxon/Mann-Whitney Rank-sum ("ranksum"),
#' Chi-squared ("chisq"), Fisher's Exact test ("fe"),
#' Anova ("anova"), and Kruskal-Wallis ("kw") are currently supported.
#' @param exact Logical indicator to use exact version of Wilcoxon rank-sum test (using coin package). Default = FALSE.
#' @param pval.dec Number of decimal places for p-values. Default = 3.
#' @param testcol Indicator (logical) to display column with statistical test names. Default = TRUE.
#' @param pvalcol Indicator (logical) to display column with p-values. Default = TRUE.
#' @param mingroup Minimum non-missing group size required to report p-value (0 by default to report all p-values).
#' @param mincell Minimum non-missing cell size required to report p-value (0 by default to report all p-values).
#' @param paired Indicator (logical) to use a test for paired data (only available for ttest and ranksum). Default = FALSE.
#' @param altp Numeric vector for alternative p-values to use (default is NA, use regular p-values).
#' @param kable Indicator (logical) to use kable to display table. Default = TRUE.
#' @param htmlTable Indicator (logical) to use htmlTable package to display table instead of kable Default = FALSE.
#' @param use_flextable Indicator (logical) to use flextable package to display table. Default = FALSE.
#' @param htmltitle Character label for htmlTable variable names column. Default = " ".
#' @param caption Character title for htmlTable or kable table. Default = " ".
#' @param color Character Hex color to use for htmlTable striping. Default = "#EEEEEE" (light grey).
#' @param byref Indicator (logical) to include reference "by" category column be included (default = TRUE).
#' @keywords summary table consulting Lisa
#' @importFrom knitr kable
#' @importFrom htmlTable htmlTable
#' @importFrom coin wilcox_test wilcoxsign_test pvalue
#' @importFrom MASS polr
#' @importFrom clinfun jonckheere.test
#' @importFrom multiCA multiCA.test
#' @import flextable
#' @export
nicetable <- function(df
                      ### REQUIRED inputs
                      ,covs
                      ,type

                      ### covariate labels
                      ,labels = NA

                      ### stratification specifications
                      ,by = NA
                      ,bylab = NA
                      ,warnmissby = FALSE
                      ,allcol = TRUE
                      ,alllab = NA

                      ### categorical covariate options
                      ,orderfreq = FALSE
                      ,percent = 2
                      ,perc.dec = 1

                      ### continuous covariate options
                      ,stats = c("mean_sd", "median_q1q3", "minmax")
                      ,cont.dec = 2

                      ### missing data reporting options
                      ,dispmiss = TRUE
                      ,dispN = FALSE

                      ### statistical tests and options
                      ,tests = NA
                      ,exact = FALSE
                      ,pval.dec = 3
                      ,testcol = TRUE
                      ,pvalcol = TRUE
		              ,mingroup = 0
		              ,mincell = 0
		              ,paired = FALSE
		              ,altp = NA

		              ### table formatting and options
		              ,kable = TRUE
		              ,htmlTable = FALSE
		              ,use_flextable = FALSE
		              ,htmltitle = ""
		              ,caption = ""
                      ,color = "#EEEEEE"
		              ,byref = TRUE
		              ){

    # check required user inputs ---------------------------------------

    try(if (class(df[1]) != "data.frame") stop("df must be a dataframe\n"))

    ### if only one variable type is entered, repeat for all variables
    if (length(type) == 1) {
        type <- rep(type, length(covs))
    }

    ## remove any covs that do not appear in dataset
    covs2 <- covs[covs %in% names(df)]
    if (length(covs2) != length(covs)) cat("Warning! Covariate(s) do not exist:", covs[!(covs %in% names(df))],"\n")
    covs <- covs2
    type <- type[covs %in% names(df)]
    try(if (length(covs) == 0) stop("No valid covs\n"))

    ## check that all types are valid
    if (length(type[!(type %in% c(1,2))]) > 0) cat("Warning! Invalid type for covariate(s): ", covs[!(type %in% c(1,2))], "\n")

    ## check that by variable appears in dataset
    if (!is.na(by)){
        by2 <- by[by %in% names(df)]
        try(if (length(by2) != 1) stop("Grouping variable: ", by[!(by %in% names(df))]," does not exist in dataset\n"))
        by <- by2
    }

    simpleCap <- function(x) {
      s <- strsplit(x, " ")[[1]]
      paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
    }

    if (sum(!is.na(altp)) > 0){
        if (length(altp) != length(covs)) altp <- NA
        if (length(altp) == length(covs)){
            testcol <- FALSE
            tests <- "p"
        }

    }

    if (is.na(by)){
        byref = TRUE
    }

    blanks <- TRUE

    htmlcaption <- caption

    if (is.na(bylab) & !is.na(by)) bylab <- by

    noby <- 0
    noby[is.na(by)] <- 1

    if (is.na(by)){
        df$Allcol <- "All"
        if (!is.na(alllab)) df$Allcol <- alllab
        by <- "Allcol"
        allcol <- FALSE
    }

    ### if data is paired, do not print a column for combined data
    if (paired == TRUE){
        allcol <- FALSE
    }

    pvalf <- paste("%.", pval.dec, "f", sep="")
    percf <- paste("%.", perc.dec, "f", sep="")
    contf <- paste("%.", cont.dec, "f", sep="")

    ### define functions for continuous summary stats

    sem <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
    nice_sem    <- function(x) sprintf(contf, round(sem(x), cont.dec))
    nice_mean   <- function(x) sprintf(contf, round(  mean(x, na.rm=TRUE), cont.dec))
    nice_sd     <- function(x) sprintf(contf, round(    sd(x, na.rm=TRUE), cont.dec))
    nice_median <- function(x) sprintf(contf, round(median(x, na.rm=TRUE), cont.dec))
    nice_iqr    <- function(x) sprintf(contf, round(   IQR(x, na.rm=TRUE), cont.dec))
    nice_min    <- function(x) sprintf(contf, round(   min(x, na.rm=TRUE), cont.dec))
    nice_max    <- function(x) sprintf(contf, round(   max(x, na.rm=TRUE), cont.dec))
    nice_q1     <- function(x) sprintf(contf, round(quantile(x, c(0.25), na.rm=T), cont.dec))
    nice_q3     <- function(x) sprintf(contf, round(quantile(x, c(0.75), na.rm=T), cont.dec))
    nice_range  <- function(x) sprintf(contf, round(diff(range(x, na.rm=TRUE)), cont.dec))

    nice_minmax <- function(x) paste(nice_min(x), ", ", nice_max(x), sep="")
    nice_q1q3   <- function(x) paste(nice_q1(x), ", ",  nice_q3(x), sep="")

    nice_mean_sd  <- function(x) paste(nice_mean(x), " (",  nice_sd(x), ")", sep="")
    nice_mean_sem <- function(x) paste(nice_mean(x), " (", nice_sem(x), ")", sep="")
    nice_median_iqr    <- function(x) paste(nice_median(x), " (",   nice_iqr(x), ")", sep="")
    nice_median_range  <- function(x) paste(nice_median(x), " (", nice_range(x), ")", sep="")
    nice_median_q1q3   <- function(x) paste(nice_median(x), " [", nice_q1q3(x) , "]", sep="")
    nice_median_minmax <- function(x) paste(nice_median(x), " [", nice_minmax(x),"]", sep="")

    nicelab <- data.frame("nicefun" = c("nice_mean",
                                        "nice_sd",
                                        "nice_sem",
                                        "nice_median",
                                        "nice_iqr",
                                        "nice_q1",
                                        "nice_q3",
                                        "nice_q1q3",
                                        "nice_min",
                                        "nice_max",
                                        "nice_minmax",
                                        "nice_range",
                                        "nice_mean_sd",
                                        "nice_median_range",
                                        "nice_median_iqr",
                                        "nice_median_q1q3",
                                        "nice_median_minmax"),
                          "label" = c("* Mean",
                                      "* SD",
                                      "* SEM",
                                      "* Median",
                                      "* IQR",
                                      "* Q1",
                                      "* Q3",
                                      "* Q1, Q3",
                                      "* Min",
                                      "* Max",
                                      "* Min, Max",
                                      "* Range",
                                      "* Mean (SD)",
                                      "* Median (Range)",
                                      "* Median (IQR)",
                                      "* Median [Q1, Q3]",
                                      "* Median [Min, Max]"),
                          stringsAsFactors = FALSE)
    rownames(nicelab) <- nicelab$nicefun
    nicelab$fun <- unlist(lapply(nicelab[,"nicefun"], function(x) strsplit(x, "nice_")[[1]][2]))

    ### Use same stats for all continuous variables
    ### Trouble-shoot user error by removing bad values
    ### For categorical variables, stats = "NA"
    if (stats[1] == "mean_sd_median_range") stats <- c("mean_sd", "median_minmax") ## for backwards compatability
    stats <- tolower(stats)
    stats <- unique(stats)
    stats <- stats[stats %in% nicelab$fun]
    stats <- rep(list(stats), length(covs))
    stats[which(type == 2)] <- "NA"

    ### Trouble-shoot user entered tests
    tests <- tolower(tests)
    if (length(tests) == 1){
        tests <- rep(tests, length(covs))
    }
    if (length(percent) == 1){
        percent <- rep(percent, length(covs))
    }

    if (!is.na(tests[1]) & tests[1] == "p"){
        tests[type == 1] <- "ttest"
        tests[type == 2] <- "chisq"
    }

    if (!is.na(tests[1]) & tests[1] == "np"){
        tests[type == 1] <- "ranksum"
        tests[type == 2] <- "fe"
    }

    testlabs <- tests

    ### if labels are NA, use variable names
    if (length(labels) == 1){
        labels <- rep(labels, length(covs))
    }
    labels[is.na(labels)] <- covs[is.na(labels)]

    ### if tests is blank do not calculate a p-value
    if (length(tests) == 1){
        tests <- rep(tests, length(covs))
    }
    pval <- rep(TRUE, length(covs))
    pval[is.na(tests)] <- FALSE

    ### treat the by variables as a factor if not already
    ### subset dataframe for non missing 'by' values
    df[,by] <- as.factor(df[,by])
    total_levels <- 0
    if (sum(is.na(df[,by])) > 0){
        if (warnmissby == TRUE){
            if (sum(is.na(df[,by])) ==1){
              cat("* Note there was 1 entry missing", bylab, "which was excluded from the table. \n\n")
            }
            if (sum(is.na(df[,by])) > 1){
              cat("* Note there were", sum(is.na(df[,by])), "entries missing", bylab, "which were excluded from the table. \n\n")
            }
        }
    }
    df <- df[!is.na(df[,by]),]

    ## save the number of rows per variable to use for table striping
    rgroup <- NULL

    ## placeholder for final table
    sum_table <- NULL

    for (k in 1:length(covs)){

        p <- NA

        if (type[k] == 2){

            ### treat categorical covariates as factors
            ### if orderfreq = TRUE then reorder all unordered factor levels by descending frequency
            ### if covariate is already a factor keep the original ordering
            if (is.factor(df[,covs[k]]) == FALSE){
                df[,covs[k]] <- factor(df[,covs[k]])
                if (orderfreq == TRUE) {
                    tb <- table(df[,covs[k]])
                    df[,covs[k]] <- factor(df[,covs[k]],
                                           levels = names(tb[order(tb, decreasing = TRUE)]))
                }
            }
            ### create a mini table for this variable alone
            ### the number of columns is the number of groups
            ### + 1 names column + 1 pvalue column + 1 test name column
            ### the number of rows is the number of levels + 1 for varname/label
            ### if there are missing values add another row for missing totals
                tmp <- matrix(data = NA,
                              ncol = nlevels(df[,by]) + 4,
                              nrow = 1 + nlevels(df[,covs[k]]))

            ### add first column variable label and names of levels
            tmp[1,1] <- labels[k]
            tmp[2:(1+nlevels(df[,covs[k]])),1] <- paste("* ",levels(df[,covs[k]]), "\n", sep="")

            ### create temporary dataset with NA's removed
            df.complete <- df[!is.na(df[,covs[k]]),]

            ### if group size (NA's removed) is less than min, do not report p-values
            mingroup_tab <- as.matrix(table(df.complete[,by]))
            if (sum(mingroup_tab < mingroup, na.rm=T) > 0) {
              pval[k] <- TRUE
              tests[k] <- "--"
              testlabs[k] <- "--"
            }
            ### if cell size (NA's removed) is less than min, do not report p-values
            mincell_tab <- as.matrix(table(df.complete[,covs[k]], df.complete[,by]))
            if (sum(mincell_tab < mincell, na.rm=T) > 0) {
              pval[k] <- TRUE
              tests[k] <- "--"
              testlabs[k] <- "--"
            }

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

            if (dispN) {
                tmp <- rbind(tmp, rep(NA, ncol(tmp)))
                tmp[nrow(tmp), 1] <- "* N (non-missing)"

                ### calculate N for "All column"
                tmp[nrow(tmp), 2] <- sum(!is.na(df[,covs[k]]))

                ### calculate N by subgroup
                for (l in 1:nlevels(df[,by])) {
                    tmp[nrow(tmp), (2 + l)] <- sum(!is.na(df[df[,by] == levels(df[,by])[l] ,covs[k]]))
                }
            }

            if (dispmiss) {
                tmp <- rbind(tmp, rep(NA, ncol(tmp)))
                tmp[nrow(tmp), 1] <- "* Freq Missing"

                ### calculate missing for "All column"
                tmp[nrow(tmp), 2] <- sum(is.na(df[,covs[k]]))

                ### calculate missing by subgroup
                for (l in 1:nlevels(df[,by])) {
                    tmp[nrow(tmp), (2 + l)] <- sum(is.na(df[df[,by] == levels(df[,by])[l] ,covs[k]]))
                }
            }

            if (pval[k] == TRUE){

                freq <- table(df.complete[,covs[k]], df.complete[,by])

                if (tests[k] == "fe" & (nrow(freq) > 1)) {
                    try_fe <- try(fisher.test(freq), silent = TRUE)
                    if (length(try_fe) >  1) {
                        p <- fisher.test(freq)$p.value
                        testlabs[k] <- "Fisher's exact"
                    }
                    ### if fisher's exact test will not run try chisq
                    if (length(try_fe) == 1) tests[k] <- "chisq"
                }
                if (tests[k] == "chisq" & (nrow(freq) > 1)) {
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

                if (tests[k] %in% c("ranksum","kw")){
                  if (noby==0 & nlevels(df[,by]) == 2){
                    form <- as.formula(paste("as.numeric(", covs[k], ") ~", by, sep=""))
                    try_wilcox <- try(wilcox.test(form, data = df))

                    if (length(try_wilcox) > 1){
                      if (is.finite(wilcox.test(form, data= df)$p.value)){
                        p <- wilcox.test(form, data= df)$p.value
                        testlabs[k] <- "Wilcoxon rank-sum"
                        if (exact) {
                            p <- pvalue(wilcox_test(form, data = df, distribution = "exact"))
                            testlabs[k] <- "Wilcoxon rank-sum (Ex)"
                        }

                      }
                      if (!is.finite(wilcox.test(form, data= df)$p.value)){
                        p <- NA
                        testlabs[k] <- NA
                      }
                    }
                    if (length(try_wilcox) == 1){
                      p <- NA
                      testlabs[k] <- NA
                    }
                  }
                  if (noby==0 & nlevels(df[,by]) > 2){
                    form <- as.formula(paste("as.numeric(", covs[k], ") ~", by, sep=""))
                    try_kruskal <- try(kruskal.test(form, data = df))

                    if (length(try_kruskal) > 1){
                      if (is.finite(kruskal.test(form, data=df)$p.value)){
                        p <- kruskal.test(form, data=df)$p.value
                        testlabs[k] <- "Kruskal-Wallis"
                      }
                      if (!is.finite(kruskal.test(form, data= df)$p.value)){
                        p <- NA
                        testlabs[k] <- NA
                      }
                    }
                    if (length(try_kruskal) == 1){
                      p <- NA
                      testlabs[k] <- NA
                    }
                  }
                  if (noby==1){
                    try_wilcox <- try(wilcox.test(as.numeric(df[,covs[k]]),
                                                  mu = (nlevels(df[,covs[k]])+1)/2))

                    if (length(try_wilcox) > 1 &
                        is.finite(wilcox.test(as.numeric(df[,covs[k]]),
                                              mu = (nlevels(df[,covs[k]])+1)/2))){
                      p <- wilcox.test(as.numeric(df[,covs[k]]),
                                       mu = (nlevels(df[,covs[k]])+1)/2)$p.value
                      testlabs[k] <- "Wilcoxon signed-rank"
                      # print(p)
                    }
                    if (length(try_wilcox) == 1 |
                        !is.finite(wilcox.test(as.numeric(df[,covs[k]]),
                                               mu = (nlevels(df[,covs[k]])+1)/2)$p.value)){
                      p <- NA
                      testlabs[k] <- NA
                    }
                  }
                }

                if (tests[k] == "jt"){
                  ## create numeric versions of covariate by by variable
                  numcovk <- as.numeric(df[,covs[k]])
                  numby <- as.numeric(df[,by])

                  # form <- as.formula(paste("as.numeric(", covs[k], ") ~", by, sep=""))
                  # try_jt <- try(jt.test(form, data = df))

                  try_jt <- try(jonckheere.test(x = numcovk, g = numby))

                  p <- NA
                  testlabs[k] <- NA
                  if ((length(try_jt) > 1)) {
                    jtres <- jonckheere.test(x = numcovk, g = numby)
                    if (is.finite(jtres$p.value)) {
                      p <- jtres$p.value
                      testlabs[k] <- "Jonckheere-Terpstra trend"
                    }
                  }
                  # kSamples package function jt.test
                  # if (length(try_jt) > 1 & is.finite(jt.test(form, data= df)[[6]][4])){
                  #   p <- jt.test(form, data= df)[[6]][4]
                  #   testlabs[k] <- "Jonckheere-Terpstra trend"
                  # }
                  #
                  # if (length(try_jt) == 1 | !is.finite(jt.test(form, data= df)[[6]][4])){
                  #   p <- NA
                  #   testlabs[k] <- NA
                  # }
                }

                if (tests[k] == "multica"){
                  form <- as.formula(paste(covs[k], "~", by, sep=""))
                  try_mca <- try(multiCA.test(form, data = df))

                  if (length(try_mca) > 1 &
                      is.finite(as.numeric(multiCA.test(form, data= df)[[1]][3]))){
                    p <- as.numeric(multiCA.test(form, data= df)[[1]][3])
                    testlabs[k] <- "Cochran-Armitage trend"
                  }

                  if (length(try_mca) == 1 |
                      !is.finite(as.numeric(multiCA.test(form, data= df)[[1]][3]))){
                    p <- NA
                    testlabs[k] <- NA
                  }
                }
                ### if there is only one level of the covariate do not do any tests
                if (nlevels(df[,covs[k]]) < 2){
                    p <- NA
                    testlabs[k] <- NA
                }

                if (sum(!is.na(altp)) > 0) p <- altp[k]
                tmp[1,(3+nlevels(df[,by]))] <- sprintf(pvalf, round(p, pval.dec))
                if (is.na(p)) {
                    tmp[1,(3+nlevels(df[,by]))] <- "--"
                    p <- 99
                }
                if (tests[k] == "--") {
                  tmp[1,(3+nlevels(df[,by]))] <- "--"
                  testlabs[k] <- "--"
                  p <- 99
                }

                if (htmlTable){
                  if (pval.dec == 4 & p < 0.0001) tmp[1,(3+nlevels(df[,by]))] <- "&lt; 0.0001"
                  if (pval.dec == 3 & p < 0.001 ) tmp[1,(3+nlevels(df[,by]))] <- "&lt; 0.001"
                  if (pval.dec == 2 & p < 0.01  ) tmp[1,(3+nlevels(df[,by]))] <- "&lt; 0.01"
                  if (pval.dec == 4 & round(p,4) == 1) tmp[1,(3+nlevels(df[,by]))] <- "&gt; 0.9999"
                  if (pval.dec == 3 & round(p,3) == 1) tmp[1,(3+nlevels(df[,by]))] <- "&gt; 0.999"
                  if (pval.dec == 2 & round(p,2) == 1) tmp[1,(3+nlevels(df[,by]))] <- "&gt; 0.99"
                }
                if (!htmlTable){
                  if (pval.dec == 4 & p < 0.0001) tmp[1,(3+nlevels(df[,by]))] <- "< 0.0001"
                  if (pval.dec == 3 & p < 0.001 ) tmp[1,(3+nlevels(df[,by]))] <- "< 0.001"
                  if (pval.dec == 2 & p < 0.01  ) tmp[1,(3+nlevels(df[,by]))] <- "< 0.01"
                  if (pval.dec == 4 & round(p,4) == 1) tmp[1,(3+nlevels(df[,by]))] <- "> 0.9999"
                  if (pval.dec == 3 & round(p,3) == 1) tmp[1,(3+nlevels(df[,by]))] <- "> 0.999"
                  if (pval.dec == 2 & round(p,2) == 1) tmp[1,(3+nlevels(df[,by]))] <- "> 0.99"
                }

                tmp[1,(4+nlevels(df[,by]))] <- testlabs[k]
            }

            if (k %% 2 == 0) rgroup <- c(rgroup, rep("none", nrow(tmp)))
            if (k %% 2 != 0) rgroup <- c(rgroup, rep(color,  nrow(tmp)))
            if (blanks == TRUE){
                blank <- rep(NA, ncol(tmp))
                sum_table <- rbind(sum_table, blank, tmp)
            }

        }

        if (type[k] == 1){

            ### create temporary dataset with NA's removed
            df.complete <- df[!is.na(df[,covs[k]]),]

            ### if group size (NA's removed) is less than min, do not report p-values
            mingroup_tab <- as.matrix(table(df.complete[,by]))
            if (sum(mingroup_tab < mingroup, na.rm=T) > 0) {
              pval[k] <- TRUE
              tests[k] <- "--"
              testlabs[k] <- "--"
            }

            ### create a mini table for this variable alone
            ### the number of columns is the number of groups + 4
            ### (1 all column + 1 names column + 1 pvalue column + 1 test name column)
            ### the number of rows is the number of stats requested

            tmp <- matrix(data = NA,
                          ncol = nlevels(df[,by]) + 4,
                          nrow = 1 + (length(stats[[k]])))

            tmp[1,1] <- paste(labels[k], sep=" ")

            for (s in 1:length(stats[[k]])){
                ### get corresponding function and label
                nicefun <- paste("nice", stats[[k]][s], sep="_")
                tmp[(1 + s), 1] <- nicelab[nicefun, "label"]

                ### calculate stats for "All column"
                tmp[(1 + s), 2] <- do.call(nicefun, args = list("x" = df[,covs[k]]))
                tmp[(1 + s), 2] <- gsub("NA|Inf|-Inf|NaN", "--", tmp[(1 + s), 2])

                ### calculate stats for subgroups
                for (l in 1:nlevels(df[,by])){
                    tmp[(1 + s), (2 + l)] <- do.call(nicefun, args = list("x" = df[df[,by] == levels(df[,by])[l] ,covs[k]]))
                    tmp[(1 + s), (2 + l)] <- gsub("NA|Inf|-Inf|NaN", "--", tmp[(1 + s), (2 + l)])
                }
            }

            if (dispN) {
                tmp <- rbind(tmp, rep(NA, ncol(tmp)))
                tmp[nrow(tmp), 1] <- "* N (non-missing)"

                ### calculate missing for "All column"
                tmp[nrow(tmp), 2] <- sum(!is.na(df[,covs[k]]))

                ### calculate missing by subgroups
                for (l in 1:nlevels(df[,by])) {
                    tmp[nrow(tmp), (2 + l)] <- sum(!is.na(df[df[,by] == levels(df[,by])[l] ,covs[k]]))
                }
            }

            if (dispmiss) {
                tmp <- rbind(tmp, rep(NA, ncol(tmp)))
                tmp[nrow(tmp), 1] <- "* Freq Missing"

                ### calculate missing for "All column"
                tmp[nrow(tmp), 2] <- sum(is.na(df[,covs[k]]))

                ### calculate missing by subgroups
                for (l in 1:nlevels(df[,by])) {
                    tmp[nrow(tmp), (2 + l)] <- sum(is.na(df[df[,by] == levels(df[,by])[l] ,covs[k]]))
                }
            }


            if (pval[k] == TRUE){

                if (noby == 1){
                  if (tests[k] == "ranksum"){

                    tryok <- FALSE
                    if (length(try_wilcox <- try(wilcox.test(as.numeric(df[,covs[k]])), silent = F)) > 1) {
                        wilcox <- try_wilcox
                        tryok <- TRUE
                    }
                    if (tryok & is.finite(wilcox$p.value)){
                      p <- wilcox$p.value
                      testlabs[k] <- "Wilcoxon signed-rank"
                    }
                    if (!tryok | !is.finite(wilcox$p.value)){
                      p <- NA
                      testlabs[k] <- NA
                    }
                  }
                  if (tests[k] == "ttest"){

                        tryok <- FALSE
                        if (length(try_ttest <- try(t.test(as.numeric(df[,covs[k]])), silent = F)) > 1){
                            ttest <- try_ttest
                            tryok <- TRUE
                        }
                        if (tryok & is.finite(ttest$p.value)){
                            p <- ttest$p.value
                            testlabs[k] <- "T-test"
                        }
                        if (!tryok | !is.finite(ttest$p.value)){
                            p <- NA
                            testlabs[k] <- NA
                        }
                    }
                }

                form <- as.formula(paste(covs[k], "~", by))

                if (noby == 0 & nlevels(df[,by]) == 2){

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

                        if (length(try_wilcox) > 1 & is.finite(wilcox.test(form, data=df)$p.value)){
                            p <- wilcox.test(form, data=df)$p.value
                            testlabs[k] <- "Wilcoxon rank-sum"
                            if (exact) {
                                p <- pvalue(wilcox_test(form, data=df, distribution="exact"))
                                testlabs[k] <- "Wilcoxon rank-sum (Ex)"
                            }

                            if (paired == TRUE){
                                p <- wilcox.test(v1, v2, paired = TRUE)$p.value
                                testlabs[k] <- "Wilcoxon signed-rank"
                                if (exact){
                                    p <- pvalue(wilcoxsign_test(v1 ~ v2, distribution="exact"))
                                    testlabs[k] <- "Wilcoxon signed-rank (Ex)"
                                }

                            }
                        }
                        if (length(try_wilcox) == 1 | !is.finite(wilcox.test(form, data=df)$p.value)){
                            p <- NA
                            testlabs[k] <- NA
                        }
                    }
                }

                if (noby == 0 & nlevels(df[,by]) > 2){

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

                        try_kw <- try(kruskal.test(form, data=df))

                        if (length(try_kw) > 1 & is.finite(kruskal.test(form, data=df)$p.value)){
                            p <- kruskal.test(form, data=df)$p.value
                            testlabs[k] <- "Kruskal-Wallis"
                        }
                        if (length(try_kw) == 1 | !is.finite(kruskal.test(form, data=df)$p.value)){
                            p <- NA
                            testlabs[k] <- NA
                        }

                    }
                    if (tests[k] == "jt"){
                      form <- as.formula(paste(covs[k], " ~ ", by, sep=""))
                      try_jt <- try(jt.test(form, data = df))

                      if (length(try_jt) > 1 & is.finite(jt.test(form, data= df)[[6]][4])){
                        p <- jt.test(form, data= df)[[6]][4]
                        testlabs[k] <- "Jonckheere-Terpstra trend"
                      }

                      if (length(try_jt) == 1 | !is.finite(jt.test(form, data= df)[[6]][4])){
                        p <- NA
                        testlabs[k] <- NA
                      }
                    }
                }

                if (sum(!is.na(altp)) > 0) p <- altp[k]
                tmp[1,(3+nlevels(df[,by]))] <- sprintf(pvalf, round(p, pval.dec))
                if (is.na(p)) {
                    tmp[1,(3+nlevels(df[,by]))] <- "--"
                    p <- 99
                }
                if (tests[k] == "--") {
                    tmp[1,(3+nlevels(df[,by]))] <- "--"
                    testlabs[k] <- "--"
                    p <- 99
                }
                if (htmlTable){
                    if (pval.dec == 4 & p < 0.0001) tmp[1,(3+nlevels(df[,by]))] <- "&lt; 0.0001"
                    if (pval.dec == 3 & p < 0.001 ) tmp[1,(3+nlevels(df[,by]))] <- "&lt; 0.001"
                    if (pval.dec == 2 & p < 0.01  ) tmp[1,(3+nlevels(df[,by]))] <- "&lt; 0.01"
                    if (pval.dec == 4 & round(p,4) == 1) tmp[1,(3+nlevels(df[,by]))] <- "&gt; 0.9999"
                    if (pval.dec == 3 & round(p,3) == 1) tmp[1,(3+nlevels(df[,by]))] <- "&gt; 0.999"
                    if (pval.dec == 2 & round(p,2) == 1) tmp[1,(3+nlevels(df[,by]))] <- "&gt; 0.99"
                }
                if (!htmlTable){
                    if (pval.dec == 4 & p < 0.0001) tmp[1,(3+nlevels(df[,by]))] <- "< 0.0001"
                    if (pval.dec == 3 & p < 0.001 ) tmp[1,(3+nlevels(df[,by]))] <- "< 0.001"
                    if (pval.dec == 2 & p < 0.01  ) tmp[1,(3+nlevels(df[,by]))] <- "< 0.01"
                    if (pval.dec == 4 & round(p,4) == 1) tmp[1,(3+nlevels(df[,by]))] <- "> 0.9999"
                    if (pval.dec == 3 & round(p,3) == 1) tmp[1,(3+nlevels(df[,by]))] <- "> 0.999"
                    if (pval.dec == 2 & round(p,2) == 1) tmp[1,(3+nlevels(df[,by]))] <- "> 0.99"
                }
                if (!is.finite(p)) tmp[1,(3+nlevels(df[,by]))] <- "--"
                tmp[1,(4+nlevels(df[,by]))] <- testlabs[k]
            }

            if (k %% 2 == 0) rgroup <- c(rgroup, rep("none", nrow(tmp)))
            if (k %% 2 != 0) rgroup <- c(rgroup, rep(color, nrow(tmp)))
            if (blanks == TRUE){
                blank <- rep(NA, ncol(tmp))
                sum_table <- rbind(sum_table, blank, tmp)
            }

        }
    }

    if (!is.na(alllab)) all <- alllab
    if ( is.na(alllab)) all <- "All"

    final_table <- data.frame(sum_table, row.names = make.names(sum_table[,1], unique = T))

    ## header text
    head_txt <- c("",
                  all,
                  paste(unlist(lapply(levels(df[,by]), simpleCap))),
                  "",
                  "")

    ## header sample sizes
    head_nms <- c("Variable",
                  paste(" (N = ", nrow(df), ")", sep=""),
                  paste(" (N = ", table(df[,by]), ")", sep=""),
                  "p-value",
                  "Test")

    # print(final_table)

    # grab only user requested columns
    reqcols_n <- 1:ncol(final_table)

    pvalcol_n <- which(head_nms == "p-value")
    testcol_n <- which(head_nms == "Test")
    allcol_n <- min(which(head_txt == all))
    varcol_n <- which(head_nms == "Variable")
    othcol_n <- reqcols_n[!(reqcols_n %in% c(pvalcol_n, testcol_n, allcol_n, varcol_n))]

    if (!pvalcol)                reqcols_n <- reqcols_n[!(reqcols_n %in% c(pvalcol_n))]
    if (sum(!is.na(tests)) == 0) reqcols_n <- reqcols_n[!(reqcols_n %in% c(pvalcol_n, testcol_n))]
    if (!testcol)                reqcols_n <- reqcols_n[!(reqcols_n %in% c(testcol_n))]

    ## stratification but no 'all' column
    if (!allcol & noby == 0) reqcols_n <- reqcols_n[!(reqcols_n %in% allcol_n)]

    ## stratification but no 'reference' column
    if (noby == 0 & !byref) reqcols_n <- reqcols_n[!(reqcols_n %in% min(othcol_n))]

    ## no by-group stratification:
    if (noby == 1) reqcols_n <- reqcols_n[reqcols_n %in% c(varcol_n, allcol_n, pvalcol_n, testcol_n)]

    head_txt <- head_txt[reqcols_n]
    head_nms <- head_nms[reqcols_n]
    final_table <- final_table[,reqcols_n, drop=FALSE]

    if (htmlTable & kable) htmlTable <- FALSE

    if (htmlTable){

        final_html <- final_table
        names(final_html) <- paste(head_txt, head_nms, sep="")

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

            if (sum(head_nms %in% c("p-value", "Test")) == 0){
                cgroup <- c(all, unlist(lapply(levels(df[,by]), simpleCap)))
                if (allcol == FALSE) cgroup <- cgroup[2:length(cgroup)]
                n.cgroup <- rep(1, length(cgroup))
            }
            if (sum(head_nms %in% c("p-value", "Test") > 0)){
                cgroup <- c(all, unlist(lapply(levels(df[,by]), simpleCap)), "")
                if (allcol == FALSE) cgroup <- cgroup[2:length(cgroup)]
                n.cgroup <- c(rep(1, length(cgroup)-1), sum(head_nms %in% c("p-value", "Test")))
            }

        ### create htmlTable
            htmlver <- htmlTable(x = final_html[,2:ncol(final_html)],
                                 header = head_nms[2:length(head_nms)],
                                 rnames = final_html[,"Variable"],
                                 rowlabel = htmltitle,
                                 caption = htmlcaption,
                                 escape.html = F,
                                 cgroup = cgroup,
                                 n.cgroup = n.cgroup,
                                 css.cell='border-collapse: collapse; padding: 4px;',
                                 col.rgroup=rgroup)

            print(htmlver)
            return(final_html)
    }

    if (kable){

            final_kable <- final_table
            names(final_kable) <- paste(head_txt, head_nms, sep="")

            for (i in 1:ncol(final_kable)){
                final_kable[,i] <- as.character(final_kable[,i])
            }

            final_kable <- final_kable[!is.na(final_kable[,1]),]

            ### get header rows
            head <- which(is.na(final_kable[,2]))
            ### get non-header rows
            nohead <- which(!is.na(final_kable[,2]))
            ### indent non-header rows and remove *
            final_kable[nohead,"Variable"] <- paste("&nbsp; &nbsp; &nbsp;",
                                                   substring(final_kable[nohead,"Variable"], 3))
            final_kable[nohead,"Variable"] <- gsub("\n", "", final_kable[nohead,"Variable"],fixed = T)

            ### bold header rows
            final_kable[head,"Variable"] <- paste("<b>",
                                                 final_kable[head,"Variable"],
                                                 "<b/>", sep="")

            tabalign <- rep("c", ncol(final_kable))
            tabalign[1] <- "l"
            if ("p-value" %in% names(final_kable)) tabalign[which(names(final_kable) == "p-value")] <- "r"

            names(final_kable) <- gsub(")\\.1$", ")", names(final_kable))

            tabalign <- paste(tabalign, collapse="")

            print(kable(x = final_kable
                        ,row.names = FALSE
                        ,caption = htmlcaption
                        ,align = tabalign
                        ,format = "markdown"
                        )
                  )

            return(final_kable)
    }

    fillnas <- function(S) {
        L <- !is.na(S)
        c(S[L][1], S[L])[cumsum(L)+1]
    }

    if (use_flextable){

        final_flex <- final_table

        for (i in 1:ncol(final_flex)){
            final_flex[,i] <- as.character(final_flex[,i])
        }

        final_flex <- final_flex[!is.na(final_flex[,1]),]

        ### reformat grouped data for flextable
        final_flex$flexgroup <- NA
        final_flex$flexgroup[is.na(final_flex[,2])] <- final_flex[is.na(final_flex[,2]),1]
        final_flex$flexgroup <- fillnas(final_flex$flexgroup)

        if ("Test" %in% head_nms){
            flex_testcol <- which(head_nms == "Test")
            final_flex[,flex_testcol] <- fillnas(final_flex[,flex_testcol])
        }
        if ("p-value" %in% head_nms){
            flex_pvalcol <- which(head_nms == "p-value")
            final_flex[,flex_pvalcol] <- fillnas(final_flex[,flex_pvalcol])
        }
        final_flex <- final_flex[!is.na(final_flex[,2]),]
        if ("p-value" %in% head_nms) final_flex[duplicated(final_flex$flexgroup, final_flex[,flex_pvalcol]), flex_pvalcol] <- NA
        if ("Test" %in% head_nms) final_flex[duplicated(final_flex$flexgroup, final_flex[,flex_testcol]), flex_testcol] <- NA

        ### indent non-header rows and remove *
        final_flex[!is.na(final_flex[,2]),1] <- substring(final_flex[!is.na(final_flex[,2]),1], 3)
        final_flex[!is.na(final_flex[,2]),1] <- gsub("\n", "", final_flex[!is.na(final_flex[,2]),1], fixed = T)

        final_flex_gr <- as_grouped_data(final_flex, groups = "flexgroup")

        head_nms_flex <- c("flexgroup", head_nms)
        head_nms_flex[1:2] <- c("", "")
        names(head_nms_flex) <- names(final_flex_gr)

        head_txt_flex <- c("flexgroup", head_txt)
        head_txt_flex[1:2] <- c("", "")
        names(head_txt_flex) <- names(final_flex_gr)

        if ("Test" %in% head_nms){
            flex_testcol <- which(head_nms_flex == "Test")
            final_flex_gr[,flex_testcol] <- c(final_flex_gr[,flex_testcol][-1], NA)
        }
        if ("p-value" %in% head_nms){
            flex_pvalcol <- which(head_nms_flex == "p-value")
            final_flex_gr[,flex_pvalcol] <- c(final_flex_gr[,flex_pvalcol][-1], NA)
        }

        ft <- flextable(final_flex_gr)

        ## format header
        ft <- delete_part(ft, part = "header")
        ft <- add_header(x = ft
                         ,values = head_nms_flex
                         ,top = TRUE
        )
        ft <- add_header(x = ft
                         ,values = head_txt_flex
                         ,top = TRUE
        )

        ## text fonts and alignment
        ft <- theme_booktabs(ft)
        ft <- bold(ft, part = "header")
        ft <- bold(ft, j = 1, i = ~ !is.na(flexgroup), bold = TRUE, part = "body" )
        ft <- align(ft, align = "center", part = "all")
        ft <- align(ft, align = "left", j = c(1,2))
        if ("p-value" %in% head_nms) ft <- align(ft, align = "right", j = flex_pvalcol)
        if ("Test" %in% head_nms) ft <- align(ft, align = "right", j = flex_testcol)

        ft <- merge_h_range(ft, i = ~ !is.na(flexgroup), j1 = 1, j2 = 2)
        ft <- merge_h_range(ft, part = "header", j1 = 1, j2 = 2)

        knit_print(autofit(ft))

        return(list("dat" = final_flex_gr
                    ,"ft" = autofit(ft)
                    ,"head_nms" = head_nms_flex
                    ,"head_txt" = head_txt_flex
                    )
               )
    }

}
