% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nicetable.R
\name{nicetable}
\alias{nicetable}
\title{Lisa's Summary Table Function}
\usage{
nicetable(
  df,
  covs,
  type,
  labels = NA,
  by = NA,
  bylab = NA,
  warnmissby = FALSE,
  allcol = TRUE,
  alllab = NA,
  orderfreq = FALSE,
  percent = 2,
  perc.dec = 1,
  stats = c("mean_sd", "median_q1q3", "minmax"),
  cont.dec = 2,
  dispmiss = TRUE,
  dispN = FALSE,
  tests = NA,
  exact = FALSE,
  pval.dec = 3,
  testcol = TRUE,
  pvalcol = TRUE,
  mingroup = 0,
  mincell = 0,
  paired = FALSE,
  altp = NA,
  fisher.test.workspace = 2e+05,
  kable = TRUE,
  htmlTable = FALSE,
  use_flextable = FALSE,
  htmltitle = "",
  caption = "",
  color = "#EEEEEE",
  byref = TRUE
)
}
\arguments{
\item{df}{Dataset, a dataframe object (REQUIRED).}

\item{covs}{Character vector of variable names to include in table (REQUIRED).}

\item{type}{Numeric vector indicating variable type for each covariate: 1 for continuous and 2 for categorical (REQUIRED).}

\item{labels}{Labels for covariates. Default = NA, variable names will be used.}

\item{by}{Variable name (character) to stratify by. Defaults to NA, no stratifying variable and no tests.}

\item{bylab}{Label (character) for stratification variable to use in warning statement. Default is NA.}

\item{warnmissby}{Indicator (logical) to warn user of missing values in stratification variable. Default = FALSE.}

\item{allcol}{Indicator (logical) to diplay the "All Data" column. Default = TRUE.}

\item{alllab}{Character label for "All" column. Default = "All (N = )".}

\item{orderfreq}{Indicator (logical) to order (non-factor) categorical variables by descending frequency.
Ordered factor variables will retain original ordering. Default = FALSE.}

\item{percent}{Should row or column percents be used: 2 for row and 1 for column percents. Default is row percentages (2).}

\item{perc.dec}{Number of decimal places for percentages (categorcal variables). Default = 1.}

\item{stats}{Statistics to display for continuous variables, a character vector of the following options
(Default = mean_sd, median_q1q3, minmax): mean, sd, median, iqr, q1, q3, q1q3, min, max, minmax, range, sem,
mean_sd, mean_sem, median_iqr, median_range, median_q1q3, median_minmax.}

\item{cont.dec}{Number of decimal placess for continuous variable summary stats (mean, median, sd, iqr, etc.). Default = 2.}

\item{dispmiss}{Indicator (logical) to display number of missing values. Default = TRUE.}

\item{dispN}{Indicator (logical) to display number of non-missing values. Default = FALSE.}

\item{tests}{Character vector of tests to calculate p-values. If only one is entered it will apply to all variables.
If NA (default), no tests will be done.
Parametric ("p": T-test, Chi-squared, Anova, etc.),
Non-Parametric ("np": Rank-sum, Fisher's exact, Kruskal-Wallis, etc.),
T-test ("ttest"), Wilcoxon/Mann-Whitney Rank-sum ("ranksum"),
Chi-squared ("chisq"), Fisher's Exact test ("fe"),
Anova ("anova"), and Kruskal-Wallis ("kw") are currently supported.}

\item{exact}{Logical indicator to use exact version of Wilcoxon rank-sum test (using coin package). Default = FALSE.}

\item{pval.dec}{Number of decimal places for p-values. Default = 3.}

\item{testcol}{Indicator (logical) to display column with statistical test names. Default = TRUE.}

\item{pvalcol}{Indicator (logical) to display column with p-values. Default = TRUE.}

\item{mingroup}{Minimum non-missing group size required to report p-value (0 by default to report all p-values).}

\item{mincell}{Minimum non-missing cell size required to report p-value (0 by default to report all p-values).}

\item{paired}{Indicator (logical) to use a test for paired data (only available for ttest and ranksum). Default = FALSE.}

\item{altp}{Numeric vector for alternative p-values to use (default is NA, use regular p-values).}

\item{fisher.test.workspace}{Number. Use this to change the default workspace for fisher.test if desired. Default = 200000.}

\item{kable}{Indicator (logical) to use kable to display table. Default = TRUE.}

\item{htmlTable}{Indicator (logical) to use htmlTable package to display table instead of kable Default = FALSE.}

\item{use_flextable}{Indicator (logical) to use flextable package to display table. Default = FALSE.}

\item{htmltitle}{Character label for htmlTable variable names column. Default = " ".}

\item{caption}{Character title for htmlTable or kable table. Default = " ".}

\item{color}{Character Hex color to use for htmlTable striping. Default = "#EEEEEE" (light grey).}

\item{byref}{Indicator (logical) to include reference "by" category column be included (default = TRUE).}
}
\description{
This function creates a nice looking summary table.
It returns a dataframe and prints an html table by default for use in R markdown documents.
}
\keyword{Lisa}
\keyword{consulting}
\keyword{summary}
\keyword{table}
