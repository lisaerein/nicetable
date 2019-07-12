‘nicetable’ R package example
================
Lisa Rein
2019-07-12

### Load R packages

``` r
packs <- c("devtools",
           "knitr",
           "MASS",
           "htmlTable")

lapply(packs, function(x) require(x, character.only = T))

install_github("lisaerein/nicetable")
library(nicetable)

help(nicetable)

options(knitr.kable.NA = '.')
```

### Load *survey* example dataset from the MASS package

``` r
head(survey <- survey)
```

    ##      Sex Wr.Hnd NW.Hnd W.Hnd    Fold Pulse    Clap Exer Smoke Height
    ## 1 Female   18.5   18.0 Right  R on L    92    Left Some Never 173.00
    ## 2   Male   19.5   20.5  Left  R on L   104    Left None Regul 177.80
    ## 3   Male   18.0   13.3 Right  L on R    87 Neither None Occas     NA
    ## 4   Male   18.8   18.9 Right  R on L    NA Neither None Never 160.00
    ## 5   Male   20.0   20.0 Right Neither    35   Right Some Never 165.00
    ## 6 Female   18.0   17.7 Right  L on R    64   Right Some Never 172.72
    ##        M.I    Age
    ## 1   Metric 18.250
    ## 2 Imperial 17.583
    ## 3     <NA> 16.917
    ## 4   Metric 20.333
    ## 5   Metric 23.667
    ## 6 Imperial 21.000

### Create a summary table for all results

  - Required arguments:
    
    ``` 
      + df: name of data frame.
      + covs: character vector of column names to include in table.
      + type: numeric vector of variable types (1 for continuous and 2 for categorical).
    ```

<!-- end list -->

``` r
tab <- nicetable(df = survey,
                  covs = names(survey),
                  type = c(2,1,1,2,2,1,2,2,2,1,2,1))
```

| Variable          |      All (N = 237).1      |
| :---------------- | :-----------------------: |
| <b>Sex<b/>        |             .             |
| Female            |        118 (50.0%)        |
| Male              |        118 (50.0%)        |
| Freq Missing      |             1             |
| <b>Wr.Hnd<b/>     |             .             |
| Mean (SD)         |       18.67 (1.88)        |
| Median \[Q1, Q3\] |  18.50 \[17.50, 19.80\]   |
| Min, Max          |       13.00, 23.20        |
| Freq Missing      |             1             |
| <b>NW.Hnd<b/>     |             .             |
| Mean (SD)         |       18.58 (1.97)        |
| Median \[Q1, Q3\] |  18.50 \[17.50, 19.72\]   |
| Min, Max          |       12.50, 23.50        |
| Freq Missing      |             1             |
| <b>W.Hnd<b/>      |             .             |
| Left              |         18 (7.6%)         |
| Right             |        218 (92.4%)        |
| Freq Missing      |             1             |
| <b>Fold<b/>       |             .             |
| L on R            |        99 (41.8%)         |
| Neither           |         18 (7.6%)         |
| R on L            |        120 (50.6%)        |
| Freq Missing      |             0             |
| <b>Pulse<b/>      |             .             |
| Mean (SD)         |       74.15 (11.69)       |
| Median \[Q1, Q3\] |  72.50 \[66.00, 80.00\]   |
| Min, Max          |       35.00, 104.00       |
| Freq Missing      |            45             |
| <b>Clap<b/>       |             .             |
| Left              |        39 (16.5%)         |
| Neither           |        50 (21.2%)         |
| Right             |        147 (62.3%)        |
| Freq Missing      |             1             |
| <b>Exer<b/>       |             .             |
| Freq              |        115 (48.5%)        |
| None              |        24 (10.1%)         |
| Some              |        98 (41.4%)         |
| Freq Missing      |             0             |
| <b>Smoke<b/>      |             .             |
| Heavy             |         11 (4.7%)         |
| Never             |        189 (80.1%)        |
| Occas             |         19 (8.1%)         |
| Regul             |         17 (7.2%)         |
| Freq Missing      |             1             |
| <b>Height<b/>     |             .             |
| Mean (SD)         |       172.38 (9.85)       |
| Median \[Q1, Q3\] | 171.00 \[165.00, 180.00\] |
| Min, Max          |      150.00, 200.00       |
| Freq Missing      |            28             |
| <b>M.I<b/>        |             .             |
| Imperial          |        68 (32.5%)         |
| Metric            |        141 (67.5%)        |
| Freq Missing      |            28             |
| <b>Age<b/>        |             .             |
| Mean (SD)         |       20.37 (6.47)        |
| Median \[Q1, Q3\] |  18.58 \[17.67, 20.17\]   |
| Min, Max          |       16.75, 73.00        |
| Freq Missing      |             0             |

### Choose summary statistics for continuous variables and use 1 decimal place. Use labels instead of variable names.

``` r
labs <- c("Gender",
          "Span of writing hand",
          "Span of non-writing hand",
          "Writing hand",
          "Which arm is on top when you fold your arms?",
          "Pulse (bpm)",
          "Which arm is on top when you clap?",
          "Exercise frequency",
          "Smoking frequency",
          "Height (cm)",
          "Which units did students use",
          "Age (years)")

tab <- nicetable(df = survey,
                 covs = names(survey),
                 labels = labs,
                 type = c(2,1,1,2,2,1,2,2,2,1,2,1),
                 stats = c("mean_sd", "median", "iqr", "range"),
                 cont.dec = 1)
```

| Variable                                            | All (N = 237).1 |
| :-------------------------------------------------- | :-------------: |
| <b>Gender<b/>                                       |        .        |
| Female                                              |   118 (50.0%)   |
| Male                                                |   118 (50.0%)   |
| Freq Missing                                        |        1        |
| <b>Span of writing hand<b/>                         |        .        |
| Mean (SD)                                           |   18.7 (1.9)    |
| Median                                              |      18.5       |
| IQR                                                 |       2.3       |
| Range                                               |      10.2       |
| Freq Missing                                        |        1        |
| <b>Span of non-writing hand<b/>                     |        .        |
| Mean (SD)                                           |   18.6 (2.0)    |
| Median                                              |      18.5       |
| IQR                                                 |       2.2       |
| Range                                               |      11.0       |
| Freq Missing                                        |        1        |
| <b>Writing hand<b/>                                 |        .        |
| Left                                                |    18 (7.6%)    |
| Right                                               |   218 (92.4%)   |
| Freq Missing                                        |        1        |
| <b>Which arm is on top when you fold your arms?<b/> |        .        |
| L on R                                              |   99 (41.8%)    |
| Neither                                             |    18 (7.6%)    |
| R on L                                              |   120 (50.6%)   |
| Freq Missing                                        |        0        |
| <b>Pulse (bpm)<b/>                                  |        .        |
| Mean (SD)                                           |   74.2 (11.7)   |
| Median                                              |      72.5       |
| IQR                                                 |      14.0       |
| Range                                               |      69.0       |
| Freq Missing                                        |       45        |
| <b>Which arm is on top when you clap?<b/>           |        .        |
| Left                                                |   39 (16.5%)    |
| Neither                                             |   50 (21.2%)    |
| Right                                               |   147 (62.3%)   |
| Freq Missing                                        |        1        |
| <b>Exercise frequency<b/>                           |        .        |
| Freq                                                |   115 (48.5%)   |
| None                                                |   24 (10.1%)    |
| Some                                                |   98 (41.4%)    |
| Freq Missing                                        |        0        |
| <b>Smoking frequency<b/>                            |        .        |
| Heavy                                               |    11 (4.7%)    |
| Never                                               |   189 (80.1%)   |
| Occas                                               |    19 (8.1%)    |
| Regul                                               |    17 (7.2%)    |
| Freq Missing                                        |        1        |
| <b>Height (cm)<b/>                                  |        .        |
| Mean (SD)                                           |   172.4 (9.8)   |
| Median                                              |      171.0      |
| IQR                                                 |      15.0       |
| Range                                               |      50.0       |
| Freq Missing                                        |       28        |
| <b>Which units did students use<b/>                 |        .        |
| Imperial                                            |   68 (32.5%)    |
| Metric                                              |   141 (67.5%)   |
| Freq Missing                                        |       28        |
| <b>Age (years)<b/>                                  |        .        |
| Mean (SD)                                           |   20.4 (6.5)    |
| Median                                              |      18.6       |
| IQR                                                 |       2.5       |
| Range                                               |      56.2       |
| Freq Missing                                        |        0        |

### Create summary table stratified by gender

``` r
tab <- nicetable(df = survey,
                 by = "Sex",
                 covs = names(survey)[-1],
                 type = c(1,1,2,2,1,2,2,2,1,2,1))
```

| Variable          |       All (N = 236)       |     Female (N = 118)      |      Male (N = 118)       |
| :---------------- | :-----------------------: | :-----------------------: | :-----------------------: |
| <b>Wr.Hnd<b/>     |             .             |             .             |             .             |
| Mean (SD)         |       18.66 (1.88)        |       17.60 (1.31)        |       19.74 (1.75)        |
| Median \[Q1, Q3\] |  18.50 \[17.50, 19.75\]   |  17.50 \[17.00, 18.50\]   |  19.50 \[18.50, 21.00\]   |
| Min, Max          |       13.00, 23.20        |       13.00, 20.80        |       14.00, 23.20        |
| Freq Missing      |             1             |             0             |             1             |
| <b>NW.Hnd<b/>     |             .             |             .             |             .             |
| Mean (SD)         |       18.58 (1.97)        |       17.46 (1.41)        |       19.71 (1.80)        |
| Median \[Q1, Q3\] |  18.50 \[17.50, 19.75\]   |  17.60 \[16.75, 18.15\]   |  19.50 \[18.50, 20.90\]   |
| Min, Max          |       12.50, 23.50        |       12.50, 20.70        |       13.30, 23.50        |
| Freq Missing      |             1             |             0             |             1             |
| <b>W.Hnd<b/>      |             .             |             .             |             .             |
| Left              |         17 (7.2%)         |         7 (6.0%)          |         10 (8.5%)         |
| Right             |        218 (92.8%)        |        110 (94.0%)        |        108 (91.5%)        |
| Freq Missing      |             1             |             1             |             0             |
| <b>Fold<b/>       |             .             |             .             |             .             |
| L on R            |        98 (41.5%)         |        48 (40.7%)         |        50 (42.4%)         |
| Neither           |         18 (7.6%)         |         6 (5.1%)          |        12 (10.2%)         |
| R on L            |        120 (50.8%)        |        64 (54.2%)         |        56 (47.5%)         |
| Freq Missing      |             0             |             0             |             0             |
| <b>Pulse<b/>      |             .             |             .             |             .             |
| Mean (SD)         |       74.16 (11.72)       |       75.13 (11.41)       |       73.20 (12.00)       |
| Median \[Q1, Q3\] |  72.00 \[66.00, 80.00\]   |  75.00 \[68.00, 82.00\]   |  72.00 \[65.00, 80.00\]   |
| Min, Max          |       35.00, 104.00       |       40.00, 104.00       |       35.00, 104.00       |
| Freq Missing      |            45             |            23             |            22             |
| <b>Clap<b/>       |             .             |             .             |             .             |
| Left              |        39 (16.6%)         |        21 (17.8%)         |        18 (15.4%)         |
| Neither           |        49 (20.9%)         |        24 (20.3%)         |        25 (21.4%)         |
| Right             |        147 (62.6%)        |        73 (61.9%)         |        74 (63.2%)         |
| Freq Missing      |             1             |             0             |             1             |
| <b>Exer<b/>       |             .             |             .             |             .             |
| Freq              |        114 (48.3%)        |        49 (41.5%)         |        65 (55.1%)         |
| None              |        24 (10.2%)         |         11 (9.3%)         |        13 (11.0%)         |
| Some              |        98 (41.5%)         |        58 (49.2%)         |        40 (33.9%)         |
| Freq Missing      |             0             |             0             |             0             |
| <b>Smoke<b/>      |             .             |             .             |             .             |
| Heavy             |         11 (4.7%)         |         5 (4.2%)          |         6 (5.1%)          |
| Never             |        188 (80.0%)        |        99 (83.9%)         |        89 (76.1%)         |
| Occas             |         19 (8.1%)         |         9 (7.6%)          |         10 (8.5%)         |
| Regul             |         17 (7.2%)         |         5 (4.2%)          |        12 (10.3%)         |
| Freq Missing      |             1             |             0             |             1             |
| <b>Height<b/>     |             .             |             .             |             .             |
| Mean (SD)         |       172.38 (9.87)       |       165.69 (6.15)       |       178.83 (8.38)       |
| Median \[Q1, Q3\] | 171.00 \[165.00, 180.00\] | 166.75 \[162.56, 170.00\] | 180.00 \[172.79, 185.00\] |
| Min, Max          |      150.00, 200.00       |      150.00, 180.34       |      154.94, 200.00       |
| Freq Missing      |            28             |            16             |            12             |
| <b>M.I<b/>        |             .             |             .             |             .             |
| Imperial          |        68 (32.7%)         |        32 (31.4%)         |        36 (34.0%)         |
| Metric            |        140 (67.3%)        |        70 (68.6%)         |        70 (66.0%)         |
| Freq Missing      |            28             |            16             |            12             |
| <b>Age<b/>        |             .             |             .             |             .             |
| Mean (SD)         |       20.37 (6.49)        |       20.41 (6.91)        |       20.33 (6.07)        |
| Median \[Q1, Q3\] |  18.58 \[17.65, 20.17\]   |  18.42 \[17.50, 19.98\]   |  18.88 \[17.92, 20.29\]   |
| Min, Max          |       16.75, 73.00        |       16.92, 73.00        |       16.75, 70.42        |
| Freq Missing      |             0             |             0             |             0             |

### Change percentages from row to column percentages and use 2 decimal places. Add a row to the number of non-missing values.

``` r
tab <- nicetable(df = survey,
                 by = "Sex",
                 covs = names(survey)[-1],
                 type = c(1,1,2,2,1,2,2,2,1,2,1),
                 percent = 1,
                 perc.dec = 2,
                 dispN = TRUE)
```

| Variable          |       All (N = 236)       |     Female (N = 118)      |      Male (N = 118)       |
| :---------------- | :-----------------------: | :-----------------------: | :-----------------------: |
| <b>Wr.Hnd<b/>     |             .             |             .             |             .             |
| Mean (SD)         |       18.66 (1.88)        |       17.60 (1.31)        |       19.74 (1.75)        |
| Median \[Q1, Q3\] |  18.50 \[17.50, 19.75\]   |  17.50 \[17.00, 18.50\]   |  19.50 \[18.50, 21.00\]   |
| Min, Max          |       13.00, 23.20        |       13.00, 20.80        |       14.00, 23.20        |
| N (non-missing)   |            235            |            118            |            117            |
| Freq Missing      |             1             |             0             |             1             |
| <b>NW.Hnd<b/>     |             .             |             .             |             .             |
| Mean (SD)         |       18.58 (1.97)        |       17.46 (1.41)        |       19.71 (1.80)        |
| Median \[Q1, Q3\] |  18.50 \[17.50, 19.75\]   |  17.60 \[16.75, 18.15\]   |  19.50 \[18.50, 20.90\]   |
| Min, Max          |       12.50, 23.50        |       12.50, 20.70        |       13.30, 23.50        |
| N (non-missing)   |            235            |            118            |            117            |
| Freq Missing      |             1             |             0             |             1             |
| <b>W.Hnd<b/>      |             .             |             .             |             .             |
| Left              |        17 (7.23%)         |        7 (41.18%)         |        10 (58.82%)        |
| Right             |       218 (92.77%)        |       110 (50.46%)        |       108 (49.54%)        |
| N (non-missing)   |            235            |            117            |            118            |
| Freq Missing      |             1             |             1             |             0             |
| <b>Fold<b/>       |             .             |             .             |             .             |
| L on R            |        98 (41.53%)        |        48 (48.98%)        |        50 (51.02%)        |
| Neither           |        18 (7.63%)         |        6 (33.33%)         |        12 (66.67%)        |
| R on L            |       120 (50.85%)        |        64 (53.33%)        |        56 (46.67%)        |
| N (non-missing)   |            236            |            118            |            118            |
| Freq Missing      |             0             |             0             |             0             |
| <b>Pulse<b/>      |             .             |             .             |             .             |
| Mean (SD)         |       74.16 (11.72)       |       75.13 (11.41)       |       73.20 (12.00)       |
| Median \[Q1, Q3\] |  72.00 \[66.00, 80.00\]   |  75.00 \[68.00, 82.00\]   |  72.00 \[65.00, 80.00\]   |
| Min, Max          |       35.00, 104.00       |       40.00, 104.00       |       35.00, 104.00       |
| N (non-missing)   |            191            |            95             |            96             |
| Freq Missing      |            45             |            23             |            22             |
| <b>Clap<b/>       |             .             |             .             |             .             |
| Left              |        39 (16.60%)        |        21 (53.85%)        |        18 (46.15%)        |
| Neither           |        49 (20.85%)        |        24 (48.98%)        |        25 (51.02%)        |
| Right             |       147 (62.55%)        |        73 (49.66%)        |        74 (50.34%)        |
| N (non-missing)   |            235            |            118            |            117            |
| Freq Missing      |             1             |             0             |             1             |
| <b>Exer<b/>       |             .             |             .             |             .             |
| Freq              |       114 (48.31%)        |        49 (42.98%)        |        65 (57.02%)        |
| None              |        24 (10.17%)        |        11 (45.83%)        |        13 (54.17%)        |
| Some              |        98 (41.53%)        |        58 (59.18%)        |        40 (40.82%)        |
| N (non-missing)   |            236            |            118            |            118            |
| Freq Missing      |             0             |             0             |             0             |
| <b>Smoke<b/>      |             .             |             .             |             .             |
| Heavy             |        11 (4.68%)         |        5 (45.45%)         |        6 (54.55%)         |
| Never             |       188 (80.00%)        |        99 (52.66%)        |        89 (47.34%)        |
| Occas             |        19 (8.09%)         |        9 (47.37%)         |        10 (52.63%)        |
| Regul             |        17 (7.23%)         |        5 (29.41%)         |        12 (70.59%)        |
| N (non-missing)   |            235            |            118            |            117            |
| Freq Missing      |             1             |             0             |             1             |
| <b>Height<b/>     |             .             |             .             |             .             |
| Mean (SD)         |       172.38 (9.87)       |       165.69 (6.15)       |       178.83 (8.38)       |
| Median \[Q1, Q3\] | 171.00 \[165.00, 180.00\] | 166.75 \[162.56, 170.00\] | 180.00 \[172.79, 185.00\] |
| Min, Max          |      150.00, 200.00       |      150.00, 180.34       |      154.94, 200.00       |
| N (non-missing)   |            208            |            102            |            106            |
| Freq Missing      |            28             |            16             |            12             |
| <b>M.I<b/>        |             .             |             .             |             .             |
| Imperial          |        68 (32.69%)        |        32 (47.06%)        |        36 (52.94%)        |
| Metric            |       140 (67.31%)        |        70 (50.00%)        |        70 (50.00%)        |
| N (non-missing)   |            208            |            102            |            106            |
| Freq Missing      |            28             |            16             |            12             |
| <b>Age<b/>        |             .             |             .             |             .             |
| Mean (SD)         |       20.37 (6.49)        |       20.41 (6.91)        |       20.33 (6.07)        |
| Median \[Q1, Q3\] |  18.58 \[17.65, 20.17\]   |  18.42 \[17.50, 19.98\]   |  18.88 \[17.92, 20.29\]   |
| Min, Max          |       16.75, 73.00        |       16.92, 73.00        |       16.75, 70.42        |
| N (non-missing)   |            236            |            118            |            118            |
| Freq Missing      |             0             |             0             |             0             |

### Add p-values for statistical comparisons. Select tests automatically using non-parametric methods.

``` r
tab <- nicetable(df = survey,
                 by = "Sex",
                 covs = names(survey)[-1],
                 type = c(1,1,2,2,1,2,2,2,1,2,1),
                 tests = "np")
```

| Variable          |       All (N = 236)       |     Female (N = 118)      |      Male (N = 118)       |  p-value |       Test        |
| :---------------- | :-----------------------: | :-----------------------: | :-----------------------: | -------: | :---------------: |
| <b>Wr.Hnd<b/>     |             .             |             .             |             .             | \< 0.001 | Wilcoxon rank-sum |
| Mean (SD)         |       18.66 (1.88)        |       17.60 (1.31)        |       19.74 (1.75)        |        . |         .         |
| Median \[Q1, Q3\] |  18.50 \[17.50, 19.75\]   |  17.50 \[17.00, 18.50\]   |  19.50 \[18.50, 21.00\]   |        . |         .         |
| Min, Max          |       13.00, 23.20        |       13.00, 20.80        |       14.00, 23.20        |        . |         .         |
| Freq Missing      |             1             |             0             |             1             |        . |         .         |
| <b>NW.Hnd<b/>     |             .             |             .             |             .             | \< 0.001 | Wilcoxon rank-sum |
| Mean (SD)         |       18.58 (1.97)        |       17.46 (1.41)        |       19.71 (1.80)        |        . |         .         |
| Median \[Q1, Q3\] |  18.50 \[17.50, 19.75\]   |  17.60 \[16.75, 18.15\]   |  19.50 \[18.50, 20.90\]   |        . |         .         |
| Min, Max          |       12.50, 23.50        |       12.50, 20.70        |       13.30, 23.50        |        . |         .         |
| Freq Missing      |             1             |             0             |             1             |        . |         .         |
| <b>W.Hnd<b/>      |             .             |             .             |             .             |    0.616 |  Fisher’s exact   |
| Left              |         17 (7.2%)         |         7 (6.0%)          |         10 (8.5%)         |        . |         .         |
| Right             |        218 (92.8%)        |        110 (94.0%)        |        108 (91.5%)        |        . |         .         |
| Freq Missing      |             1             |             1             |             0             |        . |         .         |
| <b>Fold<b/>       |             .             |             .             |             .             |    0.290 |  Fisher’s exact   |
| L on R            |        98 (41.5%)         |        48 (40.7%)         |        50 (42.4%)         |        . |         .         |
| Neither           |         18 (7.6%)         |         6 (5.1%)          |        12 (10.2%)         |        . |         .         |
| R on L            |        120 (50.8%)        |        64 (54.2%)         |        56 (47.5%)         |        . |         .         |
| Freq Missing      |             0             |             0             |             0             |        . |         .         |
| <b>Pulse<b/>      |             .             |             .             |             .             |    0.149 | Wilcoxon rank-sum |
| Mean (SD)         |       74.16 (11.72)       |       75.13 (11.41)       |       73.20 (12.00)       |        . |         .         |
| Median \[Q1, Q3\] |  72.00 \[66.00, 80.00\]   |  75.00 \[68.00, 82.00\]   |  72.00 \[65.00, 80.00\]   |        . |         .         |
| Min, Max          |       35.00, 104.00       |       40.00, 104.00       |       35.00, 104.00       |        . |         .         |
| Freq Missing      |            45             |            23             |            22             |        . |         .         |
| <b>Clap<b/>       |             .             |             .             |             .             |    0.913 |  Fisher’s exact   |
| Left              |        39 (16.6%)         |        21 (17.8%)         |        18 (15.4%)         |        . |         .         |
| Neither           |        49 (20.9%)         |        24 (20.3%)         |        25 (21.4%)         |        . |         .         |
| Right             |        147 (62.6%)        |        73 (61.9%)         |        74 (63.2%)         |        . |         .         |
| Freq Missing      |             1             |             0             |             1             |        . |         .         |
| <b>Exer<b/>       |             .             |             .             |             .             |    0.056 |  Fisher’s exact   |
| Freq              |        114 (48.3%)        |        49 (41.5%)         |        65 (55.1%)         |        . |         .         |
| None              |        24 (10.2%)         |         11 (9.3%)         |        13 (11.0%)         |        . |         .         |
| Some              |        98 (41.5%)         |        58 (49.2%)         |        40 (33.9%)         |        . |         .         |
| Freq Missing      |             0             |             0             |             0             |        . |         .         |
| <b>Smoke<b/>      |             .             |             .             |             .             |    0.310 |  Fisher’s exact   |
| Heavy             |         11 (4.7%)         |         5 (4.2%)          |         6 (5.1%)          |        . |         .         |
| Never             |        188 (80.0%)        |        99 (83.9%)         |        89 (76.1%)         |        . |         .         |
| Occas             |         19 (8.1%)         |         9 (7.6%)          |         10 (8.5%)         |        . |         .         |
| Regul             |         17 (7.2%)         |         5 (4.2%)          |        12 (10.3%)         |        . |         .         |
| Freq Missing      |             1             |             0             |             1             |        . |         .         |
| <b>Height<b/>     |             .             |             .             |             .             | \< 0.001 | Wilcoxon rank-sum |
| Mean (SD)         |       172.38 (9.87)       |       165.69 (6.15)       |       178.83 (8.38)       |        . |         .         |
| Median \[Q1, Q3\] | 171.00 \[165.00, 180.00\] | 166.75 \[162.56, 170.00\] | 180.00 \[172.79, 185.00\] |        . |         .         |
| Min, Max          |      150.00, 200.00       |      150.00, 180.34       |      154.94, 200.00       |        . |         .         |
| Freq Missing      |            28             |            16             |            12             |        . |         .         |
| <b>M.I<b/>        |             .             |             .             |             .             |    0.768 |  Fisher’s exact   |
| Imperial          |        68 (32.7%)         |        32 (31.4%)         |        36 (34.0%)         |        . |         .         |
| Metric            |        140 (67.3%)        |        70 (68.6%)         |        70 (66.0%)         |        . |         .         |
| Freq Missing      |            28             |            16             |            12             |        . |         .         |
| <b>Age<b/>        |             .             |             .             |             .             |    0.037 | Wilcoxon rank-sum |
| Mean (SD)         |       20.37 (6.49)        |       20.41 (6.91)        |       20.33 (6.07)        |        . |         .         |
| Median \[Q1, Q3\] |  18.58 \[17.65, 20.17\]   |  18.42 \[17.50, 19.98\]   |  18.88 \[17.92, 20.29\]   |        . |         .         |
| Min, Max          |       16.75, 73.00        |       16.92, 73.00        |       16.75, 70.42        |        . |         .         |
| Freq Missing      |             0             |             0             |             0             |        . |         .         |

### Remove “All” column. Change striping colors. Add a title. Change to parametric tests.

``` r
tab <- nicetable(df = survey,
                 allcol = FALSE,
                 by = "Sex",
                 covs = names(survey)[-1],
                 type = c(1,1,2,2,1,2,2,2,1,2,1),
                 tests = "p",
                 caption = "Table 1: Survey responses by gender",
                 htmltitle = "Survey questions",
                 color = "powderblue")
```

| Variable          |     Female (N = 118)      |      Male (N = 118)       |  p-value |    Test     |
| :---------------- | :-----------------------: | :-----------------------: | -------: | :---------: |
| <b>Wr.Hnd<b/>     |             .             |             .             | \< 0.001 |   T-test    |
| Mean (SD)         |       17.60 (1.31)        |       19.74 (1.75)        |        . |      .      |
| Median \[Q1, Q3\] |  17.50 \[17.00, 18.50\]   |  19.50 \[18.50, 21.00\]   |        . |      .      |
| Min, Max          |       13.00, 20.80        |       14.00, 23.20        |        . |      .      |
| Freq Missing      |             0             |             1             |        . |      .      |
| <b>NW.Hnd<b/>     |             .             |             .             | \< 0.001 |   T-test    |
| Mean (SD)         |       17.46 (1.41)        |       19.71 (1.80)        |        . |      .      |
| Median \[Q1, Q3\] |  17.60 \[16.75, 18.15\]   |  19.50 \[18.50, 20.90\]   |        . |      .      |
| Min, Max          |       12.50, 20.70        |       13.30, 23.50        |        . |      .      |
| Freq Missing      |             0             |             1             |        . |      .      |
| <b>W.Hnd<b/>      |             .             |             .             |    0.627 | Chi-squared |
| Left              |         7 (6.0%)          |         10 (8.5%)         |        . |      .      |
| Right             |        110 (94.0%)        |        108 (91.5%)        |        . |      .      |
| Freq Missing      |             1             |             0             |        . |      .      |
| <b>Fold<b/>       |             .             |             .             |    0.276 | Chi-squared |
| L on R            |        48 (40.7%)         |        50 (42.4%)         |        . |      .      |
| Neither           |         6 (5.1%)          |        12 (10.2%)         |        . |      .      |
| R on L            |        64 (54.2%)         |        56 (47.5%)         |        . |      .      |
| Freq Missing      |             0             |             0             |        . |      .      |
| <b>Pulse<b/>      |             .             |             .             |    0.256 |   T-test    |
| Mean (SD)         |       75.13 (11.41)       |       73.20 (12.00)       |        . |      .      |
| Median \[Q1, Q3\] |  75.00 \[68.00, 82.00\]   |  72.00 \[65.00, 80.00\]   |        . |      .      |
| Min, Max          |       40.00, 104.00       |       35.00, 104.00       |        . |      .      |
| Freq Missing      |            23             |            22             |        . |      .      |
| <b>Clap<b/>       |             .             |             .             |    0.881 | Chi-squared |
| Left              |        21 (17.8%)         |        18 (15.4%)         |        . |      .      |
| Neither           |        24 (20.3%)         |        25 (21.4%)         |        . |      .      |
| Right             |        73 (61.9%)         |        74 (63.2%)         |        . |      .      |
| Freq Missing      |             0             |             1             |        . |      .      |
| <b>Exer<b/>       |             .             |             .             |    0.057 | Chi-squared |
| Freq              |        49 (41.5%)         |        65 (55.1%)         |        . |      .      |
| None              |         11 (9.3%)         |        13 (11.0%)         |        . |      .      |
| Some              |        58 (49.2%)         |        40 (33.9%)         |        . |      .      |
| Freq Missing      |             0             |             0             |        . |      .      |
| <b>Smoke<b/>      |             .             |             .             |    0.314 | Chi-squared |
| Heavy             |         5 (4.2%)          |         6 (5.1%)          |        . |      .      |
| Never             |        99 (83.9%)         |        89 (76.1%)         |        . |      .      |
| Occas             |         9 (7.6%)          |         10 (8.5%)         |        . |      .      |
| Regul             |         5 (4.2%)          |        12 (10.3%)         |        . |      .      |
| Freq Missing      |             0             |             1             |        . |      .      |
| <b>Height<b/>     |             .             |             .             | \< 0.001 |   T-test    |
| Mean (SD)         |       165.69 (6.15)       |       178.83 (8.38)       |        . |      .      |
| Median \[Q1, Q3\] | 166.75 \[162.56, 170.00\] | 180.00 \[172.79, 185.00\] |        . |      .      |
| Min, Max          |      150.00, 180.34       |      154.94, 200.00       |        . |      .      |
| Freq Missing      |            16             |            12             |        . |      .      |
| <b>M.I<b/>        |             .             |             .             |    0.802 | Chi-squared |
| Imperial          |        32 (31.4%)         |        36 (34.0%)         |        . |      .      |
| Metric            |        70 (68.6%)         |        70 (66.0%)         |        . |      .      |
| Freq Missing      |            16             |            12             |        . |      .      |
| <b>Age<b/>        |             .             |             .             |    0.929 |   T-test    |
| Mean (SD)         |       20.41 (6.91)        |       20.33 (6.07)        |        . |      .      |
| Median \[Q1, Q3\] |  18.42 \[17.50, 19.98\]   |  18.88 \[17.92, 20.29\]   |        . |      .      |
| Min, Max          |       16.92, 73.00        |       16.75, 70.42        |        . |      .      |
| Freq Missing      |             0             |             0             |        . |      .      |

Table 1: Survey responses by gender
