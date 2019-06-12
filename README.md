'nicetable' R package example
================
Lisa Rein
2019-06-12

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

-   Required arguments:

        + df: name of data frame.
        + covs: character vector of column names to include in table.
        + type: numeric vector of variable types (1 for continuous and 2 for categorical).

``` r
tab <- nicetable(df = survey,
                  covs = names(survey),
                  type = c(2,1,1,2,2,1,2,2,2,1,2,1))
```

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">
<thead>
<tr>
<td colspan="2" style="text-align: left;">
</td>
</tr>
<tr>
<th style="border-top: 2px solid grey;">
</th>
<th colspan="1" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
All
</th>
</tr>
<tr>
<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
N = 237
</th>
</tr>
</thead>
<tbody>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Sex<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Female
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
118 (50.0%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Male
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
118 (50.0%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
1
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Wr.Hnd<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
18.67 (1.88)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
18.50 \[17.50, 19.80\]
</td>
</tr>
<tr>
<td style="text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
13.00, 23.20
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>NW.Hnd<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
18.58 (1.97)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
18.50 \[17.50, 19.72\]
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
12.50, 23.50
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
1
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>W.Hnd<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Left
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
18 (7.6%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Right
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
218 (92.4%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Fold<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      L on R
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
99 (41.8%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Neither
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
18 (7.6%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      R on L
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
120 (50.6%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
0
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Pulse<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
74.15 (11.69)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
72.50 \[66.00, 80.00\]
</td>
</tr>
<tr>
<td style="text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
35.00, 104.00
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
45
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Clap<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Left
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
39 (16.5%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Neither
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
50 (21.2%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Right
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
147 (62.3%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
1
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Exer<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
115 (48.5%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      None
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
24 (10.1%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Some
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
98 (41.4%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Smoke<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Heavy
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
11 (4.7%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Never
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
189 (80.1%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Occas
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
19 (8.1%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Regul
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
17 (7.2%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
1
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Height<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
172.38 (9.85)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
171.00 \[165.00, 180.00\]
</td>
</tr>
<tr>
<td style="text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
150.00, 200.00
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
28
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>M.I<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Imperial
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
68 (32.5%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Metric
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
141 (67.5%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
28
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Age<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
20.37 (6.47)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
18.58 \[17.67, 20.17\]
</td>
</tr>
<tr>
<td style="text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
16.75, 73.00
</td>
</tr>
<tr>
<td style="border-bottom: 2px solid grey; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; border-bottom: 2px solid grey; text-align: center;">
0
</td>
</tr>
</tbody>
</table>
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

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">
<thead>
<tr>
<td colspan="2" style="text-align: left;">
</td>
</tr>
<tr>
<th style="border-top: 2px solid grey;">
</th>
<th colspan="1" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
All
</th>
</tr>
<tr>
<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
N = 237
</th>
</tr>
</thead>
<tbody>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Gender<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Female
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
118 (50.0%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Male
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
118 (50.0%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
1
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Span of writing hand<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
18.7 (1.9)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Median
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
18.5
</td>
</tr>
<tr>
<td style="text-align: left;">
      IQR
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
2.3
</td>
</tr>
<tr>
<td style="text-align: left;">
      Range
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
10.2
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Span of non-writing hand<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
18.6 (2.0)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Median
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
18.5
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      IQR
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
2.2
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Range
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
11.0
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
1
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Writing hand<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Left
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
18 (7.6%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Right
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
218 (92.4%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Which arm is on top when you fold your arms?<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      L on R
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
99 (41.8%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Neither
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
18 (7.6%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      R on L
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
120 (50.6%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
0
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Pulse (bpm)<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
74.2 (11.7)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Median
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
72.5
</td>
</tr>
<tr>
<td style="text-align: left;">
      IQR
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
14.0
</td>
</tr>
<tr>
<td style="text-align: left;">
      Range
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
69.0
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
45
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Which arm is on top when you clap?<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Left
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
39 (16.5%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Neither
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
50 (21.2%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Right
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
147 (62.3%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
1
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Exercise frequency<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
115 (48.5%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      None
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
24 (10.1%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Some
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
98 (41.4%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Smoking frequency<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Heavy
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
11 (4.7%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Never
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
189 (80.1%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Occas
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
19 (8.1%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Regul
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
17 (7.2%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
1
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Height (cm)<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
172.4 (9.8)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Median
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
171.0
</td>
</tr>
<tr>
<td style="text-align: left;">
      IQR
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
15.0
</td>
</tr>
<tr>
<td style="text-align: left;">
      Range
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
50.0
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
28
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Which units did students use<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Imperial
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
68 (32.5%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Metric
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
141 (67.5%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
28
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Age (years)<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
20.4 (6.5)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Median
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
18.6
</td>
</tr>
<tr>
<td style="text-align: left;">
      IQR
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
2.5
</td>
</tr>
<tr>
<td style="text-align: left;">
      Range
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
56.2
</td>
</tr>
<tr>
<td style="border-bottom: 2px solid grey; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; border-bottom: 2px solid grey; text-align: center;">
0
</td>
</tr>
</tbody>
</table>
### Create summary table stratified by gender

``` r
tab <- nicetable(df = survey,
                 by = "Sex",
                 covs = names(survey)[-1],
                 type = c(1,1,2,2,1,2,2,2,1,2,1))
```

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">
<thead>
<tr>
<td colspan="6" style="text-align: left;">
</td>
</tr>
<tr>
<th style="border-top: 2px solid grey;">
</th>
<th colspan="1" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
All
</th>
<th style="border-top: 2px solid grey;; border-bottom: hidden;">
 
</th>
<th colspan="1" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
Female
</th>
<th style="border-top: 2px solid grey;; border-bottom: hidden;">
 
</th>
<th colspan="1" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
Male
</th>
</tr>
<tr>
<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
N = 236
</th>
<th style="border-bottom: 1px solid grey;" colspan="1">
 
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
N = 118
</th>
<th style="border-bottom: 1px solid grey;" colspan="1">
 
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
N = 118
</th>
</tr>
</thead>
<tbody>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Wr.Hnd<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
18.66 (1.88)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
17.60 (1.31)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
19.74 (1.75)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
18.50 \[17.50, 19.75\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
17.50 \[17.00, 18.50\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
19.50 \[18.50, 21.00\]
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
13.00, 23.20
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
13.00, 20.80
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
14.00, 23.20
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
1
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
0
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
1
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>NW.Hnd<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
18.58 (1.97)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
17.46 (1.41)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
19.71 (1.80)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
18.50 \[17.50, 19.75\]
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
17.60 \[16.75, 18.15\]
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
19.50 \[18.50, 20.90\]
</td>
</tr>
<tr>
<td style="text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
12.50, 23.50
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
12.50, 20.70
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
13.30, 23.50
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>W.Hnd<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Left
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
17 (7.2%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
7 (6.0%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
10 (8.5%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Right
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
218 (92.8%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
110 (94.0%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
108 (91.5%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
1
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
1
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
0
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Fold<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      L on R
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
98 (41.5%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
48 (40.7%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
50 (42.4%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Neither
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
18 (7.6%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
6 (5.1%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
12 (10.2%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      R on L
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
120 (50.8%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
64 (54.2%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
56 (47.5%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Pulse<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
74.16 (11.72)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
75.13 (11.41)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
73.20 (12.00)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
72.00 \[66.00, 80.00\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
75.00 \[68.00, 82.00\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
72.00 \[65.00, 80.00\]
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
35.00, 104.00
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
40.00, 104.00
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
35.00, 104.00
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
45
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
23
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
22
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Clap<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Left
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
39 (16.6%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
21 (17.8%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
18 (15.4%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Neither
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
49 (20.9%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
24 (20.3%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
25 (21.4%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Right
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
147 (62.6%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
73 (61.9%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
74 (63.2%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Exer<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
114 (48.3%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
49 (41.5%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
65 (55.1%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      None
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
24 (10.2%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
11 (9.3%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
13 (11.0%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Some
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
98 (41.5%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
58 (49.2%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
40 (33.9%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
0
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
0
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
0
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Smoke<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Heavy
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
11 (4.7%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
5 (4.2%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
6 (5.1%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Never
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
188 (80.0%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
99 (83.9%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
89 (76.1%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Occas
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
19 (8.1%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
9 (7.6%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
10 (8.5%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Regul
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
17 (7.2%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
5 (4.2%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
12 (10.3%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Height<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
172.38 (9.87)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
165.69 (6.15)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
178.83 (8.38)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
171.00 \[165.00, 180.00\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
166.75 \[162.56, 170.00\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
180.00 \[172.79, 185.00\]
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
150.00, 200.00
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
150.00, 180.34
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
154.94, 200.00
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
28
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
16
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
12
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>M.I<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Imperial
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
68 (32.7%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
32 (31.4%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
36 (34.0%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Metric
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
140 (67.3%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
70 (68.6%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
70 (66.0%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
28
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
16
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
12
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Age<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
20.37 (6.49)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
20.41 (6.91)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
20.33 (6.07)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
18.58 \[17.65, 20.17\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
18.42 \[17.50, 19.98\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
18.88 \[17.92, 20.29\]
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
16.75, 73.00
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
16.92, 73.00
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
16.75, 70.42
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; border-bottom: 2px solid grey; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; border-bottom: 2px solid grey; text-align: center;">
0
</td>
<td style="background-color: #eeeeee; border-bottom: 2px solid grey;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; border-bottom: 2px solid grey; text-align: center;">
0
</td>
<td style="background-color: #eeeeee; border-bottom: 2px solid grey;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; border-bottom: 2px solid grey; text-align: center;">
0
</td>
</tr>
</tbody>
</table>
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

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">
<thead>
<tr>
<td colspan="6" style="text-align: left;">
</td>
</tr>
<tr>
<th style="border-top: 2px solid grey;">
</th>
<th colspan="1" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
All
</th>
<th style="border-top: 2px solid grey;; border-bottom: hidden;">
 
</th>
<th colspan="1" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
Female
</th>
<th style="border-top: 2px solid grey;; border-bottom: hidden;">
 
</th>
<th colspan="1" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
Male
</th>
</tr>
<tr>
<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
N = 236
</th>
<th style="border-bottom: 1px solid grey;" colspan="1">
 
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
N = 118
</th>
<th style="border-bottom: 1px solid grey;" colspan="1">
 
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
N = 118
</th>
</tr>
</thead>
<tbody>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Wr.Hnd<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
18.66 (1.88)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
17.60 (1.31)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
19.74 (1.75)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
18.50 \[17.50, 19.75\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
17.50 \[17.00, 18.50\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
19.50 \[18.50, 21.00\]
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
13.00, 23.20
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
13.00, 20.80
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
14.00, 23.20
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      N (non-missing)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
235
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
118
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
117
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
1
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
0
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
1
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>NW.Hnd<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
18.58 (1.97)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
17.46 (1.41)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
19.71 (1.80)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
18.50 \[17.50, 19.75\]
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
17.60 \[16.75, 18.15\]
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
19.50 \[18.50, 20.90\]
</td>
</tr>
<tr>
<td style="text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
12.50, 23.50
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
12.50, 20.70
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
13.30, 23.50
</td>
</tr>
<tr>
<td style="text-align: left;">
      N (non-missing)
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
235
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
118
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
117
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>W.Hnd<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Left
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
17 (7.23%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
7 (41.18%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
10 (58.82%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Right
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
218 (92.77%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
110 (50.46%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
108 (49.54%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      N (non-missing)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
235
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
117
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
118
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
1
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
1
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
0
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Fold<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      L on R
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
98 (41.53%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
48 (48.98%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
50 (51.02%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Neither
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
18 (7.63%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
6 (33.33%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
12 (66.67%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      R on L
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
120 (50.85%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
64 (53.33%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
56 (46.67%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      N (non-missing)
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
236
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
118
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
118
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Pulse<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
74.16 (11.72)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
75.13 (11.41)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
73.20 (12.00)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
72.00 \[66.00, 80.00\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
75.00 \[68.00, 82.00\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
72.00 \[65.00, 80.00\]
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
35.00, 104.00
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
40.00, 104.00
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
35.00, 104.00
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      N (non-missing)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
191
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
95
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
96
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
45
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
23
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
22
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Clap<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Left
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
39 (16.60%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
21 (53.85%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
18 (46.15%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Neither
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
49 (20.85%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
24 (48.98%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
25 (51.02%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Right
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
147 (62.55%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
73 (49.66%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
74 (50.34%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      N (non-missing)
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
235
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
118
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
117
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Exer<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
114 (48.31%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
49 (42.98%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
65 (57.02%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      None
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
24 (10.17%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
11 (45.83%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
13 (54.17%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Some
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
98 (41.53%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
58 (59.18%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
40 (40.82%)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      N (non-missing)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
236
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
118
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
118
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
0
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
0
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
0
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Smoke<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Heavy
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
11 (4.68%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
5 (45.45%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
6 (54.55%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Never
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
188 (80.00%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
99 (52.66%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
89 (47.34%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Occas
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
19 (8.09%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
9 (47.37%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
10 (52.63%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Regul
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
17 (7.23%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
5 (29.41%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
12 (70.59%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      N (non-missing)
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
235
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
118
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
117
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Height<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
172.38 (9.87)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
165.69 (6.15)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
178.83 (8.38)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
171.00 \[165.00, 180.00\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
166.75 \[162.56, 170.00\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
180.00 \[172.79, 185.00\]
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
150.00, 200.00
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
150.00, 180.34
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
154.94, 200.00
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      N (non-missing)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
208
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
102
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
106
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
28
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
16
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
12
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>M.I<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Imperial
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
68 (32.69%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
32 (47.06%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
36 (52.94%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      Metric
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
140 (67.31%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
70 (50.00%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
70 (50.00%)
</td>
</tr>
<tr>
<td style="text-align: left;">
      N (non-missing)
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
208
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
102
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
106
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
28
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
16
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
12
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Age<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
20.37 (6.49)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
20.41 (6.91)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
20.33 (6.07)
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
18.58 \[17.65, 20.17\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
18.42 \[17.50, 19.98\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
18.88 \[17.92, 20.29\]
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
16.75, 73.00
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
16.92, 73.00
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
16.75, 70.42
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      N (non-missing)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
236
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
118
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
118
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; border-bottom: 2px solid grey; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; border-bottom: 2px solid grey; text-align: center;">
0
</td>
<td style="background-color: #eeeeee; border-bottom: 2px solid grey;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; border-bottom: 2px solid grey; text-align: center;">
0
</td>
<td style="background-color: #eeeeee; border-bottom: 2px solid grey;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; border-bottom: 2px solid grey; text-align: center;">
0
</td>
</tr>
</tbody>
</table>
### Add p-values for statistical comparisons. Select tests automatically using non-parametric methods.

``` r
tab <- nicetable(df = survey,
                 by = "Sex",
                 covs = names(survey)[-1],
                 type = c(1,1,2,2,1,2,2,2,1,2,1),
                 tests = "np")
```

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">
<thead>
<tr>
<td colspan="9" style="text-align: left;">
</td>
</tr>
<tr>
<th style="border-top: 2px solid grey;">
</th>
<th colspan="1" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
All
</th>
<th style="border-top: 2px solid grey;; border-bottom: hidden;">
 
</th>
<th colspan="1" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
Female
</th>
<th style="border-top: 2px solid grey;; border-bottom: hidden;">
 
</th>
<th colspan="1" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
Male
</th>
<th style="border-top: 2px solid grey;; border-bottom: hidden;">
 
</th>
<th colspan="2" style="font-weight: 900; border-top: 2px solid grey; text-align: center;">
</th>
</tr>
<tr>
<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
N = 236
</th>
<th style="border-bottom: 1px solid grey;" colspan="1">
 
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
N = 118
</th>
<th style="border-bottom: 1px solid grey;" colspan="1">
 
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
N = 118
</th>
<th style="border-bottom: 1px solid grey;" colspan="1">
 
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
p-value
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
Test
</th>
</tr>
</thead>
<tbody>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Wr.Hnd<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
&lt; 0.001
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
Wilcoxon rank-sum
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
18.66 (1.88)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
17.60 (1.31)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
19.74 (1.75)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
18.50 \[17.50, 19.75\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
17.50 \[17.00, 18.50\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
19.50 \[18.50, 21.00\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
13.00, 23.20
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
13.00, 20.80
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
14.00, 23.20
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
1
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
0
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
1
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>NW.Hnd<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
&lt; 0.001
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
Wilcoxon rank-sum
</td>
</tr>
<tr>
<td style="text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
18.58 (1.97)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
17.46 (1.41)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
19.71 (1.80)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
18.50 \[17.50, 19.75\]
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
17.60 \[16.75, 18.15\]
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
19.50 \[18.50, 20.90\]
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
12.50, 23.50
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
12.50, 20.70
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
13.30, 23.50
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>W.Hnd<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
0.616
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
Fisher's exact
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Left
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
17 (7.2%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
7 (6.0%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
10 (8.5%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Right
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
218 (92.8%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
110 (94.0%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
108 (91.5%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
1
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
1
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
0
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Fold<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0.290
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
Fisher's exact
</td>
</tr>
<tr>
<td style="text-align: left;">
      L on R
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
98 (41.5%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
48 (40.7%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
50 (42.4%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Neither
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
18 (7.6%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
6 (5.1%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
12 (10.2%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      R on L
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
120 (50.8%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
64 (54.2%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
56 (47.5%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Pulse<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
0.149
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
Wilcoxon rank-sum
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
74.16 (11.72)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
75.13 (11.41)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
73.20 (12.00)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
72.00 \[66.00, 80.00\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
75.00 \[68.00, 82.00\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
72.00 \[65.00, 80.00\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
35.00, 104.00
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
40.00, 104.00
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
35.00, 104.00
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
45
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
23
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
22
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Clap<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0.913
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
Fisher's exact
</td>
</tr>
<tr>
<td style="text-align: left;">
      Left
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
39 (16.6%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
21 (17.8%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
18 (15.4%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Neither
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
49 (20.9%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
24 (20.3%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
25 (21.4%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Right
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
147 (62.6%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
73 (61.9%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
74 (63.2%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Exer<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
0.056
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
Fisher's exact
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
114 (48.3%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
49 (41.5%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
65 (55.1%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      None
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
24 (10.2%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
11 (9.3%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
13 (11.0%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Some
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
98 (41.5%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
58 (49.2%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
40 (33.9%)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
0
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
0
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
0
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Smoke<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0.310
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
Fisher's exact
</td>
</tr>
<tr>
<td style="text-align: left;">
      Heavy
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
11 (4.7%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
5 (4.2%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
6 (5.1%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Never
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
188 (80.0%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
99 (83.9%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
89 (76.1%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Occas
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
19 (8.1%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
9 (7.6%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
10 (8.5%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Regul
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
17 (7.2%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
5 (4.2%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
12 (10.3%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Height<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
&lt; 0.001
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
Wilcoxon rank-sum
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
172.38 (9.87)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
165.69 (6.15)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
178.83 (8.38)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
171.00 \[165.00, 180.00\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
166.75 \[162.56, 170.00\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
180.00 \[172.79, 185.00\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
150.00, 200.00
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
150.00, 180.34
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
154.94, 200.00
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
28
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
16
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
12
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>M.I<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0.768
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
Fisher's exact
</td>
</tr>
<tr>
<td style="text-align: left;">
      Imperial
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
68 (32.7%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
32 (31.4%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
36 (34.0%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Metric
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
140 (67.3%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
70 (68.6%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
70 (66.0%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
28
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
16
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
12
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
<b>Age<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
0.037
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
Wilcoxon rank-sum
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
20.37 (6.49)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
20.41 (6.91)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
20.33 (6.07)
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
18.58 \[17.65, 20.17\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
18.42 \[17.50, 19.98\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
18.88 \[17.92, 20.29\]
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
16.75, 73.00
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
16.92, 73.00
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
16.75, 70.42
</td>
<td style="background-color: #eeeeee;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; text-align: center;">
</td>
</tr>
<tr style="background-color: #eeeeee;">
<td style="background-color: #eeeeee; border-bottom: 2px solid grey; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; border-bottom: 2px solid grey; text-align: center;">
0
</td>
<td style="background-color: #eeeeee; border-bottom: 2px solid grey;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; border-bottom: 2px solid grey; text-align: center;">
0
</td>
<td style="background-color: #eeeeee; border-bottom: 2px solid grey;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; border-bottom: 2px solid grey; text-align: center;">
0
</td>
<td style="background-color: #eeeeee; border-bottom: 2px solid grey;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; border-bottom: 2px solid grey; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #eeeeee; border-bottom: 2px solid grey; text-align: center;">
</td>
</tr>
</tbody>
</table>
### Remove "All" column. Change striping colors. Add a title. Change to parametric tests.

``` r
tab <- nicetable(df = survey,
                 allcol = FALSE,
                 by = "Sex",
                 covs = names(survey)[-1],
                 type = c(1,1,2,2,1,2,2,2,1,2,1),
                 tests = "p",
                 htmlcaption = "Table 1: Survey responses by gender",
                 htmltitle = "Survey questions",
                 color = "powderblue")
```

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">
<thead>
<tr>
<td colspan="7" style="text-align: left;">
Table 1: Survey responses by gender
</td>
</tr>
<tr>
<th style="border-top: 2px solid grey;">
</th>
<th colspan="1" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
Female
</th>
<th style="border-top: 2px solid grey;; border-bottom: hidden;">
 
</th>
<th colspan="1" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
Male
</th>
<th style="border-top: 2px solid grey;; border-bottom: hidden;">
 
</th>
<th colspan="2" style="font-weight: 900; border-top: 2px solid grey; text-align: center;">
</th>
</tr>
<tr>
<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">
Survey questions
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
N = 118
</th>
<th style="border-bottom: 1px solid grey;" colspan="1">
 
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
N = 118
</th>
<th style="border-bottom: 1px solid grey;" colspan="1">
 
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
p-value
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
Test
</th>
</tr>
</thead>
<tbody>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
<b>Wr.Hnd<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
&lt; 0.001
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
T-test
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
17.60 (1.31)
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
19.74 (1.75)
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
17.50 \[17.00, 18.50\]
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
19.50 \[18.50, 21.00\]
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
13.00, 20.80
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
14.00, 23.20
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
0
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
1
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>NW.Hnd<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
&lt; 0.001
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
T-test
</td>
</tr>
<tr>
<td style="text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
17.46 (1.41)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
19.71 (1.80)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
17.60 \[16.75, 18.15\]
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
19.50 \[18.50, 20.90\]
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
12.50, 20.70
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
13.30, 23.50
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
<b>W.Hnd<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
0.627
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
Chi-squared
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
      Left
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
7 (6.0%)
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
10 (8.5%)
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
      Right
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
110 (94.0%)
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
108 (91.5%)
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
1
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
0
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Fold<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0.276
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
Chi-squared
</td>
</tr>
<tr>
<td style="text-align: left;">
      L on R
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
48 (40.7%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
50 (42.4%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Neither
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
6 (5.1%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
12 (10.2%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      R on L
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
64 (54.2%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
56 (47.5%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
<b>Pulse<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
0.256
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
T-test
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
75.13 (11.41)
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
73.20 (12.00)
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
75.00 \[68.00, 82.00\]
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
72.00 \[65.00, 80.00\]
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
40.00, 104.00
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
35.00, 104.00
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
23
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
22
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Clap<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0.881
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
Chi-squared
</td>
</tr>
<tr>
<td style="text-align: left;">
      Left
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
21 (17.8%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
18 (15.4%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Neither
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
24 (20.3%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
25 (21.4%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Right
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
73 (61.9%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
74 (63.2%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
<b>Exer<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
0.057
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
Chi-squared
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
      Freq
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
49 (41.5%)
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
65 (55.1%)
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
      None
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
11 (9.3%)
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
13 (11.0%)
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
      Some
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
58 (49.2%)
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
40 (33.9%)
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
0
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
0
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>Smoke<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0.314
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
Chi-squared
</td>
</tr>
<tr>
<td style="text-align: left;">
      Heavy
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
5 (4.2%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
6 (5.1%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Never
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
99 (83.9%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
89 (76.1%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Occas
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
9 (7.6%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
10 (8.5%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Regul
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
5 (4.2%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
12 (10.3%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
1
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
<b>Height<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
&lt; 0.001
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
T-test
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
165.69 (6.15)
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
178.83 (8.38)
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
166.75 \[162.56, 170.00\]
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
180.00 \[172.79, 185.00\]
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
150.00, 180.34
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
154.94, 200.00
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
16
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
12
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
<b>M.I<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
0.802
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
Chi-squared
</td>
</tr>
<tr>
<td style="text-align: left;">
      Imperial
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
32 (31.4%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
36 (34.0%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Metric
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
70 (68.6%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
70 (66.0%)
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr>
<td style="text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
16
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
12
</td>
<td style colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; text-align: center;">
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
<b>Age<b/>
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
0.929
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
T-test
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
      Mean (SD)
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
20.41 (6.91)
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
20.33 (6.07)
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
      Median \[Q1, Q3\]
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
18.42 \[17.50, 19.98\]
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
18.88 \[17.92, 20.29\]
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; text-align: left;">
      Min, Max
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
16.92, 73.00
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
16.75, 70.42
</td>
<td style="background-color: #b0e0e6;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; text-align: center;">
</td>
</tr>
<tr style="background-color: #b0e0e6;">
<td style="background-color: #b0e0e6; border-bottom: 2px solid grey; text-align: left;">
      Freq Missing
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; border-bottom: 2px solid grey; text-align: center;">
0
</td>
<td style="background-color: #b0e0e6; border-bottom: 2px solid grey;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; border-bottom: 2px solid grey; text-align: center;">
0
</td>
<td style="background-color: #b0e0e6; border-bottom: 2px solid grey;" colspan="1">
 
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; border-bottom: 2px solid grey; text-align: center;">
</td>
<td style="border-collapse: collapse; padding: 4px; background-color: #b0e0e6; border-bottom: 2px solid grey; text-align: center;">
</td>
</tr>
</tbody>
</table>
