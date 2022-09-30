Data import
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(haven)
```

\##Read in some data

Read in the litters dataset.

``` r
litters_df = read_csv("./data/FAS_litters.csv")
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
litters_df = janitor::clean_names(litters_df)
```

\##Take a look at the data

Printing in the console.

``` r
litters_df 
```

    ## # A tibble: 49 × 8
    ##    group litter_number   gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>                <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #85                   19.7        34.7       20       3       4       3
    ##  2 Con7  #1/2/95/2             27          42         19       8       0       7
    ##  3 Con7  #5/5/3/83/3-3         26          41.4       19       6       0       5
    ##  4 Con7  #5/4/2/95/2           28.5        44.1       19       5       1       4
    ##  5 Con7  #4/2/95/3-3           NA          NA         20       6       0       6
    ##  6 Con7  #2/2/95/3-2           NA          NA         20       6       0       4
    ##  7 Con7  #1/5/3/83/3-3/2       NA          NA         20       9       0       9
    ##  8 Con8  #3/83/3-3             NA          NA         20       9       1       8
    ##  9 Con8  #2/95/3               NA          NA         20       8       0       8
    ## 10 Con8  #3/5/2/2/95           28.5        NA         20       8       0       8
    ## # … with 39 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth, ⁴​pups_survive

``` r
tail(litters_df) 
```

    ## # A tibble: 6 × 8
    ##   group litter_number gd0_weight gd18_weight gd_of_birth pups_…¹ pups_…² pups_…³
    ##   <chr> <chr>              <dbl>       <dbl>       <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 Low8  #79                 25.4        43.8          19       8       0       7
    ## 2 Low8  #100                20          39.2          20       8       0       7
    ## 3 Low8  #4/84               21.8        35.2          20       4       0       4
    ## 4 Low8  #108                25.6        47.5          20       8       0       7
    ## 5 Low8  #99                 23.5        39            20       6       0       5
    ## 6 Low8  #110                25.5        42.7          20       7       0       6
    ## # … with abbreviated variable names ¹​pups_born_alive, ²​pups_dead_birth,
    ## #   ³​pups_survive

``` r
skimr::skim(litters_df)
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | litters_df |
| Number of rows                                   | 49         |
| Number of columns                                | 8          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| character                                        | 2          |
| numeric                                          | 6          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| group         |         0 |             1 |   4 |   4 |     0 |        6 |          0 |
| litter_number |         0 |             1 |   3 |  15 |     0 |       49 |          0 |

**Variable type: numeric**

| skim_variable   | n_missing | complete_rate |  mean |   sd |   p0 |   p25 |   p50 |   p75 | p100 | hist  |
|:----------------|----------:|--------------:|------:|-----:|-----:|------:|------:|------:|-----:|:------|
| gd0_weight      |        15 |          0.69 | 24.38 | 3.28 | 17.0 | 22.30 | 24.10 | 26.67 | 33.4 | ▃▇▇▆▁ |
| gd18_weight     |        17 |          0.65 | 41.52 | 4.05 | 33.4 | 38.88 | 42.25 | 43.80 | 52.7 | ▃▃▇▂▁ |
| gd_of_birth     |         0 |          1.00 | 19.65 | 0.48 | 19.0 | 19.00 | 20.00 | 20.00 | 20.0 | ▅▁▁▁▇ |
| pups_born_alive |         0 |          1.00 |  7.35 | 1.76 |  3.0 |  6.00 |  8.00 |  8.00 | 11.0 | ▁▃▂▇▁ |
| pups_dead_birth |         0 |          1.00 |  0.33 | 0.75 |  0.0 |  0.00 |  0.00 |  0.00 |  4.0 | ▇▂▁▁▁ |
| pups_survive    |         0 |          1.00 |  6.41 | 2.05 |  1.0 |  5.00 |  7.00 |  8.00 |  9.0 | ▁▃▂▇▇ |

\##Options to read csv

``` r
litters_df = read_csv("./data/FAS_litters.csv", skip = 10, col_names = FALSE)
```

    ## Rows: 40 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): X1, X2
    ## dbl (6): X3, X4, X5, X6, X7, X8
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

\##Other file formats

Read in an excel file.

``` r
mlb_df = read_excel("./data/mlb11.xlsx", range = "A1:F7")
mlb_df
```

    ## # A tibble: 6 × 6
    ##   team                 runs at_bats  hits homeruns bat_avg
    ##   <chr>               <dbl>   <dbl> <dbl>    <dbl>   <dbl>
    ## 1 Texas Rangers         855    5659  1599      210   0.283
    ## 2 Boston Red Sox        875    5710  1600      203   0.28 
    ## 3 Detroit Tigers        787    5563  1540      169   0.277
    ## 4 Kansas City Royals    730    5672  1560      129   0.275
    ## 5 St. Louis Cardinals   762    5532  1513      162   0.273
    ## 6 New York Mets         718    5600  1477      108   0.264

Read in a sas file

``` r
pulse_df = read_sas("./data/public_pulse_data.sas7bdat")
pulse_df
```

    ## # A tibble: 1,087 × 7
    ##       ID   age Sex    BDIScore_BL BDIScore_01m BDIScore_06m BDIScore_12m
    ##    <dbl> <dbl> <chr>        <dbl>        <dbl>        <dbl>        <dbl>
    ##  1 10003  48.0 male             7            1            2            0
    ##  2 10015  72.5 male             6           NA           NA           NA
    ##  3 10022  58.5 male            14            3            8           NA
    ##  4 10026  72.7 male            20            6           18           16
    ##  5 10035  60.4 male             4            0            1            2
    ##  6 10050  84.7 male             2           10           12            8
    ##  7 10078  31.3 male             4            0           NA           NA
    ##  8 10088  56.9 male             5           NA            0            2
    ##  9 10091  76.0 male             0            3            4            0
    ## 10 10092  74.2 female          10            2           11            6
    ## # … with 1,077 more rows

\##Comparison with Base R

# Data maniputation

``` r
litters_df = read_csv("./data/FAS_litters.csv")
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
litters_df = janitor::clean_names(litters_df)
```

## ‘select’

Choose some columns and not others.

``` r
select(litters_df, group, gd0_weight:gd_of_birth)
```

    ## # A tibble: 49 × 4
    ##    group gd0_weight gd18_weight gd_of_birth
    ##    <chr>      <dbl>       <dbl>       <dbl>
    ##  1 Con7        19.7        34.7          20
    ##  2 Con7        27          42            19
    ##  3 Con7        26          41.4          19
    ##  4 Con7        28.5        44.1          19
    ##  5 Con7        NA          NA            20
    ##  6 Con7        NA          NA            20
    ##  7 Con7        NA          NA            20
    ##  8 Con8        NA          NA            20
    ##  9 Con8        NA          NA            20
    ## 10 Con8        28.5        NA            20
    ## # … with 39 more rows

``` r
select(litters_df, -litter_number)
```

    ## # A tibble: 49 × 7
    ##    group gd0_weight gd18_weight gd_of_birth pups_born_alive pups_dead_…¹ pups_…²
    ##    <chr>      <dbl>       <dbl>       <dbl>           <dbl>        <dbl>   <dbl>
    ##  1 Con7        19.7        34.7          20               3            4       3
    ##  2 Con7        27          42            19               8            0       7
    ##  3 Con7        26          41.4          19               6            0       5
    ##  4 Con7        28.5        44.1          19               5            1       4
    ##  5 Con7        NA          NA            20               6            0       6
    ##  6 Con7        NA          NA            20               6            0       4
    ##  7 Con7        NA          NA            20               9            0       9
    ##  8 Con8        NA          NA            20               9            1       8
    ##  9 Con8        NA          NA            20               8            0       8
    ## 10 Con8        28.5        NA            20               8            0       8
    ## # … with 39 more rows, and abbreviated variable names ¹​pups_dead_birth,
    ## #   ²​pups_survive

Renaming columns

``` r
select(litters_df, GROUP = group)
```

    ## # A tibble: 49 × 1
    ##    GROUP
    ##    <chr>
    ##  1 Con7 
    ##  2 Con7 
    ##  3 Con7 
    ##  4 Con7 
    ##  5 Con7 
    ##  6 Con7 
    ##  7 Con7 
    ##  8 Con8 
    ##  9 Con8 
    ## 10 Con8 
    ## # … with 39 more rows

``` r
rename(litters_df, GROUP = group)
```

    ## # A tibble: 49 × 8
    ##    GROUP litter_number   gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>                <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #85                   19.7        34.7       20       3       4       3
    ##  2 Con7  #1/2/95/2             27          42         19       8       0       7
    ##  3 Con7  #5/5/3/83/3-3         26          41.4       19       6       0       5
    ##  4 Con7  #5/4/2/95/2           28.5        44.1       19       5       1       4
    ##  5 Con7  #4/2/95/3-3           NA          NA         20       6       0       6
    ##  6 Con7  #2/2/95/3-2           NA          NA         20       6       0       4
    ##  7 Con7  #1/5/3/83/3-3/2       NA          NA         20       9       0       9
    ##  8 Con8  #3/83/3-3             NA          NA         20       9       1       8
    ##  9 Con8  #2/95/3               NA          NA         20       8       0       8
    ## 10 Con8  #3/5/2/2/95           28.5        NA         20       8       0       8
    ## # … with 39 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth, ⁴​pups_survive

Select helpers

``` r
select(litters_df, starts_with("gd"))
```

    ## # A tibble: 49 × 3
    ##    gd0_weight gd18_weight gd_of_birth
    ##         <dbl>       <dbl>       <dbl>
    ##  1       19.7        34.7          20
    ##  2       27          42            19
    ##  3       26          41.4          19
    ##  4       28.5        44.1          19
    ##  5       NA          NA            20
    ##  6       NA          NA            20
    ##  7       NA          NA            20
    ##  8       NA          NA            20
    ##  9       NA          NA            20
    ## 10       28.5        NA            20
    ## # … with 39 more rows

``` r
select(litters_df, litter_number, everything())
```

    ## # A tibble: 49 × 8
    ##    litter_number   group gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr>           <chr>      <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 #85             Con7        19.7        34.7       20       3       4       3
    ##  2 #1/2/95/2       Con7        27          42         19       8       0       7
    ##  3 #5/5/3/83/3-3   Con7        26          41.4       19       6       0       5
    ##  4 #5/4/2/95/2     Con7        28.5        44.1       19       5       1       4
    ##  5 #4/2/95/3-3     Con7        NA          NA         20       6       0       6
    ##  6 #2/2/95/3-2     Con7        NA          NA         20       6       0       4
    ##  7 #1/5/3/83/3-3/2 Con7        NA          NA         20       9       0       9
    ##  8 #3/83/3-3       Con8        NA          NA         20       9       1       8
    ##  9 #2/95/3         Con8        NA          NA         20       8       0       8
    ## 10 #3/5/2/2/95     Con8        28.5        NA         20       8       0       8
    ## # … with 39 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth, ⁴​pups_survive

``` r
relocate(litters_df, litter_number)
```

    ## # A tibble: 49 × 8
    ##    litter_number   group gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr>           <chr>      <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 #85             Con7        19.7        34.7       20       3       4       3
    ##  2 #1/2/95/2       Con7        27          42         19       8       0       7
    ##  3 #5/5/3/83/3-3   Con7        26          41.4       19       6       0       5
    ##  4 #5/4/2/95/2     Con7        28.5        44.1       19       5       1       4
    ##  5 #4/2/95/3-3     Con7        NA          NA         20       6       0       6
    ##  6 #2/2/95/3-2     Con7        NA          NA         20       6       0       4
    ##  7 #1/5/3/83/3-3/2 Con7        NA          NA         20       9       0       9
    ##  8 #3/83/3-3       Con8        NA          NA         20       9       1       8
    ##  9 #2/95/3         Con8        NA          NA         20       8       0       8
    ## 10 #3/5/2/2/95     Con8        28.5        NA         20       8       0       8
    ## # … with 39 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth, ⁴​pups_survive

## ‘filter’

``` r
filter(litters_df, gd0_weight < 22)
```

    ## # A tibble: 8 × 8
    ##   group litter_number gd0_weight gd18_weight gd_of_birth pups_…¹ pups_…² pups_…³
    ##   <chr> <chr>              <dbl>       <dbl>       <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 Con7  #85                 19.7        34.7          20       3       4       3
    ## 2 Mod7  #59                 17          33.4          19       8       0       5
    ## 3 Mod7  #103                21.4        42.1          19       9       1       9
    ## 4 Mod7  #106                21.7        37.8          20       5       0       2
    ## 5 Mod7  #62                 19.5        35.9          19       7       2       4
    ## 6 Low8  #53                 21.8        37.2          20       8       1       7
    ## 7 Low8  #100                20          39.2          20       8       0       7
    ## 8 Low8  #4/84               21.8        35.2          20       4       0       4
    ## # … with abbreviated variable names ¹​pups_born_alive, ²​pups_dead_birth,
    ## #   ³​pups_survive

``` r
filter(litters_df, gd0_weight >= 22)
```

    ## # A tibble: 26 × 8
    ##    group litter_number gd0_weight gd18_weight gd_of_bi…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>              <dbl>       <dbl>      <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #1/2/95/2           27          42           19       8       0       7
    ##  2 Con7  #5/5/3/83/3-3       26          41.4         19       6       0       5
    ##  3 Con7  #5/4/2/95/2         28.5        44.1         19       5       1       4
    ##  4 Con8  #3/5/2/2/95         28.5        NA           20       8       0       8
    ##  5 Con8  #5/4/3/83/3         28          NA           19       9       0       8
    ##  6 Mod7  #3/82/3-2           28          45.9         20       5       0       5
    ##  7 Mod7  #4/2/95/2           23.5        NA           19       9       0       7
    ##  8 Mod7  #5/3/83/5-2         22.6        37           19       5       0       5
    ##  9 Mod7  #94/2               24.4        42.9         19       7       1       3
    ## 10 Low7  #84/2               24.3        40.8         20       8       0       8
    ## # … with 16 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth, ⁴​pups_survive

Whether =20?

``` r
filter(litters_df, gd_of_birth == 20)
```

    ## # A tibble: 32 × 8
    ##    group litter_number   gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>                <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #85                   19.7        34.7       20       3       4       3
    ##  2 Con7  #4/2/95/3-3           NA          NA         20       6       0       6
    ##  3 Con7  #2/2/95/3-2           NA          NA         20       6       0       4
    ##  4 Con7  #1/5/3/83/3-3/2       NA          NA         20       9       0       9
    ##  5 Con8  #3/83/3-3             NA          NA         20       9       1       8
    ##  6 Con8  #2/95/3               NA          NA         20       8       0       8
    ##  7 Con8  #3/5/2/2/95           28.5        NA         20       8       0       8
    ##  8 Con8  #1/6/2/2/95-2         NA          NA         20       7       0       6
    ##  9 Con8  #3/5/3/83/3-3-2       NA          NA         20       8       0       8
    ## 10 Con8  #3/6/2/2/95-3         NA          NA         20       7       0       7
    ## # … with 22 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth, ⁴​pups_survive

``` r
filter(litters_df, !(gd_of_birth == 20))
```

    ## # A tibble: 17 × 8
    ##    group litter_number gd0_weight gd18_weight gd_of_bi…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>              <dbl>       <dbl>      <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #1/2/95/2           27          42           19       8       0       7
    ##  2 Con7  #5/5/3/83/3-3       26          41.4         19       6       0       5
    ##  3 Con7  #5/4/2/95/2         28.5        44.1         19       5       1       4
    ##  4 Con8  #5/4/3/83/3         28          NA           19       9       0       8
    ##  5 Con8  #2/2/95/2           NA          NA           19       5       0       4
    ##  6 Mod7  #59                 17          33.4         19       8       0       5
    ##  7 Mod7  #103                21.4        42.1         19       9       1       9
    ##  8 Mod7  #1/82/3-2           NA          NA           19       6       0       6
    ##  9 Mod7  #3/83/3-2           NA          NA           19       8       0       8
    ## 10 Mod7  #4/2/95/2           23.5        NA           19       9       0       7
    ## 11 Mod7  #5/3/83/5-2         22.6        37           19       5       0       5
    ## 12 Mod7  #94/2               24.4        42.9         19       7       1       3
    ## 13 Mod7  #62                 19.5        35.9         19       7       2       4
    ## 14 Low7  #112                23.9        40.5         19       6       1       1
    ## 15 Mod8  #5/93/2             NA          NA           19       8       0       8
    ## 16 Mod8  #7/110/3-2          27.5        46           19       8       1       8
    ## 17 Low8  #79                 25.4        43.8         19       8       0       7
    ## # … with abbreviated variable names ¹​gd_of_birth, ²​pups_born_alive,
    ## #   ³​pups_dead_birth, ⁴​pups_survive

``` r
filter(litters_df, gd_of_birth != 20)
```

    ## # A tibble: 17 × 8
    ##    group litter_number gd0_weight gd18_weight gd_of_bi…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>              <dbl>       <dbl>      <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #1/2/95/2           27          42           19       8       0       7
    ##  2 Con7  #5/5/3/83/3-3       26          41.4         19       6       0       5
    ##  3 Con7  #5/4/2/95/2         28.5        44.1         19       5       1       4
    ##  4 Con8  #5/4/3/83/3         28          NA           19       9       0       8
    ##  5 Con8  #2/2/95/2           NA          NA           19       5       0       4
    ##  6 Mod7  #59                 17          33.4         19       8       0       5
    ##  7 Mod7  #103                21.4        42.1         19       9       1       9
    ##  8 Mod7  #1/82/3-2           NA          NA           19       6       0       6
    ##  9 Mod7  #3/83/3-2           NA          NA           19       8       0       8
    ## 10 Mod7  #4/2/95/2           23.5        NA           19       9       0       7
    ## 11 Mod7  #5/3/83/5-2         22.6        37           19       5       0       5
    ## 12 Mod7  #94/2               24.4        42.9         19       7       1       3
    ## 13 Mod7  #62                 19.5        35.9         19       7       2       4
    ## 14 Low7  #112                23.9        40.5         19       6       1       1
    ## 15 Mod8  #5/93/2             NA          NA           19       8       0       8
    ## 16 Mod8  #7/110/3-2          27.5        46           19       8       1       8
    ## 17 Low8  #79                 25.4        43.8         19       8       0       7
    ## # … with abbreviated variable names ¹​gd_of_birth, ²​pups_born_alive,
    ## #   ³​pups_dead_birth, ⁴​pups_survive

``` r
filter(litters_df, group == "Mod8")
```

    ## # A tibble: 7 × 8
    ##   group litter_number gd0_weight gd18_weight gd_of_birth pups_…¹ pups_…² pups_…³
    ##   <chr> <chr>              <dbl>       <dbl>       <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 Mod8  #97                 24.5        42.8          20       8       1       8
    ## 2 Mod8  #5/93               NA          41.1          20      11       0       9
    ## 3 Mod8  #5/93/2             NA          NA            19       8       0       8
    ## 4 Mod8  #7/82-3-2           26.9        43.2          20       7       0       7
    ## 5 Mod8  #7/110/3-2          27.5        46            19       8       1       8
    ## 6 Mod8  #2/95/2             28.5        44.5          20       9       0       9
    ## 7 Mod8  #82/4               33.4        52.7          20       8       0       6
    ## # … with abbreviated variable names ¹​pups_born_alive, ²​pups_dead_birth,
    ## #   ³​pups_survive

``` r
filter(litters_df, group %in% c("Con7", "Mod8"))
```

    ## # A tibble: 14 × 8
    ##    group litter_number   gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>                <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #85                   19.7        34.7       20       3       4       3
    ##  2 Con7  #1/2/95/2             27          42         19       8       0       7
    ##  3 Con7  #5/5/3/83/3-3         26          41.4       19       6       0       5
    ##  4 Con7  #5/4/2/95/2           28.5        44.1       19       5       1       4
    ##  5 Con7  #4/2/95/3-3           NA          NA         20       6       0       6
    ##  6 Con7  #2/2/95/3-2           NA          NA         20       6       0       4
    ##  7 Con7  #1/5/3/83/3-3/2       NA          NA         20       9       0       9
    ##  8 Mod8  #97                   24.5        42.8       20       8       1       8
    ##  9 Mod8  #5/93                 NA          41.1       20      11       0       9
    ## 10 Mod8  #5/93/2               NA          NA         19       8       0       8
    ## 11 Mod8  #7/82-3-2             26.9        43.2       20       7       0       7
    ## 12 Mod8  #7/110/3-2            27.5        46         19       8       1       8
    ## 13 Mod8  #2/95/2               28.5        44.5       20       9       0       9
    ## 14 Mod8  #82/4                 33.4        52.7       20       8       0       6
    ## # … with abbreviated variable names ¹​gd_of_birth, ²​pups_born_alive,
    ## #   ³​pups_dead_birth, ⁴​pups_survive

## ‘mutate’

str_to_lower: lowercase

``` r
mutate(
  litters_df, 
  wt_gain = gd18_weight -gd0_weight,
  group = str_to_lower(group))
```

    ## # A tibble: 49 × 9
    ##    group litter_number   gd0_w…¹ gd18_…² gd_of…³ pups_…⁴ pups_…⁵ pups_…⁶ wt_gain
    ##    <chr> <chr>             <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 con7  #85                19.7    34.7      20       3       4       3    15  
    ##  2 con7  #1/2/95/2          27      42        19       8       0       7    15  
    ##  3 con7  #5/5/3/83/3-3      26      41.4      19       6       0       5    15.4
    ##  4 con7  #5/4/2/95/2        28.5    44.1      19       5       1       4    15.6
    ##  5 con7  #4/2/95/3-3        NA      NA        20       6       0       6    NA  
    ##  6 con7  #2/2/95/3-2        NA      NA        20       6       0       4    NA  
    ##  7 con7  #1/5/3/83/3-3/2    NA      NA        20       9       0       9    NA  
    ##  8 con8  #3/83/3-3          NA      NA        20       9       1       8    NA  
    ##  9 con8  #2/95/3            NA      NA        20       8       0       8    NA  
    ## 10 con8  #3/5/2/2/95        28.5    NA        20       8       0       8    NA  
    ## # … with 39 more rows, and abbreviated variable names ¹​gd0_weight,
    ## #   ²​gd18_weight, ³​gd_of_birth, ⁴​pups_born_alive, ⁵​pups_dead_birth,
    ## #   ⁶​pups_survive

\##‘arrange’

``` r
arrange(litters_df, pups_born_alive)
```

    ## # A tibble: 49 × 8
    ##    group litter_number gd0_weight gd18_weight gd_of_bi…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>              <dbl>       <dbl>      <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #85                 19.7        34.7         20       3       4       3
    ##  2 Low7  #111                25.5        44.6         20       3       2       3
    ##  3 Low8  #4/84               21.8        35.2         20       4       0       4
    ##  4 Con7  #5/4/2/95/2         28.5        44.1         19       5       1       4
    ##  5 Con8  #2/2/95/2           NA          NA           19       5       0       4
    ##  6 Mod7  #3/82/3-2           28          45.9         20       5       0       5
    ##  7 Mod7  #5/3/83/5-2         22.6        37           19       5       0       5
    ##  8 Mod7  #106                21.7        37.8         20       5       0       2
    ##  9 Con7  #5/5/3/83/3-3       26          41.4         19       6       0       5
    ## 10 Con7  #4/2/95/3-3         NA          NA           20       6       0       6
    ## # … with 39 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth, ⁴​pups_survive

## ‘%\>%’

``` r
litters_data_row = read_csv("./data/FAS_litters.csv")
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
litters_clean_name = janitor::clean_names(litters_data_row)
litters_data_selected = select(litters_clean_name, -pups_survive)
litters_without_missing = drop_na(litters_data_selected, gd0_weight)
```

use the pipe operator instead

``` r
litters_df =
  read_csv("./data/FAS_litters.csv") %>% 
  janitor::clean_names() %>% 
  select(-pups_survive) %>% 
  drop_na(gd0_weight)
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

\#Tidy data

\##pivot_longer

``` r
pulse_data = 
  haven::read_sas("./data/public_pulse_data.sas7bdat") %>% 
  janitor::clean_names()
```

wide format to long format

``` r
pulse_data_tidy =
  pulse_data %>% 
  pivot_longer(
    bdi_score_bl:bdi_score_12m,
    names_to = "visit",
    names_prefix = "bdi_score_",
    values_to = "bdi"
  )
```

rewrite, combine, and extend (to add a mutate)

``` r
pulse_data = 
  haven::read_sas("./data/public_pulse_data.sas7bdat") %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    bdi_score_bl:bdi_score_12m,
    names_to = "visit",
    names_prefix = "bdi_score_",
    values_to = "bdi"
  ) %>% 
  relocate(id, visit) %>%
  mutate(visit = recode(visit, "bl" = "00m"))
```
