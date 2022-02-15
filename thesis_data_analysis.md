Iron Tolerance in Blood Feeding vs Obligate Non Biting Populations of
the Pitcher Plant Mosquito, Wyeomyia smithii
================
Sophia Bevans

Reproduction of analysis detailed in experimental design given by
Dr. Bill Bradshaw and Dr. Christina Holzapfel in document “Sophia
Experiments II”.

``` r
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
```

## Data

Wyeomyia smithii populations: KC, ML (non-biting), WI, LI (biting) were
collected from Maine, Wisconsin, Florida, and Alabama, respectively, in
the summer of 2016.  
Experimental groups: 0 = treatment (water), 1 = 13mg/L ferrous sulfate,
2 = 130mg/L ferrous sulfate  
Column names refer to population and experimental group (ex: WI0 is
control group from Florida)

Treatment group ML0 is missing in the data due to issues with
experimental equipment.

pupae contains date of pupation for pupae in each treatment group  
pupa_sex contains sex and eclosion date of pupae in each treatment
group  
adult_sex contains sex and date of death for adults in each treatment
group  
eggs contains date and counts of eggs from each treatment group  
hatch counts contains date and counts of hatch from each treatment group

``` r
pupae <- read_csv("iron_tolerance_pupae.csv")
pupa_sex <- read_csv("iron_tolerance_pupa_sex.csv")
adult_sex <- read_csv("iron_tolerance_adult_sex.csv")
eggs <- read_csv("iron_tolerance_egg_counts.csv")
hatch <- read_csv("iron_tolerance_hatch_counts.csv")
```

``` r
head(pupa_sex)
```

    ## # A tibble: 6 × 23
    ##   `Date of Eclosion` KC0_male KC0_female KC1_male KC1_female KC2_male KC2_female
    ##   <chr>                 <dbl>      <dbl>    <dbl>      <dbl>    <dbl>      <dbl>
    ## 1 12/31/19                 NA         NA       NA         NA       NA         NA
    ## 2 1/1/20                   NA         NA       NA          1       NA         NA
    ## 3 1/2/20                    2         NA       NA         NA       NA         NA
    ## 4 1/3/20                   NA         NA        1         NA        1         NA
    ## 5 1/4/20                    2         NA        1         NA        1          1
    ## 6 1/5/20                    1         NA        1          1        2         NA
    ## # … with 16 more variables: ML1_male <dbl>, ML1_female <dbl>, ML2_male <dbl>,
    ## #   ML2_female <dbl>, WI0_male <dbl>, WI0_female <dbl>, WI1_male <dbl>,
    ## #   WI1_female <dbl>, WI2_male <dbl>, WI2_female <dbl>, LI0_male <dbl>,
    ## #   LI0_female <dbl>, LI1_male <dbl>, LI1_female <dbl>, LI2_male <dbl>,
    ## #   LI2_female <dbl>

``` r
pivot_no_sex <- function(df) {
  df %>% 
  mutate(Date = mdy(Date)) %>%
  pivot_longer(cols = matches("..[0-2]"), names_to = c("Population", "Treatment"),
               names_sep = 2, values_to = "Count", values_drop_na = TRUE)
}

pivot_sex <- function(df) {
  df %>%
  mutate(Date = mdy(Date)) %>%
  pivot_longer(cols = matches("..[0-2]_*"), 
               names_to = c("Population", "Treatment", "Sex"), 
               names_pattern = "([A-Z]{2})([0-2])_([e-f]*male)", 
               values_to = "Count", values_drop_na = TRUE)
}
```

Pivot tables and format Date column:

``` r
pupae <- pivot_no_sex(pupae)
pupa_sex <- pupa_sex %>% 
  rename(Date = `Date of Eclosion`) %>%
  pivot_sex()
(adult_sex <- adult_sex %>%
  rename(Date = `Date of Death`) %>%
  pivot_sex())
```

    ## # A tibble: 614 × 5
    ##    Date       Population Treatment Sex    Count
    ##    <date>     <chr>      <chr>     <chr>  <dbl>
    ##  1 2020-01-03 ML         2         male       1
    ##  2 2020-01-05 ML         1         male       1
    ##  3 2020-01-05 ML         2         male       1
    ##  4 2020-01-07 ML         1         male       1
    ##  5 2020-01-07 ML         1         female     1
    ##  6 2020-01-08 KC         1         male       1
    ##  7 2020-01-08 ML         1         male       2
    ##  8 2020-01-08 ML         2         male       2
    ##  9 2020-01-10 ML         1         male       8
    ## 10 2020-01-10 ML         2         male       3
    ## # … with 604 more rows

``` r
eggs <- pivot_no_sex(eggs)
hatch <- pivot_no_sex(hatch)
```

## Summary Statistics

``` r
(pupae_sum <- pupae %>% 
  group_by(Population, Treatment) %>%
  summarise(total_pupae = sum(Count)) %>%
   ungroup())
```

    ## # A tibble: 11 × 3
    ##    Population Treatment total_pupae
    ##    <chr>      <chr>           <dbl>
    ##  1 KC         0                 244
    ##  2 KC         1                 244
    ##  3 KC         2                 243
    ##  4 LI         0                 248
    ##  5 LI         1                 248
    ##  6 LI         2                 248
    ##  7 ML         1                 250
    ##  8 ML         2                 250
    ##  9 WI         0                 248
    ## 10 WI         1                 248
    ## 11 WI         2                 248

``` r
(eggs_sum <- eggs %>%
  group_by(Population, Treatment) %>%
  summarize(total_eggs = sum(Count)) %>%
    ungroup())
```

    ## # A tibble: 11 × 3
    ##    Population Treatment total_eggs
    ##    <chr>      <chr>          <dbl>
    ##  1 KC         0               9382
    ##  2 KC         1               8591
    ##  3 KC         2               8864
    ##  4 LI         0               1951
    ##  5 LI         1               2777
    ##  6 LI         2               2692
    ##  7 ML         1               4168
    ##  8 ML         2               5226
    ##  9 WI         0               3419
    ## 10 WI         1               2223
    ## 11 WI         2               3217

``` r
(hatch_sum <- hatch %>%
  group_by(Population, Treatment) %>%
  summarize(total_hatch = sum(Count)) %>%
    ungroup())
```

    ## # A tibble: 11 × 3
    ##    Population Treatment total_hatch
    ##    <chr>      <chr>           <dbl>
    ##  1 KC         0                8522
    ##  2 KC         1                7587
    ##  3 KC         2                7772
    ##  4 LI         0                1533
    ##  5 LI         1                2128
    ##  6 LI         2                2216
    ##  7 ML         1                3582
    ##  8 ML         2                4701
    ##  9 WI         0                3079
    ## 10 WI         1                1993
    ## 11 WI         2                2862

``` r
(pupa_sex_sum <- pupa_sex %>%
  group_by(Population, Treatment, Sex) %>%
  summarise(total_eclosed = sum(Count), 
            mean_eclosion_date = weighted.mean(Date, Count/total_eclosed)) %>%
   ungroup())
```

    ## `summarise()` has grouped output by 'Population', 'Treatment'. You can override using the `.groups` argument.

    ## # A tibble: 22 × 5
    ##    Population Treatment Sex    total_eclosed mean_eclosion_date
    ##    <chr>      <chr>     <chr>          <dbl> <date>            
    ##  1 KC         0         female           117 2020-01-27        
    ##  2 KC         0         male              97 2020-01-23        
    ##  3 KC         1         female           115 2020-01-26        
    ##  4 KC         1         male             104 2020-01-22        
    ##  5 KC         2         female           114 2020-01-28        
    ##  6 KC         2         male              93 2020-01-21        
    ##  7 LI         0         female            98 2020-01-30        
    ##  8 LI         0         male             146 2020-01-25        
    ##  9 LI         1         female            93 2020-01-29        
    ## 10 LI         1         male             147 2020-01-26        
    ## # … with 12 more rows

``` r
(adult_sex_sum <- adult_sex %>%
  group_by(Population, Treatment, Sex) %>%
  summarise(total_dead = sum(Count), 
            mean_death_date = weighted.mean(Date, Count/total_dead)) %>%
   ungroup())
```

    ## `summarise()` has grouped output by 'Population', 'Treatment'. You can override using the `.groups` argument.

    ## # A tibble: 22 × 5
    ##    Population Treatment Sex    total_dead mean_death_date
    ##    <chr>      <chr>     <chr>       <dbl> <date>         
    ##  1 KC         0         female        115 2020-02-08     
    ##  2 KC         0         male           91 2020-02-04     
    ##  3 KC         1         female        115 2020-02-06     
    ##  4 KC         1         male          104 2020-02-03     
    ##  5 KC         2         female        114 2020-02-09     
    ##  6 KC         2         male           92 2020-02-01     
    ##  7 LI         0         female         90 2020-02-13     
    ##  8 LI         0         male          115 2020-02-06     
    ##  9 LI         1         female         92 2020-02-11     
    ## 10 LI         1         male          130 2020-02-05     
    ## # … with 12 more rows

### Calculate Mean Adult Longevity (Mean Eclosion Date - Mean Death Date)

``` r
(longev <- merge(pupa_sex_sum, adult_sex_sum, by = c("Population", "Treatment", "Sex")) %>% mutate(longevity = as.numeric(mean_death_date - mean_eclosion_date)) %>%
  select(-c(total_eclosed, total_dead, mean_eclosion_date, mean_death_date)) %>% 
  mutate(Treatment = factor(Treatment),
         Sex = factor(Sex),
         Biting_Propensity = factor(case_when(
           Population %in% c("KC", "ML") ~ "Non-Biting",
           Population %in% c("LI", "WI") ~ "Biting"))))
```

    ##    Population Treatment    Sex longevity Biting_Propensity
    ## 1          KC         0 female 12.013452        Non-Biting
    ## 2          KC         0   male 11.980741        Non-Biting
    ## 3          KC         1 female 11.095652        Non-Biting
    ## 4          KC         1   male 11.576923        Non-Biting
    ## 5          KC         2 female 12.271930        Non-Biting
    ## 6          KC         2   male 11.277349        Non-Biting
    ## 7          LI         0 female 14.544898            Biting
    ## 8          LI         0   male 11.136748            Biting
    ## 9          LI         1 female 12.690743            Biting
    ## 10         LI         1   male 10.064050            Biting
    ## 11         LI         2 female 16.526136            Biting
    ## 12         LI         2   male 13.155591            Biting
    ## 13         ML         1 female  9.507003        Non-Biting
    ## 14         ML         1   male  7.630256        Non-Biting
    ## 15         ML         2 female 10.026003        Non-Biting
    ## 16         ML         2   male  8.142137        Non-Biting
    ## 17         WI         0 female 19.460385            Biting
    ## 18         WI         0   male 16.326523            Biting
    ## 19         WI         1 female 12.223188            Biting
    ## 20         WI         1   male 11.069700            Biting
    ## 21         WI         2 female 16.845269            Biting
    ## 22         WI         2   male 12.732749            Biting

## Nested ANOVA, Sex and Longevity in each group

``` r
longev_anova <- aov(longevity ~ Treatment / Biting_Propensity / Sex, data = longev)
summary(longev_anova)
```

    ##                                 Df Sum Sq Mean Sq F value Pr(>F)  
    ## Treatment                        2  43.03  21.517   5.028 0.0308 *
    ## Treatment:Biting_Propensity      3  58.47  19.491   4.555 0.0293 *
    ## Treatment:Biting_Propensity:Sex  6  30.83   5.138   1.201 0.3797  
    ## Residuals                       10  42.79   4.279                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### Eggs and Hatch Per Eclosed Female, Arcsin Transform % Hatched Eggs

``` r
(eggs_hatch <- pupa_sex_sum %>% 
  filter(Sex == "female") %>% 
  merge(eggs_sum, by = c("Population", "Treatment")) %>%
   merge(hatch_sum, by = c("Population", "Treatment")) %>%
  mutate(EggsPerFemale = total_eggs / total_eclosed, 
         HatchPerFemale = total_hatch / total_eclosed, 
         perc_hatched = total_hatch / total_eggs,
         arcsin_ph = asin(sqrt(perc_hatched))) %>%
  select(Population, Treatment, EggsPerFemale, HatchPerFemale, perc_hatched, arcsin_ph) %>%
  mutate(Treatment = factor(Treatment),
         Biting_Propensity = factor(case_when(
           Population %in% c("KC", "ML") ~ "Non-Biting",
           Population %in% c("LI", "WI") ~ "Biting"))))
```

    ##    Population Treatment EggsPerFemale HatchPerFemale perc_hatched arcsin_ph
    ## 1          KC         0      80.18803       72.83761    0.9083351  1.263207
    ## 2          KC         1      74.70435       65.97391    0.8831335  1.221904
    ## 3          KC         2      77.75439       68.17544    0.8768051  1.212167
    ## 4          LI         0      19.90816       15.64286    0.7857509  1.089566
    ## 5          LI         1      29.86022       22.88172    0.7662946  1.066227
    ## 6          LI         2      24.47273       20.14545    0.8231798  1.136800
    ## 7          ML         1      40.86275       35.11765    0.8594050  1.186443
    ## 8          ML         2      51.74257       46.54455    0.8995408  1.248281
    ## 9          WI         0      30.52679       27.49107    0.9005557  1.249973
    ## 10         WI         1      19.33043       17.33043    0.8965362  1.243316
    ## 11         WI         2      27.03361       24.05042    0.8896487  1.232170
    ##    Biting_Propensity
    ## 1         Non-Biting
    ## 2         Non-Biting
    ## 3         Non-Biting
    ## 4             Biting
    ## 5             Biting
    ## 6             Biting
    ## 7         Non-Biting
    ## 8         Non-Biting
    ## 9             Biting
    ## 10            Biting
    ## 11            Biting

## Nested ANOVA, Fertility, Fecundity, Reproductive Success

``` r
fertility_anova <- aov(arcsin_ph ~ Treatment / Biting_Propensity, data = eggs_hatch)
summary(fertility_anova)
```

    ##                             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Treatment                    2 0.00168 0.000839   0.122  0.888
    ## Treatment:Biting_Propensity  3 0.01035 0.003451   0.502  0.697
    ## Residuals                    5 0.03437 0.006875

``` r
fecundity_anova <- aov(log(EggsPerFemale) ~ Treatment / Biting_Propensity, data = eggs_hatch)
summary(fecundity_anova)
```

    ##                             Df Sum Sq Mean Sq F value Pr(>F)  
    ## Treatment                    2 0.0265  0.0133   0.145 0.8682  
    ## Treatment:Biting_Propensity  3 2.4357  0.8119   8.906 0.0189 *
    ## Residuals                    5 0.4558  0.0912                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
repsuccess_anova <- aov(log(HatchPerFemale) ~ Treatment / Biting_Propensity, data = eggs_hatch)
summary(repsuccess_anova)
```

    ##                             Df Sum Sq Mean Sq F value Pr(>F)  
    ## Treatment                    2 0.0378  0.0189   0.195 0.8288  
    ## Treatment:Biting_Propensity  3 2.7141  0.9047   9.329 0.0172 *
    ## Residuals                    5 0.4849  0.0970                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Figures

``` r
longev %>% 
  ggplot() + geom_point(aes(x = Treatment, y = longevity, shape = Sex, color = Biting_Propensity), size = 8, stroke = 1.25, position = position_dodge(width = 0.5)) + 
  labs(x = "Iron Treatment", y = "Mean Longevity (Days)", title =  "Mean Adult Longevity of Males and Females", caption = "Ferrous sulfate concentrations:\n0: 0mg/mL, 1: 13mg/mL, 2: 130 mg/mL.\nSignificance of nested ANOVA is given by: *P<0.05") + 
  theme_classic() + scale_shape_manual(values = c(1, 2)) + 
  scale_color_manual(values = c("red", "dodgerblue")) + ylim(0, 20) + 
  annotate("text", x = 0.75, y = 2, label = expression(atop(Fe^"*",
                                                    Biting^"*")))
```

![](thesis_data_analysis_files/figure-gfm/longevity%20figure-1.png)<!-- -->

``` r
eggs_hatch %>% 
  ggplot() + geom_point(aes(x = Treatment, y = log10(EggsPerFemale), color = Biting_Propensity), shape = 1, size = 8, stroke = 1.25, position = position_dodge(width = 0.5)) + 
  labs(x = "Iron Treatment", y = "Log(Eggs per Female)", title =  "Lifetime Fecundity", subtitle = "Eggs Per Eclosed Females", caption = "Ferrous sulfate concentrations:\n0: 0mg/mL, 1: 13mg/mL, 2: 130 mg/mL.\nSignificance of nested ANOVA is given by: *P<0.05") + 
  theme_classic() + 
  scale_color_manual(values = c("red", "dodgerblue")) + ylim(0, 2) + 
  annotate("text", x = 0.75, y = 0.25, label = expression(Biting^"*"))
```

![](thesis_data_analysis_files/figure-gfm/fecundity%20figure-1.png)<!-- -->

``` r
eggs_hatch %>% 
  ggplot() + geom_point(aes(x = Treatment, y = log10(HatchPerFemale), color = Biting_Propensity), shape = 1, size = 8, stroke = 1.25, position = position_dodge(width = 0.5)) + 
  labs(x = "Iron Treatment", y = "Log(Hatch per Female)", title =  "Lifetime Reproductive Success", subtitle = "Hatch Per Eclosed Females", caption = "Ferrous sulfate concentrations:\n0: 0mg/mL, 1: 13mg/mL, 2: 130 mg/mL.\nSignificance of nested ANOVA is given by: *P<0.05") + 
  theme_classic() + 
  scale_color_manual(values = c("red", "dodgerblue")) + ylim(0, 2) + 
  annotate("text", x = 0.75, y = 0.25, label = expression(Biting^"*"))
```

![](thesis_data_analysis_files/figure-gfm/reproductive%20success%20figure-1.png)<!-- -->

``` r
eggs_hatch %>% 
  ggplot() + geom_point(aes(x = Treatment, y = arcsin_ph, color = Biting_Propensity), shape = 1, size = 8, stroke = 1.25, position = position_dodge(width = 0.5)) + 
  labs(x = "Iron Treatment", y = "Arcsine Transformed Frequency Hatched", title =  "Fertility", subtitle = "Frequency of Eggs Hatched", caption = "Ferrous sulfate concentrations:\n0: 0mg/mL, 1: 13mg/mL, 2: 130 mg/mL.") + 
  theme_classic() + 
  scale_color_manual(values = c("red", "dodgerblue")) + ylim(0, 2)
```

![](thesis_data_analysis_files/figure-gfm/fertility%20figure-1.png)<!-- -->
