Iron Tolerance in Blood Feeding vs Obligate Non Biting Populations of
the Pitcher Plant Mosquito, Wyeomyia smithii
================
Sophia Bevans

Reproduction of analysis detailed in experimental design given by
Dr. Bill Bradshaw and Dr. Christina Holzapfel in document “Sophia
Experiments II”.

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

### Initial Data Format

``` r
head(pupa_sex_orig)
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

### Pivoted Tables with Formatted Dates:

``` r
#functions and code in external_analysis_code.R
head(pupa_sex)
```

    ## # A tibble: 6 × 5
    ##   Date       Population Treatment Sex    Count
    ##   <date>     <chr>      <chr>     <chr>  <dbl>
    ## 1 2019-12-31 ML         1         male       1
    ## 2 2019-12-31 ML         2         male       1
    ## 3 2020-01-01 KC         1         female     1
    ## 4 2020-01-01 ML         1         male       5
    ## 5 2020-01-01 ML         2         male       5
    ## 6 2020-01-02 KC         0         male       2

### Summary Statistics

``` r
head(pupae_sum)
```

    ## # A tibble: 6 × 3
    ##   Population Treatment total_pupae
    ##   <chr>      <chr>           <dbl>
    ## 1 KC         0                 244
    ## 2 KC         1                 244
    ## 3 KC         2                 243
    ## 4 LI         0                 248
    ## 5 LI         1                 248
    ## 6 LI         2                 248

``` r
head(pupa_sex_sum)
```

    ## # A tibble: 6 × 5
    ##   Population Treatment Sex    total_eclosed mean_eclosion_date
    ##   <chr>      <chr>     <chr>          <dbl> <date>            
    ## 1 KC         0         female           117 2020-01-27        
    ## 2 KC         0         male              97 2020-01-23        
    ## 3 KC         1         female           115 2020-01-26        
    ## 4 KC         1         male             104 2020-01-22        
    ## 5 KC         2         female           114 2020-01-28        
    ## 6 KC         2         male              93 2020-01-21

### Mean Adult Longevity (Mean Eclosion Date - Mean Death Date)

``` r
head(longev)
```

    ##   Population Treatment    Sex longevity Biting_Propensity
    ## 1         KC         0 female  12.01345        Non-Biting
    ## 2         KC         0   male  11.98074        Non-Biting
    ## 3         KC         1 female  11.09565        Non-Biting
    ## 4         KC         1   male  11.57692        Non-Biting
    ## 5         KC         2 female  12.27193        Non-Biting
    ## 6         KC         2   male  11.27735        Non-Biting

## Nested ANOVA, Sex and Longevity in Treatment Group

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

### Mean Eggs and Hatched Eggs Per Eclosed Female, Arcsine Transformed Proportion of Hatched Eggs

``` r
head(eggs_hatch)
```

    ##   Population Treatment EggsPerFemale HatchPerFemale perc_hatched arcsin_ph
    ## 1         KC         0      80.18803       72.83761    0.9083351  1.263207
    ## 2         KC         1      74.70435       65.97391    0.8831335  1.221904
    ## 3         KC         2      77.75439       68.17544    0.8768051  1.212167
    ## 4         LI         0      19.90816       15.64286    0.7857509  1.089566
    ## 5         LI         1      29.86022       22.88172    0.7662946  1.066227
    ## 6         LI         2      24.47273       20.14545    0.8231798  1.136800
    ##   Biting_Propensity
    ## 1        Non-Biting
    ## 2        Non-Biting
    ## 3        Non-Biting
    ## 4            Biting
    ## 5            Biting
    ## 6            Biting

## Nested ANOVA, Fertility, Fecundity, Reproductive Success

### Fertility

``` r
fertility_anova <- aov(arcsin_ph ~ Treatment / Biting_Propensity, data = eggs_hatch)
summary(fertility_anova)
```

    ##                             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Treatment                    2 0.00168 0.000839   0.122  0.888
    ## Treatment:Biting_Propensity  3 0.01035 0.003451   0.502  0.697
    ## Residuals                    5 0.03437 0.006875

### Fecundity

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

### Lifetime Reproductive Success

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
