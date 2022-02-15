suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))

#functions to pivot tibbles and fix dates
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

#read in initial data from csv files
pupae <- read_csv("data/iron_tolerance_pupae.csv")
pupa_sex <- read_csv("data/iron_tolerance_pupa_sex.csv")
adult_sex <- read_csv("data/iron_tolerance_adult_sex.csv")
eggs <- read_csv("data/iron_tolerance_egg_counts.csv")
hatch <- read_csv("data/iron_tolerance_hatch_counts.csv")

pupa_sex_orig <- pupa_sex

#pivot tables and format Date column
pupae <- pivot_no_sex(pupae)
pupa_sex <- pupa_sex %>% 
  rename(Date = `Date of Eclosion`) %>%
  pivot_sex()
(adult_sex <- adult_sex %>%
    rename(Date = `Date of Death`) %>%
    pivot_sex())
eggs <- pivot_no_sex(eggs)
hatch <- pivot_no_sex(hatch)

#compute summary statistics by population, and treatment
pupae_sum <- pupae %>% 
    group_by(Population, Treatment) %>%
    summarise(total_pupae = sum(Count)) %>%
    ungroup()

eggs_sum <- eggs %>%
    group_by(Population, Treatment) %>%
    summarize(total_eggs = sum(Count)) %>%
    ungroup()

hatch_sum <- hatch %>%
    group_by(Population, Treatment) %>%
    summarize(total_hatch = sum(Count)) %>%
    ungroup()

#compute summary statistics by population, treatment, and sex
pupa_sex_sum <- pupa_sex %>%
  group_by(Population, Treatment, Sex) %>%
  summarise(total_eclosed = sum(Count), 
            mean_eclosion_date = weighted.mean(Date, Count/total_eclosed)) %>%
  ungroup()

adult_sex_sum <- adult_sex %>%
    group_by(Population, Treatment, Sex) %>%
    summarise(total_dead = sum(Count), 
              mean_death_date = weighted.mean(Date, Count/total_dead)) %>%
    ungroup()

#calculate mean adult longevity by population, treatment, sex
longev <- merge(pupa_sex_sum, adult_sex_sum, by = c("Population", "Treatment", "Sex")) %>% mutate(longevity = as.numeric(mean_death_date - mean_eclosion_date)) %>%
  select(-c(total_eclosed, total_dead, mean_eclosion_date, mean_death_date)) %>% 
  mutate(Treatment = factor(Treatment),
         Sex = factor(Sex),
         Biting_Propensity = factor(case_when(
           Population %in% c("KC", "ML") ~ "Non-Biting",
           Population %in% c("LI", "WI") ~ "Biting")))

#calculate eggs per female, hatch per female, frequency hatched eggs, and arcsine transformed frequency 
eggs_hatch <- pupa_sex_sum %>% 
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
           Population %in% c("LI", "WI") ~ "Biting")))