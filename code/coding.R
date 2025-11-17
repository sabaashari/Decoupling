library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(scales)
library(tidyr)
library(rlang)
library(hash)
library(grid)
library(openxlsx)
library(texreg)
library(lmtest)
library(sandwich)
library(car)
library(caret)
library(Hmisc)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read_csv("../Data/G20_solidarity_agency.csv")
#mapping <- read.xlsx("Data/SAGE_code_book.xlsx")

GDP_per_capita <- read.xlsx("../Data/GDP-2006-2023.xlsx") 
GDP_per_capita <- GDP_per_capita %>% 
  pivot_longer(
  cols = colnames(GDP_per_capita)[5:22], 
  names_to = "Year",
  values_to = "GDP"
)
GDP_growth <- read.xlsx("../Data/GDP_growth.xlsx")
GDP_growth <- GDP_growth %>% 
  pivot_longer(
  cols = colnames(GDP_growth)[5:22], 
  names_to = "Year",
  values_to = "growth"
) 
GDP_per_capita <- GDP_per_capita %>% left_join(GDP_growth, by = c("Country.Name","Country.Code", "Year" ))
GDP_per_capita$Year <- as.double(GDP_per_capita$Year)

G20_countries <- c("Argentina", "Australia", "Brazil", "Canada", "China", "France", 
                   "Germany", "India", "Indonesia", "Italy", "Japan", "Mexico", "Russia", 
                   "Saudi Arabia", "South Africa", "South Korea", "Turkey", 
                   "United Kingdom", "United States")
income_group_long <- read.csv("../Data/world-bank-income-groups.csv")%>% 
  filter(Year >= 2006, Entity %in% G20_countries)
income_group <- income_group_long %>% group_by (Entity) %>% 
  summarise( income_group = names(which.max(table(World.Bank.s.income.classification))))
GDP_per_capita <- GDP_per_capita %>% left_join(income_group_long, by = c("Country.Name" = "Entity","Year" ))


solidarity <- c("WP27", "WP110", "WP108", "WP109", "WP103", "WP105", "WP106", "WP61", "WP129")
Agency <- c("WP138", "WP139", "WP146", "WP134", "WP93", "WP97", "WP98", "WP92", "WP91")


# 1:Yes, 2: No, 3: DK,  4:Refused
# 1:Satisfied, 2:unsatisfied, 3: DK, 4: Refused, 
## Encode Solidarity and Agency indices ####
dd <- df %>% dplyr:: mutate (across(all_of(c(solidarity, Agency[-3])), 
                                    ~ case_when(
                                      .%in% c("Yes", "Good place", "Satisfied")   ~ 1,
                                      .%in% c("No", "Not a good place", "Dissatisfied")  ~ -1,
                                      .%in% c("(DK)")  ~ 0,
                                      .%in% c("(Refused)")  ~ 0,
                                      TRUE ~ NA_integer_
                                    )))
dd2 <- df %>% dplyr:: mutate(across(all_of(c(solidarity, Agency[-3])), 
                                    ~ case_when(
                                      .%in% c("Yes", "Good place", "Satisfied")   ~ 2,
                                      .%in% c("No", "Not a good place", "Dissatisfied")  ~ -2,
                                      .%in% c("(DK)")  ~ 0,
                                      .%in% c("(Refused)")  ~ -1,
                                      TRUE ~ NA_integer_
                                    )))
dd3 <- df %>% dplyr:: mutate(across(all_of(c(solidarity, Agency[-3])), 
                                    ~ case_when(
                                      .%in% c("Yes", "Good place", "Satisfied")   ~ 1,
                                      .%in% c("No", "Not a good place", "Dissatisfied")  ~ -1,
                                      .%in% c("(DK)")  ~ 0,
                                      .%in% c("(Refused)")  ~ -1,
                                      TRUE ~ NA_integer_
                                    )))

dd$solidarity_score_1 <- rowSums(dd[,solidarity], na.rm = TRUE)
dd$solidarity_score_2 <- rowSums(dd2[,solidarity], na.rm = TRUE)
dd$solidarity_score_3 <- rowSums(dd3[,solidarity], na.rm = TRUE)
dd$solidarity_score_1 <- ifelse(rowSums(is.na(dd[, solidarity]))==9, NA_integer_, dd$solidarity_score_1)
dd$solidarity_score_2 <- ifelse(rowSums(is.na(dd2[, solidarity]))==9, NA_integer_, dd$solidarity_score_2)
dd$solidarity_score_3 <- ifelse(rowSums(is.na(dd3[, solidarity]))==9, NA_integer_, dd$solidarity_score_3)


dd$agency_score_1  <- rowSums(dd[,Agency], na.rm = TRUE)
dd$agency_score_2 <- rowSums(dd2[,Agency], na.rm = TRUE)
dd$agency_score_3 <- rowSums(dd3[,Agency], na.rm = TRUE)
dd$agency_score_1 <- ifelse(rowSums(is.na(dd[,Agency]))==9, NA_integer_, dd$agency_score_1)
dd$agency_score_2 <- ifelse(rowSums(is.na(dd[,Agency]))==9, NA_integer_, dd$agency_score_2)
dd$agency_score_3 <- ifelse(rowSums(is.na(dd[,Agency]))==9, NA_integer_, dd$agency_score_3)

write.csv(dd, "../Data/cleaned_G20.csv")
write.csv(GDP_per_capita %>% select(-Code), "../Data/GDP_growth.csv")

## Three different coding types, correlations between GDP and solidarity, per country, per year across all countries. 
# I can also depict that for each year. I did it for each country, 
# I can do it for each year. I can also make correlation lines instead of time series for each country. 

### per country/year indices ####
# standard deviation, coefficient of variation #
# successive difference, z-score normalization #
avg_country_year <- dd %>% group_by(COUNTRYNEW, YEAR_WAVE) %>% 
  summarise(mean_solidarity_1 = weighted.mean(solidarity_score_1,WGT, na.rm = TRUE), 
            mean_solidarity_2 = weighted.mean(solidarity_score_2, WGT, na.rm = TRUE),
            mean_solidarity_3 = weighted.mean(solidarity_score_3, WGT, na.rm = TRUE),
            solidarity_sd = sqrt(sum(WGT*(solidarity_score_1 - weighted.mean(solidarity_score_1,WGT))^2)/sum(WGT)),
            solidarity_max = max(solidarity_score_1), 
            solidarity_min = min(solidarity_score_1),
            agency_score_1 = weighted.mean(agency_score_1,WGT, na.rm = TRUE), 
            agency_score_2 = weighted.mean(agency_score_2,WGT, na.rm = TRUE), 
            agency_score_3 = weighted.mean(agency_score_3,WGT, na.rm = TRUE)) %>% 
  left_join(GDP_per_capita,  by = c("COUNTRYNEW" = "Country.Name", "YEAR_WAVE" = "Year")) 

avg_country_year <- avg_country_year %>% ungroup() %>%
  mutate(GDP_z = (GDP - mean(GDP))/sd(GDP) , 
         agency_z_1 =  (agency_score_1 - mean(agency_score_1, na.rm = TRUE))/sd(agency_score_1, na.rm = TRUE), 
         solidarity_z_1 = (mean_solidarity_1 - mean(mean_solidarity_1))/sd(mean_solidarity_1), 
         solidarity_z_2 = (mean_solidarity_2 - mean(mean_solidarity_2, na.rm = TRUE))/sd(mean_solidarity_2, na.rm = TRUE), 
         agency_z_2 = (agency_score_2 - mean(agency_score_2, na.rm = TRUE))/sd(agency_score_2, na.rm = TRUE))

avg_country_year <- avg_country_year %>% ungroup() %>% group_by(COUNTRYNEW) %>% 
  mutate(growth_solidarity =  c(diff(mean_solidarity_1), NA)/ mean_solidarity_1, 
         growth_agency =  c(diff(agency_score_1), NA)/ agency_score_1)
write.xlsx(avg_country_year, "../Data/average_country_year.xlsx")

### T-tests for different coding styles ####
corr_per_country <- avg_country_year %>% ungroup() %>% group_by(COUNTRYNEW) %>% 
  summarise(cor_sol_1 = cor(mean_solidarity_1, GDP, use = "complete.obs"),
            cor_sol_2 = cor(mean_solidarity_2, GDP, use = "complete.obs"),
            cor_sol_3 = cor(mean_solidarity_3, GDP, use = "complete.obs"),
            cor_agency_1 = cor(agency_score_1, GDP, use = "complete.obs"), 
            cor_agency_2 = cor(agency_score_2, GDP, use = "complete.obs"), 
            cor_agency_3 = cor(agency_score_3, GDP, use = "complete.obs"), 
            cor_sol_growth_1 = cor(mean_solidarity_1, growth, use = "complete.obs"), 
            cor_sol_growth_2 = cor(growth_solidarity, growth, use = "complete.obs"),
            cor_Agency_growth_1 = cor(agency_score_1, growth, use = "complete.obs"),
            cor_Agency_growth_2 = cor(growth_agency, growth, use = "complete.obs"),
            D_S = 1-cor_sol_1,
            D_S_1 = 1- cor_sol_growth_1, 
            D_S_2 = 1- cor_sol_growth_2,
            D_A = 1- cor_agency_1, 
            D_A_1 = 1- cor_Agency_growth_1, 
            D_A_2 = 1- cor_Agency_growth_2, 
            D = (D_A+D_S)/2
  )
t <- t.test(corr_per_country$cor_sol_1 - corr_per_country$cor_sol_2, mu = 0 )
t.test(corr_per_country$cor_sol_1 - corr_per_country$cor_sol_3, mu = 0 )
t.test(corr_per_country$cor_agency_1 - corr_per_country$cor_agency_2, mu = 0)
t.test(corr_per_country$cor_agency_1 - corr_per_country$cor_agency_3, mu = 0)
t.test(corr_across_countries$cor_agency_1 - corr_across_countries$cor_agency_3, mu = 0)
t.test(corr_across_countries$cor_sol_1 - corr_across_countries$cor_sol_3, mu = 0)
t.test(corr_across_countries$cor_sol_1 - corr_across_countries$cor_sol_2, mu = 0 )

