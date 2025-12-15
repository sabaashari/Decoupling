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

trade <- read.xlsx("../Data/trade.xlsx")
trade <- trade %>% 
  pivot_longer(
    cols = colnames(trade)[5:22], 
    names_to = "Year",
    values_to = "trade") 

GDP_per_capita <- GDP_per_capita %>% 
  left_join(GDP_growth, by = c("Country.Name","Country.Code", "Year" )) %>%
  left_join(trade, by = c("Country.Name","Country.Code", "Year" ))

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
## Encode Solidarity and Agency indices ####
dd <- df %>% dplyr:: mutate (across(all_of(c(solidarity, Agency)), 
                                    ~ case_when(
                                      .%in% c("Yes", "Good place", "Satisfied",1)   ~ 1,
                                      .%in% c("No", "Not a good place", "Dissatisfied", 2)  ~ -1,
                                      .%in% c("(DK)",3)  ~ 0,
                                      .%in% c("(Refused)",4)  ~ 0,
                                      TRUE ~ NA_integer_
                                    )))
dd2 <- df %>% dplyr:: mutate(across(all_of(c(solidarity, Agency)), 
                                    ~ case_when(
                                      .%in% c("Yes", "Good place", "Satisfied",1)   ~ 2,
                                      .%in% c("No", "Not a good place", "Dissatisfied",2)  ~ -2,
                                      .%in% c("(DK)",3)  ~ 0,
                                      .%in% c("(Refused)",4)  ~ -1,
                                      TRUE ~ NA_integer_
                                    )))
dd3 <- df %>% dplyr:: mutate(across(all_of(c(solidarity, Agency)), 
                                    ~ case_when(
                                      .%in% c("Yes", "Good place", "Satisfied",1)   ~ 1,
                                      .%in% c("No", "Not a good place", "Dissatisfied",2)  ~ -1,
                                      .%in% c("(DK)",3)  ~ 0,
                                      .%in% c("(Refused)",4)  ~ -1,
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
  summarise(solidarity_1 = weighted.mean(solidarity_score_1,WGT, na.rm = TRUE), 
            solidarity_2 = weighted.mean(solidarity_score_2, WGT, na.rm = TRUE),
            solidarity_3 = weighted.mean(solidarity_score_3, WGT, na.rm = TRUE),
            solidarity_sd = sqrt(sum(WGT*(solidarity_score_1 - weighted.mean(solidarity_score_1,WGT))^2)/sum(WGT)),
            solidarity_max = max(solidarity_score_1), 
            solidarity_min = min(solidarity_score_1),
            agency_1 = weighted.mean(agency_score_1,WGT, na.rm = TRUE), 
            agency_2 = weighted.mean(agency_score_2,WGT, na.rm = TRUE), 
            agency_3 = weighted.mean(agency_score_3,WGT, na.rm = TRUE)) %>% 
  left_join(GDP_per_capita,  by = c("COUNTRYNEW" = "Country.Name", "YEAR_WAVE" = "Year")) %>% ungroup() 


to_save <- avg_country_year %>% ungroup() %>% select(-c(solidarity_2, solidarity_3, 
                                          solidarity_sd,solidarity_max, solidarity_min, agency_2, agency_3))  
colnames(to_save)[3:4] <- c("solidarity", "agency")
colnames(to_save)[8] <- "GDP_per_capita"
write.xlsx(to_save, "../Data/average_country_year.xlsx")

### T-tests for different coding styles ####
corr_per_country <- avg_country_year %>% ungroup() %>% group_by(COUNTRYNEW) %>% 
  summarise(cor_sol_1 = cor(solidarity_1, GDP, use = "complete.obs"),
            cor_sol_2 = cor(solidarity_2, GDP, use = "complete.obs"),
            cor_sol_3 = cor(solidarity_3, GDP, use = "complete.obs"),
            cor_agency_1 = cor(agency_1, GDP, use = "complete.obs"), 
            cor_agency_2 = cor(agency_2, GDP, use = "complete.obs"), 
            cor_agency_3 = cor(agency_3, GDP, use = "complete.obs"), 
            cor_sol_growth_1 = cor(solidarity_1, growth, use = "complete.obs"), 
            cor_sol_growth_2 = cor(solidarity_2, growth, use = "complete.obs"),
            cor_sol_growth_3 = cor(solidarity_3, growth, use = "complete.obs"),
            cor_Agency_growth_1 = cor(agency_1, growth, use = "complete.obs"),
            cor_Agency_growth_2 = cor(agency_2, growth, use = "complete.obs"),
            cor_Agency_growth_3 = cor(agency_3, growth, use = "complete.obs"),
            D_S = 1-cor_sol_1,
            D_S_1 = 1- cor_sol_growth_1, 
            D_S_2 = 1- cor_sol_growth_2,
            D_A = 1- cor_agency_1, 
            D_A_1 = 1- cor_Agency_growth_1, 
            D_A_2 = 1- cor_Agency_growth_2, 
            D = (D_A+D_S)/2
  )
write.xlsx(corr_per_country, "../Report/country_statistics.xlsx")

corr_across_countries <- avg_country_year %>% ungroup() %>% group_by(YEAR_WAVE) %>% 
  summarise(cor_sol_1 = cor(solidarity_1, GDP, use = "complete.obs"),
            cor_sol_2 = cor(solidarity_2, GDP, use = "complete.obs"),
            cor_sol_3 = cor(solidarity_3, GDP, use = "complete.obs"),
            cor_agency_1 = cor(agency_1, GDP, use = "complete.obs"), 
            cor_agency_2 = cor(agency_2, GDP, use = "complete.obs"),
            cor_agency_3 = cor(agency_3, GDP, use = "complete.obs"),
            cor_sol_growth = cor(solidarity_1, growth, use = "pairwise.complete.obs"), 
            cor_agency_growth = cor(agency_1, growth, use = "pairwise.complete.obs"), 
            D_S = 1- cor_sol_1, 
            D_A = 1- cor_agency_1, 
            D = (D_S + D_A)/2, 
            D_S_growth = 1 - cor_sol_growth, 
            D_A_growth = 1 - cor_agency_growth,
            D_growth = (D_S_growth + D_A_growth)/2)

write.xlsx(corr_across_countries, "../Report/across_country_statistics.xlsx")


t.test(corr_per_country$cor_sol_1, corr_per_country$cor_sol_2 )
t.test(corr_per_country$cor_sol_1, corr_per_country$cor_sol_3)
t.test(corr_per_country$cor_agency_1, corr_per_country$cor_agency_2)
t.test(corr_per_country$cor_agency_1, corr_per_country$cor_agency_3)

t.test(corr_per_country$cor_Agency_growth_1, corr_per_country$cor_Agency_growth_2)
t.test(corr_per_country$cor_Agency_growth_1, corr_per_country$cor_Agency_growth_3)
t.test(corr_per_country$cor_sol_growth_1, corr_per_country$cor_sol_growth_2)


t.test(corr_across_countries$cor_agency_1, corr_across_countries$cor_agency_2)
t.test(corr_across_countries$cor_agency_1, corr_across_countries$cor_agency_3)
t.test(corr_across_countries$cor_sol_1, corr_across_countries$cor_sol_2)
t.test(corr_across_countries$cor_sol_1, corr_across_countries$cor_sol_3)




