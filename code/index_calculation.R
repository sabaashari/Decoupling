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
library(tidyverse)
library(plm)
library(lme4)
library(tseries)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tseries)   
library(urca)
library(ggrepel)

## 1. detrended variables and compare them with non-detrended variables. 
## 2. Geography classificantion. 
## 3. Decoupling time series - bring it back to the paper.
## 4. Regression analysis - 
## 5 - write down the 
## 6 - Remove all lags - 
## 7 - Explanations and 
## 8 - nothing for solidarity, some for agency. 
## 9 - life satisfaction - Gallup - Regress LF for each country -
## 10 - non-linear estimation - opens up to that. logistic function - quadratic.- but don't do that. 
# VAR Analysis - what is it? 


## look at African Union - European union. all European countries. 
## Half day to all countries. 
## pilot balloons - limitations of the paper. 
## Austrasian, Europe, US and Canada, Latin America, Suadi Arabia with South Africa, Any pattern that emerge geographically. 
## Geographic classifications. 

## Add Regressions####

# Read clean data ####
avg_country_year <- read.xlsx("../Data/average_country_year.xlsx")
avg_country_year <- avg_country_year %>% group_by(COUNTRYNEW) %>%
  mutate(income_group = names(which.max(table(World.Bank.s.income.classification))))
avg_country_year$income_group <- gsub("countries", "", avg_country_year$income_group)

avg_country_year <- avg_country_year %>% group_by(COUNTRYNEW) %>% 
  mutate(GDP_per_capita_diff = c(NA,diff(GDP_per_capita)), 
         solidarity_diff = c(NA, diff(solidarity)),
         agency_diff = c(NA, diff(agency)),
         GDP_lag = dplyr::lag(GDP_per_capita,1), 
         GDP_per_capita_diff_lead = dplyr::lead(GDP_per_capita_diff,1))%>% 
  ungroup()

avg_country_year <- avg_country_year %>%
  group_by(COUNTRYNEW) %>%
  mutate(
    scale_factor = diff(range(solidarity)) / diff(range(GDP_per_capita)),
    min_sol = min(solidarity),
    min_gdp = min(GDP_per_capita),
    GDP_scaled = (GDP_per_capita - min_gdp) * scale_factor + min_sol
  ) %>%
  ungroup()
### correlation distributions ####
ccf_df_solidarity <- avg_country_year %>% group_by(COUNTRYNEW) %>% 
  reframe(lags  = ccf(solidarity,GDP_per_capita , lag.max = 15)$lag, 
          acf = round(ccf(solidarity,GDP_per_capita , lag.max = 15)$acf,4 ))

ccf_df_agency <- avg_country_year %>% group_by(COUNTRYNEW) %>%  
  reframe(lags = ccf(na.remove(agency),GDP_per_capita , lag.max = 15)$lag,
          acf = round(ccf(na.remove(agency), GDP_per_capita , lag.max = 15)$acf,4))


ccf_df_a_S <- avg_country_year %>% group_by(COUNTRYNEW) %>%  
  reframe(lags = ccf(na.remove(agency),solidarity , lag.max = 15)$lag,
          acf = round(ccf(na.remove(agency), solidarity , lag.max = 15)$acf,4))

p <- ggplot(ccf_df_solidarity, aes(x = lags, y = acf)) + facet_wrap(~ COUNTRYNEW, scales = "free_x") + 
  geom_segment(aes(xend = lags, yend = 0)) +
  theme_minimal() +
  labs(title = "Cross-Correlation Function between solidarity and GDP per capita", 
       x = "Lag", y = "Correlation")
ggsave("../pictures/correlation_distribution_solidarity.jpg", plot = p, width = 8, height = 6)

p <- ggplot(ccf_df_agency, aes(x = lags, y = acf)) + facet_wrap(~ COUNTRYNEW, scales = "free_x") + 
  geom_segment(aes(xend = lags, yend = 0)) +
  theme_minimal() +
  labs(title = "Cross-Correlation Function between agency and GDP per capita", 
       x = "Lag", y = "Correlation")
ggsave("../pictures/correlation_distribution_agency.jpg", plot = p, width = 8, height = 6)

p <- ggplot(ccf_df_a_S, aes(x = lags, y = acf)) + facet_wrap(~ COUNTRYNEW, scales = "free_x") + 
  geom_segment(aes(xend = lags, yend = 0)) +
  theme_minimal() +
  labs(title = "Cross-Correlation Function between solidarity and Agency", 
       x = "Lag", y = "Correlation")
ggsave("../pictures/correlation_distribution_solidarity_agency.jpg", plot = p, width = 8, height = 6)

## country specific statistics ####
G20_countries <- c("Argentina", "Australia", "Brazil", "Canada", "China", "France", 
                   "Germany", "India", "Indonesia", "Italy", "Japan", "Mexico", "Russia", 
                   "Saudi Arabia", "South Africa", "South Korea", "Turkey", 
                   "United Kingdom", "United States")
for (country in c("Indonesia", "China", "India", "Russia", "Saudi Arabia", "Argentina"))
{
  print(country)
  model <- lm(agency ~ GDP_per_capita, data = avg_country_year %>% filter(COUNTRYNEW %in% country))
  print(coeftest(model, vcov = NeweyWest(model)))
}
for (country in G20_countries)
{
  print(country)
  model <- lm(solidarity ~ GDP_lag, data = avg_country_year %>% filter(COUNTRYNEW %in% country))
  #print(summary(model))
  print(coeftest(model, vcov = NeweyWest(model)))
}


country_stat <- avg_country_year %>% ungroup() %>%
  group_by(COUNTRYNEW) %>% summarise(type = first(Type), 
   country_code = first(Country.Code),
   mean_solidarity = mean(solidarity), 
   mean_agency = mean(agency, na.rm = TRUE), 
   first_solidarity = first(solidarity),
   first_agency = first(agency),
   
   cor_sol = cor(solidarity, GDP_per_capita, use = "complete.obs"),
   cor_sol_p.value = cor.test(solidarity, GDP_per_capita, use = "complete.obs")$p.value,
   
   cor_sol_lag = cor(solidarity, dplyr::lag(GDP_per_capita,1), use = "complete.obs"), 
   cor_sol_lag_p.value = cor.test(solidarity, dplyr::lag(GDP_per_capita,1), use = "complete.obs")$p.value, 
   
   cor_sol_lag_2 = cor(solidarity, dplyr::lag(GDP_per_capita,2), use = "complete.obs"),
   cor_sol_lag2_p.value = cor.test(solidarity, dplyr::lag(GDP_per_capita,2), use = "complete.obs")$p.value,
   
   cor_sol_diff = cor(solidarity, GDP_per_capita_diff, use = "complete.obs"),
   cor_sol_diff_p.value = cor.test(solidarity, GDP_per_capita_diff, use = "complete.obs")$p.value,
   
   cor_sol_growth = cor(solidarity, growth,  use = "complete.obs"),
   cor_sol_growth_p.value = cor.test(solidarity, growth,  use = "complete.obs")$p.value,
   
   cor_sol_diff_diff = cor(solidarity_diff, GDP_per_capita_diff, use = "complete.obs"), 
   cor_sol_diff_diff_p.value = cor.test(solidarity_diff, GDP_per_capita_diff, use = "complete.obs")$p.value, 
  
   cor_sol_agency = cor(solidarity, agency, use = "complete.obs"),
   cor_sol_agency_p.value = cor.test(solidarity, agency, use = "complete.obs")$p.value,
   
   
   cor_agency = cor(agency, GDP_per_capita, use = "complete.obs"),
   cor_agency_p.value = cor.test(agency, GDP_per_capita, use = "complete.obs")$p.value,
   
   cor_agency_lag = cor(agency, dplyr::lag(GDP_per_capita,1), use = "complete.obs"),
   cor_agency_lag_p.value = cor.test(agency, dplyr::lag(GDP_per_capita,1), use = "complete.obs")$p.value,
   
   cor_agency_lag_2 = cor(agency, dplyr::lag(GDP_per_capita,2), use = "complete.obs"), 
   cor_agency_lag_2_p.value = cor.test(agency, dplyr::lag(GDP_per_capita,2), use = "complete.obs")$p.value, 
   
   cor_agency_diff = cor(agency, GDP_per_capita_diff, use = "complete.obs"),
   cor_agency_diff_p.value = cor.test(agency, GDP_per_capita_diff, use = "complete.obs")$p.value,
   
   cor_agency_diff_diff = cor(agency_diff, GDP_per_capita_diff, use = "complete.obs"),
   cor_agency_diff_diff_p.value = cor.test(agency_diff, GDP_per_capita_diff, use = "complete.obs")$p.value, 
   
   cor_agency_growth = cor(agency, growth,  use = "complete.obs"),
   cor_agency_growth_p.value = cor.test(agency, growth,  use = "complete.obs")$p.value,
   
   D_S = 1- cor_sol, 
   D_A = 1 - cor_agency, 
   D_S_diff = 1- cor_sol_diff, 
   D_A_diff = 1- cor_agency_diff, 
   D = 1- (cor_agency + cor_sol)/2,
   D_diff = (D_S_diff + D_A_diff)/2,
   GDP_per_capita = mean(GDP_per_capita), 
   D_S_growth = 1 - cor_sol_growth, 
   D_A_growth = 1 - cor_agency_growth,
   D_growth = (D_S_growth + D_A_growth)/2,
   income_group =  names(which.max(table(World.Bank.s.income.classification)))) 
country_stat$income_group <- gsub("countries", "", country_stat$income_group)
country_stat <- country_stat %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3)))
country_stat <- country_stat %>% 
  dplyr::mutate(
    across(
      .cols = contains("p.value"),   # select all columns with "p.value"
      .fns = ~ case_when(
        .x < 0.01 ~ paste0(.x, "**"),
        .x < 0.05 ~ paste0(.x, "*"),
        .x < 0.1  ~ paste0(.x, "~"),
        TRUE      ~ as.character(.x)
      )
    )
  )
write.xlsx(country_stat,"../Data/country_statistics.xlsx")

## country specific statistics covid filtered ####
country_stat_covid <- avg_country_year %>% ungroup() %>%  
  filter(!YEAR_WAVE %in% c(2019, 2020,2021)) %>%
  group_by(COUNTRYNEW) %>% summarise(type = first(Type), 
             country_code = first(Country.Code),
             mean_solidarity = mean(solidarity), 
             mean_agency = mean(agency, na.rm = TRUE), 
             first_solidarity = first(solidarity),
             first_agency = first(agency),
             
             cor_sol = cor(solidarity, GDP_per_capita, use = "complete.obs"),
             cor_sol_p.value = cor.test(solidarity, GDP_per_capita, use = "complete.obs")$p.value,
             
             cor_sol_lag = cor(solidarity, dplyr::lag(GDP_per_capita,1), use = "complete.obs"), 
             cor_sol_lag_p.value = cor.test(solidarity, dplyr::lag(GDP_per_capita,1), use = "complete.obs")$p.value, 
             
             cor_sol_lag_2 = cor(solidarity, dplyr::lag(GDP_per_capita,2), use = "complete.obs"),
             cor_sol_lag2_p.value = cor.test(solidarity, dplyr::lag(GDP_per_capita,2), use = "complete.obs")$p.value,
             
             cor_sol_diff = cor(solidarity, GDP_per_capita_diff, use = "complete.obs"),
             cor_sol_diff_p.value = cor.test(solidarity, GDP_per_capita_diff, use = "complete.obs")$p.value,
             
             cor_sol_growth = cor(solidarity, growth,  use = "complete.obs"),
             cor_sol_growth_p.value = cor.test(solidarity, growth,  use = "complete.obs")$p.value,
             
             cor_sol_diff_diff = cor(solidarity_diff, GDP_per_capita_diff, use = "complete.obs"), 
             cor_sol_diff_diff_p.value = cor.test(solidarity_diff, GDP_per_capita_diff, use = "complete.obs")$p.value, 
             
             cor_sol_agency = cor(solidarity, agency, use = "complete.obs"),
             cor_sol_agency_p.value = cor.test(solidarity, agency, use = "complete.obs")$p.value,
             
             
             cor_agency = cor(agency, GDP_per_capita, use = "complete.obs"),
             cor_agency_p.value = cor.test(agency, GDP_per_capita, use = "complete.obs")$p.value,
             
             cor_agency_lag = cor(agency, dplyr::lag(GDP_per_capita,1), use = "complete.obs"),
             cor_agency_lag_p.value = cor.test(agency, dplyr::lag(GDP_per_capita,1), use = "complete.obs")$p.value,
             
             cor_agency_lag_2 = cor(agency, dplyr::lag(GDP_per_capita,2), use = "complete.obs"), 
             cor_agency_lag_2_p.value = cor.test(agency, dplyr::lag(GDP_per_capita,2), use = "complete.obs")$p.value, 
             
             cor_agency_diff = cor(agency, GDP_per_capita_diff, use = "complete.obs"),
             cor_agency_diff_p.value = cor.test(agency, GDP_per_capita_diff, use = "complete.obs")$p.value,
             
             cor_agency_diff_diff = cor(agency_diff, GDP_per_capita_diff, use = "complete.obs"),
             cor_agency_diff_diff_p.value = cor.test(agency_diff, GDP_per_capita_diff, use = "complete.obs")$p.value, 
             
             cor_agency_growth = cor(agency, growth,  use = "complete.obs"),
             cor_agency_growth_p.value = cor.test(agency, growth,  use = "complete.obs")$p.value,
             
             D_S = 1- cor_sol, 
             D_A = 1 - cor_agency, 
             D_S_diff = 1- cor_sol_diff, 
             D_A_diff = 1- cor_agency_diff, 
             D = 1- (cor_agency + cor_sol)/2,
             D_diff = (D_S_diff + D_A_diff)/2,
             GDP_per_capita = mean(GDP_per_capita), 
             D_S_growth = 1 - cor_sol_growth, 
             D_A_growth = 1 - cor_agency_growth,
             D_growth = (D_S_growth + D_A_growth)/2,
             income_group =  names(which.max(table(World.Bank.s.income.classification)))) 
country_stat_covid$income_group <- gsub("countries", "", country_stat_covid$income_group)
country_stat_covid <- country_stat_covid %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3)))
country_stat_covid <- country_stat_covid %>% 
  dplyr::mutate(
    across(
      .cols = contains("p.value"),   # select all columns with "p.value"
      .fns = ~ case_when(
        .x < 0.01 ~ paste0(.x, "**"),
        .x < 0.05 ~ paste0(.x, "*"),
        .x < 0.1  ~ paste0(.x, "~"),
        TRUE      ~ as.character(.x)
      )
    )
  )
write.xlsx(country_stat,"../Data/country_statistics_covid_filtered.xlsx")

# gdp = c(1,2,3,4,5,6)
# s = c(2,3,4,20,6,10)
# cor(s[-1], gdp[-length(gdp)])
# a <- s[-1]
# b <- gdp[-length(gdp)]
#sum((a-mean(s))*(b-mean(gdp)))/ sqrt(sum((gdp-mean(gdp))^2)* sum((s-mean())^2 ))
#res <- data.frame(lags = ccf (a, b, lag.max = 2)$lag, acf = ccf (a, b, lag.max = 2)$acf )

## Stationary Tests ####
## ADF tests - quick search about the tests, choose one, 
stationary_test <- avg_country_year %>% group_by(COUNTRYNEW) %>% 
  summarise(adf_solidarity = round(adf.test(solidarity,  alternative = "stationary")$p.value,4),
            kpss_solidarity_level = round(kpss.test(solidarity, null = "Level")$p.value,4), 
            kpss_solidarity_trend = round(kpss.test(solidarity, null = "Trend")$p.value,4), 
            
            adf_agency = round(adf.test(na.remove(agency),  alternative = "stationary")$p.value,4),
            kpss_agency_level = round(kpss.test(agency, null = "Level")$p.value,4), 
            kpss_agency_trend = round(kpss.test(agency, null = "Trend")$p.value,4),
            
            adf_sol = adf.test(residuals(lm(solidarity ~ GDP_per_capita)))$p.value,
            kpss_sol = kpss.test(residuals(lm(solidarity ~ GDP_per_capita)), null = "Level")$p.value,
            adf_agency = adf.test(residuals(lm(agency ~ GDP_per_capita)))$p.value, 
            kpss_agency = kpss.test(residuals(lm(agency ~ GDP_per_capita)), null = "Level")$p.value,
           
            ) %>% 
            mutate(solidarity_stationary = case_when(
              adf_solidarity < 0.05 & kpss_solidarity_level > 0.05 ~ "level_stationary",
              adf_solidarity < 0.05 & kpss_solidarity_trend > 0.05 ~ "trend_stationary",
              adf_solidarity > 0.05 & kpss_solidarity_level < 0.05 ~ "level_non_stationary",
              adf_solidarity > 0.05 & kpss_solidarity_trend < 0.05 ~ "trend_non_stationary",
              TRUE ~ "non-known"), 
              agency_stationary = case_when(
                adf_agency < 0.05 & kpss_agency_level > 0.05 ~ "level_stationary",
                adf_agency < 0.05 & kpss_agency_trend > 0.05 ~ "trend_stationary",
                adf_agency > 0.05 & kpss_agency_level < 0.05 ~ "level_non_stationary",
                adf_agency > 0.05 & kpss_agency_trend < 0.05 ~ "trend_non_stationary",
                TRUE ~ "non-known"), 
              cointegrated_solidarity = case_when(
                adf_sol < 0.05 & kpss_sol > 0.05 ~ "co-integrated",
                adf_sol > 0.05 & kpss_sol < 0.05 ~ "not-related",
                TRUE ~ "unknown"), 
              cointegrated_agency = case_when(
                adf_agency < 0.05 & kpss_agency > 0.05 ~ "co-integrated",
                adf_agency > 0.05 & kpss_agency < 0.05 ~ "not-related",
                TRUE ~ "unknown")
              )%>% 
  merge(country_stat %>% select(mean_solidarity, mean_agency, COUNTRYNEW), by = "COUNTRYNEW")
stationary_test <- stationary_test %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3)))


write.xlsx(stationary_test, "../Data/stationary_tests.xlsx")

## KPSS tests
# a <-ur.pp(avg_country_year$solidarity, type = "Z-tau", model = "trend", lags = "short")
# 
# # Level stationarity (stationary around a constant mean)
# b <- avg_country_year %>% group_by(COUNTRYNEW) %>% 
#   summarise(p_s = round(adf.test(solidarity,  alternative = "stationary")$p.value,4), 
#             p_t = ur.df(solidarity,type = "trend", selectlags = "AIC"))
# 
# kpss_level <- kpss.test(solid_ts_ts, null = "Level")
# cat("KPSS (level) p-value:", kpss_level$p.value, "\n")
# 
# # Trend stationarity (stationary around a deterministic trend)
# kpss_trend <- kpss.test(solid_ts_ts, null = "Trend")
# cat("KPSS (trend) p-value:", kpss_trend$p.value, "\n")



# More control (trend vs. level) using urca::ur.df




# Per country time series ####
# Time series per country: GDP per Capita, solidarity, Agency ####
avg_country_year_scaled <- avg_country_year %>%
  group_by(COUNTRYNEW) %>%
  mutate(
    scale_factor = diff(range(solidarity)) / diff(range(GDP_per_capita)),
    min_sol = min(solidarity),
    min_gdp = min(GDP_per_capita),
    GDP_scaled = (GDP_per_capita - min_gdp) * scale_factor + min_sol
  ) %>%
  ungroup()

ggplot(avg_country_year_scaled, aes(YEAR_WAVE)) +
  geom_line(aes(y = solidarity, color = "solidarity")) +
  geom_line(aes(y = agency, color = "agency")) +
  geom_line(aes(y = GDP_scaled, color = "GDP_per_capita")) +
  facet_wrap(~ COUNTRYNEW) +
  scale_y_continuous(
    name = "solidarity / agency",
    sec.axis = sec_axis(
      transform = ~ (. - min(avg_country_year$solidarity)) / scale_factor + min(avg_country_year$GDP_per_capita),
      name = "GDP_per_capita"
    )
  ) + 
  labs(title = "Solidarity, Agency and GDP_per_capita per country (constant 2015$), G20 countries, 2006–2023") +
  scale_color_manual(
    values = c(
      "solidarity" = "blue",
      "agency" = "#F4A460",
      "GDP_per_capita" = "red"
    )
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0, vjust = 1, size = 14, face = "bold"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1, "lines")
  )
ggsave("../pictures/GDP_solidarity_agency.jpg", width = 10, height = 7)

# Time series per country: growth, solidarity, Agency ####
avg_country_year_scaled <- avg_country_year %>%
  group_by(COUNTRYNEW) %>%
  mutate(
    scale_factor = diff(range(solidarity)) / diff(range(growth)),
    min_sol = min(solidarity),
    min_gdp = min(growth),
    growth_scaled = (growth - min_gdp) * scale_factor + min_sol
  ) %>%
  ungroup()
scale_factor  <- diff(range(avg_country_year$solidarity)) / 
  diff(range(avg_country_year$growth))
ggplot(avg_country_year_scaled, aes(YEAR_WAVE)) +
  geom_line(aes(y = solidarity, color = "solidarity"))+
  geom_line(aes(y = agency, color = "agency"))+
  geom_line(aes(y = growth_scaled , color = "growth")) + facet_wrap(~COUNTRYNEW) +
  scale_y_continuous(
    name = "solidarity / agency",
    sec.axis = sec_axis(
      transform = ~ (. - min(avg_country_year$solidarity)) / scale_factor + min(avg_country_year_scaled$growth_scaled),
      name = "GDP_per_capita"
    )
  ) + 
  labs(title = "Solidarity, Agnecy and Growth rate,  G20 countries, 2006-2023") +
  scale_color_manual(values = c("solidarity" = "blue", "growth" = "red", "agency" = "#F4A460")) +
  theme_minimal() +  theme(
    strip.text = element_text(size = 11, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0, vjust = 1, size = 14, face = "bold"), 
    legend.position = "top",
    legend.title = element_blank(), 
    legend.text = element_text(size = 12, face = "bold"), 
    panel.spacing = unit(1, "lines"))

ggsave("../pictures/growth_solidarity_agency.jpg", width = 10, height = 7)


#scale_factor  <- diff(range(avg_country_year$solidarity)) / diff(range(avg_country_year$GDP_per_capita_diff, na.rm = TRUE))
avg_country_year_scaled <- avg_country_year %>%
  group_by(COUNTRYNEW) %>%
  mutate(
    scale_factor = diff(range(solidarity)) / diff(range(GDP_per_capita_diff)),
    min_sol = min(solidarity),
    min_gdp = min(growth),
    GDP_scaled = (growth - min_gdp) * scale_factor + min_sol
  ) %>%
  ungroup()
ggplot(avg_country_year_scaledavg_country_year_scaled, aes(YEAR_WAVE)) +
  geom_line(aes(y = solidarity, color = "solidarity"))+
  geom_line(aes(y = agency, color = "agency"))+
  geom_line(aes(y = (GDP_per_capita_diff - min(GDP_per_capita_diff, na.rm = TRUE)) * scale_factor + min(solidarity), 
                color = "GDP_per_capita_diff")) + facet_wrap(~COUNTRYNEW) +
  scale_y_continuous(
    name = "solidarity/agency",
    sec.axis = sec_axis(
      transform = ~ (. - min(avg_country_year$solidarity)) / scale_factor + 
        min(avg_country_year$GDP_per_capita_diff, na.rm = TRUE),
      name = "GDP_per_capita_diff"
    )
  ) + 
  labs(title = "Solidarity, Agnecy and GDP_per_capita_diff (in constant 2015$),  G20 countries, 2006-2023") +
  scale_color_manual(values = c("solidarity" = "blue", "GDP_per_capita_diff" = "red", "agency" = "#F4A460")) +
  theme_minimal() +  theme(
    strip.text = element_text(size = 11, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0, vjust = 1, size = 14, face = "bold"), 
    legend.position = "top",
    legend.title = element_blank(), 
    legend.text = element_text(size = 12, face = "bold"), 
    panel.spacing = unit(1, "lines"))
ggsave("../pictures/diff_solidarity_agency.jpg", width = 10, height = 7)


avg_country_year_scaled <- avg_country_year %>%
  group_by(COUNTRYNEW) %>%
  mutate(
    scale_factor = diff(range(solidarity_diff, na.rm = TRUE)) / diff(range(GDP_per_capita_diff, na.rm = TRUE)),
    min_sol = min(solidarity_diff, na.rm = TRUE),
    min_gdp = min(GDP_per_capita_diff, na.rm = TRUE),
    GDP_scaled = (GDP_per_capita_diff - min_gdp) * scale_factor + min_sol
  ) %>%
  ungroup()

ggplot(avg_country_year_scaled, aes(YEAR_WAVE)) +
  geom_line(aes(y = solidarity_diff, color = "solidarity")) +
  geom_line(aes(y = agency_diff, color = "agency")) +
  geom_line(aes(y = GDP_scaled, color = "GDP_per_capita")) +
  facet_wrap(~ COUNTRYNEW) +
  scale_y_continuous(
    name = "solidarity_diff / agency_diff",
    sec.axis = sec_axis(
      transform = ~ (. - min(avg_country_year_scaled$solidarity_diff, na.rm = TRUE)) / scale_factor + 
        min(avg_country_year_scaled$GDP_scaled, na.rm = TRUE),
      name = "GDP_per_capita_diff"
    )
  ) +
  labs(title = "Solidarity_diff, Agency_diff and GDP_per_capita_diff per country 
       (constant 2015$), G20 countries, 2006–2023") +
  scale_color_manual(
    values = c(
      "solidarity" = "blue",
      "agency" = "#F4A460",
      "GDP_per_capita" = "red"
    )
  ) + 
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0, vjust = 1, size = 14, face = "bold"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1, "lines")
  )
ggsave("../pictures/diff_solidarity_agency_diff.jpg", width = 10, height = 7)

avg_country_year$GDP_per_capita_diff
# per country scatter plots ####
p1 <- ggplot(avg_country_year %>% filter(Country.Code %in% c("AUS", "USA")), aes(x = GDP_per_capita, y= solidarity)) +
  geom_point(color = "blue", size =1) + facet_wrap(~COUNTRYNEW) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "solid") +
  theme_minimal() + labs(title = "scatter plot of GDP_per_capita and Solidarity for each of G20 countries") +
  geom_text(aes(label = Country.Code), vjust = -1, size = 2) +
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
    legend.text = element_text(size = 12, face = "bold")
  )
p2 <- ggplot(avg_country_year %>% filter(Country.Code %in% c("CAN", "GBR", "DEU")), aes(x = GDP_per_capita, y= solidarity)) +
  geom_point(color = "blue", size =1) + facet_wrap(~COUNTRYNEW) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "solid") +
  theme_minimal() +
  geom_text(aes(label = Country.Code), vjust = -1, size = 2) +
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
    legend.text = element_text(size = 12, face = "bold")
  )
p3 <- ggplot(avg_country_year %>% filter(Country.Code %in% c("ITA","FRA", "JPN")), aes(x = GDP_per_capita, y= solidarity)) +
  geom_point(color = "blue", size =1) + facet_wrap(~COUNTRYNEW) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "solid") +
  theme_minimal() +
  geom_text(aes(label = Country.Code), vjust = -1, size = 2) +
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
    legend.text = element_text(size = 12, face = "bold")
  )
a <- grid.arrange(p1, p2, p3)
ggsave("../pictures/GDP_solidarity_scatter_plot.jpg", plot = a, width = 10, height = 10)

p1 <- ggplot(avg_country_year 
             %>% filter(Country.Code %in% c("CAN", "GBR", "DEU")), aes(x = GDP_per_capita, y= agency)) +
  geom_point(color = "blue", size =1) + facet_wrap(~COUNTRYNEW) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "solid") +
  theme_minimal() + labs(title = "Scatter plot of GDP_per_capita and Agency for each of G20 countries") +
  geom_text(aes(label = Country.Code), vjust = -1, size = 2) +
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
    legend.text = element_text(size = 12, face = "bold")
  )

p2 <- ggplot(avg_country_year %>% filter(Country.Code %in% c("AUS", "USA")), aes(x = GDP_per_capita, y= agency)) +
  geom_point(color = "blue", size =1) + facet_wrap(~COUNTRYNEW) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "solid") +
  theme_minimal() +
  geom_text(aes(label = Country.Code), vjust = -1, size = 2) +
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
    legend.text = element_text(size = 12, face = "bold")
  )

p3 <- ggplot(avg_country_year %>% filter(Country.Code %in% c("ITA","FRA", "JPN")), aes(x = GDP_per_capita, y= agency)) +
  geom_point(color = "blue", size =1) + facet_wrap(~COUNTRYNEW) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "solid") +
  theme_minimal()  +
  geom_text(aes(label = Country.Code), vjust = -1, size = 2) +
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
    legend.text = element_text(size = 12, face = "bold")
  )
a <- grid.arrange(p1, p2, p3)
ggsave("../pictures/GDP_agency_scatter_plot.jpg", plot = a, width = 10, height = 10)

## All countries Time series ####
## Solidarity Time series
last_points <- avg_country_year %>%
  group_by(COUNTRYNEW) %>%
  filter(YEAR_WAVE == max(YEAR_WAVE)) %>% ungroup()

ggplot(avg_country_year, aes(x = YEAR_WAVE, y = solidarity, color = COUNTRYNEW)) +
  geom_line(linewidth = 0.6) +
  theme_minimal() + facet_wrap(~Type) +
  geom_text(
    data = last_points,
    aes(label = Country.Code),
    hjust = 0.2,            # move text slightly to the right
    size = 3,
    show.legend = FALSE
  ) +
  labs(
    title = "Solidarity score from 2006-2023 for G20 countries, Developing vs Developed",
    x = "Year",
    y = "Solidarity",
    color = "Country"
  ) +
  scale_color_hue(l = 40, c = 100) +
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = "bold")
  )
ggsave("../Output/solidarity_time_series.jpg",  width = 10, height = 5)

ggplot(avg_country_year, aes(x = YEAR_WAVE, y = solidarity, color = COUNTRYNEW)) +
  geom_line(linewidth = 0.6) +
  theme_minimal() + facet_wrap(~income_group) +
  geom_text(
    data = last_points,
    aes(label = Country.Code),
    hjust = 0.2,            # move text slightly to the right
    size = 3,
    show.legend = FALSE
  ) +
  labs(
    title = "Solidarity score from 2006-2023 for G20 countries, Different Income groups",
    x = "Year",
    y = "Solidarity",
    color = "Country"
  ) +
  scale_color_hue(l = 40, c = 100) +
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = "bold")
  )
ggsave("../Output/solidarity_time_series_income.jpg",  width = 10, height = 5)


## Agency time series
ggplot(avg_country_year, aes(x = YEAR_WAVE, y = agency, color = COUNTRYNEW)) +
  geom_line(linewidth = 0.6) +
  theme_minimal() + facet_wrap(~Type) +
  geom_text(
    data = last_points,
    aes(label = Country.Code),
    hjust = 0.2,            # move text slightly to the right
    size = 3,
    show.legend = FALSE
  ) +
  labs(
    title = "Agency score from 2006-2023 for G20 countries, Developing vs Developed",
    x = "Year",
    y = "Agency",
    color = "Country"
  ) +   scale_x_continuous(
    breaks = seq(2006,
                 2025,
                 by =5)) +
  scale_color_hue(l = 40, c = 100) +
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = "bold")
  )
ggsave("../Output/Agency_time_series_type.jpg",  width = 10, height = 5)

ggplot(avg_country_year, aes(x = YEAR_WAVE, y = agency, color = COUNTRYNEW)) +
  geom_line(linewidth = 0.6) +
  theme_minimal() + facet_wrap(~income_group) +
  geom_text(
    data = last_points,
    aes(label = Country.Code),
    hjust = 0.2,            # move text slightly to the right
    size = 3,
    show.legend = FALSE
  ) +
  labs(
    title = "Agency score from 2006-2023 for G20 countries, different income groups",
    x = "Year",
    y = "Agency",
    color = "Country"
  ) +   scale_x_continuous(
    breaks = seq(2006,
                 2025,
                 by =5)) +
  scale_color_hue(l = 40, c = 100) +
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = "bold")
  )
ggsave("../Output/Agency_time_series_income.jpg",  width = 10, height = 5)


# GDP_per_capita time series
ggplot(avg_country_year, aes(x = YEAR_WAVE, y = GDP_per_capita, color = COUNTRYNEW)) +
  geom_line(linewidth = 0.6) +
  theme_minimal() + facet_wrap(~Type) +
  geom_text(
    data = last_points,
    aes(label = Country.Code),
    hjust = 0.2,            # move text slightly to the right
    size = 3,
    show.legend = FALSE
  ) +
  labs(
    title = "GDP_per_capita per capita in constant 2015$ from 2006-2023 for G20 countries, Developing vs Developed",
    x = "Year",
    y = "GDP_per_capita per capita",
    color = "Country"
  ) +   scale_x_continuous(
    breaks = seq(2006,
                 2025,
                 by =5)) +
  scale_color_hue(l = 40, c = 100) +
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = "bold")
  )
ggsave("../Output/GDP_per_capita_time_series_type.jpg",  width = 10, height = 5)

ggplot(avg_country_year, aes(x = YEAR_WAVE, y = GDP_per_capita, color = COUNTRYNEW)) +
  geom_line(linewidth = 0.6) +
  theme_minimal() + facet_wrap(~income_group) +
  geom_text(
    data = last_points,
    aes(label = Country.Code),
    hjust = 0.2,            # move text slightly to the right
    size = 3,
    show.legend = FALSE
  ) +
  labs(
    title = "GDP_per_capita per capita in constant 2015$ from 2006-2023 for G20 countries, Income groups",
    x = "Year",
    y = "GDP_per_capita per capita",
    color = "Country"
  ) +   scale_x_continuous(
    breaks = seq(2006,
                 2025,
                 by =5)) +
  scale_color_hue(l = 40, c = 100) +
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = "bold")
  )
ggsave("../Output/GDP_per_capita_time_series_income.jpg",  width = 10, height = 5)


### Trade time series
last_points <- avg_country_year %>%
  group_by(COUNTRYNEW) %>%
  filter(YEAR_WAVE == max(YEAR_WAVE)) %>% ungroup()

ggplot(avg_country_year, aes(x = YEAR_WAVE, y = trade, color = COUNTRYNEW)) +
  geom_line(linewidth = 0.6) +
  theme_minimal() + facet_wrap(~Type) + geom_text(
    data = last_points,
    aes(label = Country.Code),
    hjust = 0.2,            # move text slightly to the right
    size = 3,
    show.legend = FALSE
  ) +
  labs(
    title = "% of Trade/GDP per capita from 2006-2023 for G20 countries, Developing vs Developed",
    x = "Year",
    y = "Solidarity",
    color = "Country"
  ) +
  scale_color_hue(l = 40, c = 100) +
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = "bold")
  )
ggsave("../Output/solidarity_time_series.jpg",  width = 10, height = 5)



##  T-tests for developing vs developed countries and income groups ####

library(forecast)

ggAcf(avg_country_year$GDP_per_capita) + facet_wrap(~COUNTRYNEW)

avg_country_year %>%
  group_by(COUNTRYNEW) %>%
  group_walk(~ {
    acf(.x$solidarity, main = paste("ACF -", .y$COUNTRYNEW))
  })

avg_country_year %>%
  group_by(COUNTRYNEW) %>%
  group_walk(~ {
    acf(.x$agency, main = paste("ACF -", .y$COUNTRYNEW))
  })

results <- avg_country_year %>%
  group_by(COUNTRYNEW) %>%
  summarise(
    adf_p_value = adf.test(GDP_per_capita)$p.value,
    solid_p_value = adf.test(solidarity)$p.value, 
    solid_p_value_1 = adf.test(diff(solidarity))$p.value, 
    solid_p_value_1 = adf.test(diff(solidarity))$p.value
  )

shapiro.test(country_stat$mean_agency) #check if solidarity and agency are normally distributed
shapiro.test(country_stat$mean_solidarity)

result <- data.frame()
scores <- c('mean_solidarity', 'mean_agency', 'D_S', 'D_A', 'D', 'GDP_per_capita', 
            'D_S_growth', 'D_A_growth', 'D_growth', 'cor_sol_lag', 'cor_agency_lag')
#country_stat <- country_stat %>% filter(COUNTRYNEW!="South Korea")
for (score in scores)
{
  t1 <- t.test(country_stat[country_stat$type=="Developed", score], 
               country_stat[country_stat$type=="Developing", score], mu = 0)
  
  t2 <- t.test(country_stat[country_stat$income_group %in% "High-income ", score],
               country_stat[country_stat$income_group=="Upper-middle-income ", score])
  
  estimates_1 <- unname(t1$estimate)
  estimates_2 <- unname(t2$estimate)
  result <- rbind(result, data.frame(metric = score, p_value_type = t1$p.value, 
                                     mean_developed = estimates_1[1], 
                                     mean_developing = estimates_1[2], 
                                     p_value_income = t2$p.value, 
                                     mean_high_income = estimates_2[1], 
                                     mean_upper_midle_income = estimates_2[2]))
}
result[colnames(result)[-1]] <- round(result[colnames(result)[-1]], 4)
result <- result %>% dplyr:: mutate(across(all_of(c("p_value_income")), 
                                           ~ case_when(
                                             . < 0.01 ~ paste(p_value_income,'**'),
                                             . < 0.05 ~ paste(p_value_income,'*'),
                                             . < 0.1 ~ paste(p_value_income,'~') ,
                                             TRUE ~ as.character(p_value_income))))
result <- result %>% dplyr:: mutate(across(all_of(c("p_value_type")), 
                                           ~ case_when(
                                             . < 0.01 ~ paste(p_value_type,'**'),
                                             . < 0.05 ~ paste(p_value_type,'*'),
                                             . < 0.1 ~ paste(p_value_type,'~') ,
                                             TRUE ~ as.character(p_value_type))))

write.xlsx(result, "../output/result_country_reg.xlsx")
scores <- c('solidarity', 'agency')
result <- data.frame()
for (score in scores)
{
  t1 <- t.test(avg_country_year[avg_country_year$Type=="Developed", score],
               avg_country_year[avg_country_year$Type=="Developing", score], alternative = "greater")
  
  t2 <- t.test(avg_country_year[avg_country_year$income_group %in% "High-income ", score],
               avg_country_year[avg_country_year$income_group=="Upper-middle-income ", score])
  
  estimates_1 <- unname(t1$estimate)
  estimates_2 <- unname(t2$estimate)
  result <- rbind(result, data.frame(metric = score, p_value_type = t1$p.value, 
                                     mean_developed = estimates_1[1], 
                                     mean_developing = estimates_1[2], 
                                     p_value_income = t2$p.value, 
                                     mean_high_income = estimates_2[1], 
                                     mean_upper_midle_income = estimates_2[2]))
}
result[colnames(result)[-1]] <- round(result[colnames(result)[-1]], 4)


result <- result %>% dplyr:: mutate(across(all_of(c("p_value_income", "p_value_type")), 
                                           ~ case_when(
                                             . < 0.01 ~ paste(p_value_income,'**'),
                                             . < 0.05 ~ paste(p_value_income,'*'),
                                             . < 0.1 ~ paste(p_value_income,'~') ,
                                             TRUE ~ as.character(p_value_income))))
write.xlsx(result, "../output/developing_developed_stats.xlsx")

# I can set it aside as I don't know why random effect doesn't work here
pdata <- pdata.frame(avg_country_year, index = c("COUNTRYNEW", "YEAR_WAVE"))
fe_model <- plm(
  solidarity ~ Type, 
  data = pdata, model = "between"
)
summary <- as.data.frame(summary(fe_model)$coefficients)
names(summary)[names(summary) == "Pr(>|t|)" | names(summary) == "Pr(>|z|)"] <- "p_value_FE"
summary$coef_names <- rownames(summary)
summary <- summary[c('coef_names', 'Estimate','p_value')]

re_model <- lmer(solidarity ~ Type + (1 | COUNTRYNEW), data = pdata)

summary <- as.data.frame(summary(re_model)$coefficients)
model_lm <- lm(solidarity ~Type, data = avg_country_year )


hausman_result <- phtest(fe_model, re_model)

summary(model_panel <- plm(
  agency ~ Typee, 
  data = pdata, model = "between"
))
summary(lm(agency ~Type, data = avg_country_year ))
model <- lmer(agency ~ Type + (1 | COUNTRYNEW), data = pdata)



### Archived ####
# avg_country_year <- avg_country_year %>% ungroup() %>%
#   mutate(GDP_per_capita_z = (GDP_per_capita - mean(GDP_per_capita))/sd(GDP_per_capita) , 
#          agency_z =  (agency - mean(agency, na.rm = TRUE))/sd(agency, na.rm = TRUE), 
#          solidarity_z = (solidarity - mean(solidarity))/sd(solidarity, na.rm = TRUE))
# GDP_per_capita_solidarity <- avg_country_year %>%
#   select(COUNTRYNEW, YEAR_WAVE, solidarity_z, GDP_per_capita_z) %>% 
#   pivot_longer(cols = c("GDP_per_capita_z", "solidarity_z"), 
#                names_to = "score", values_to = "value")
# 
# GDP_per_capita_agency <- avg_country_year %>% 
#   select(COUNTRYNEW, YEAR_WAVE,agency_z, GDP_per_capita_z) %>% 
#   pivot_longer(cols = c("agency_z", "GDP_per_capita_z"), 
#                names_to = "score", values_to = "value")

# ggplot(avg_country_year, aes(YEAR_WAVE)) +
#   geom_line(aes(y = solidarity_z, color = "solidarity"))+
#   geom_line(aes(y = GDP_per_capita_z, color = "GDP_per_capita")) + facet_wrap(~COUNTRYNEW) +
#   scale_color_manual(values = c("solidarity" = "blue", "GDP_per_capita" = "red")) +
#   theme_minimal()
# ggsave("../output/GDP_per_capita_solidarity.jpg", width = 10, height = 7)

# ggplot(GDP_per_capita_solidarity, aes(x = YEAR_WAVE, y = value, color = score)) +
#   geom_line(size = 0.8) + facet_wrap(~COUNTRYNEW) + 
#   scale_x_continuous(
#     breaks = seq(2006, 
#                  2025, 
#                  by = 5)) +
#   theme_minimal() + labs(title = "Solidarity and GDP_per_capita for G20 countries between 2006-2023") +
#   theme(
#     strip.text = element_text(size = 15, face = 'bold'),
#     axis.text = element_text(size = 11, color = "black"),
#     plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"), 
#     axis.title = element_blank(), 
#     legend.position = "top",
#     legend.title = element_blank(), 
#     legend.text = element_text(size = 12, face = "bold")
#   )
# ggsave("../output/GDP_per_capita_Solidarity_Saba.jpg",  width = 10, height = 7)

# scale_factor  <- diff(range(avg_country_year$agency, na.rm = TRUE)) / diff(range(avg_country_year$GDP_per_capita))
# ggplot(avg_country_year, aes(YEAR_WAVE)) +
#   geom_line(aes(y = agency, color = "Agency"))+
#   geom_line(aes(y = (GDP_per_capita - min(GDP_per_capita)) * scale_factor + min(agency, na.rm = TRUE), 
#                 color = "GDP_per_capita")) + facet_wrap(~COUNTRYNEW) +
#   scale_y_continuous(
#     name = "Agency",
#     sec.axis = sec_axis(
#       trans = ~ (. - min(avg_country_year$agency, na.rm = TRUE)) / scale_factor + 
#         min(avg_country_year$GDP_per_capita),
#       name = "GDP_per_capita"
#     )
#   ) + 
#   scale_color_manual(values = c("Agency" = "blue", "GDP_per_capita" = "red")) +
#   theme_minimal() + 
#   theme(
#     strip.text = element_text(size = 12, face = 'bold'),
#     axis.text = element_text(size = 11, color = "black"),
#     plot.title = element_text(hjust = 0, vjust = 1, size = 16, face = "bold"), 
#     legend.position = "top",
#     legend.title = element_blank(), 
#     legend.text = element_text(size = 12, face = "bold"))+
#   labs(title = "Agency and GDP_per_capita for G20 countries between 2006-2023")
# ggsave("../output/GDP_per_capita_Agency_Dennis.jpg", width = 10, height = 7)
# 
# # ggplot(GDP_per_capita_agency, aes(x = YEAR_WAVE, y = value, color = score)) +
# #   geom_line(size = 1.2) + facet_wrap(~COUNTRYNEW) + 
# #   scale_x_continuous(
# #     breaks = seq(2006, 
# #                  2025, 
# #                  by = 5)) +
# #   theme_minimal() + labs(title = "Agency and GDP_per_capita for G20 countries between 2006-2023") +
# #   theme(
# #     strip.text = element_text(size = 15, face = 'bold'),
# #     axis.text = element_text(size = 11, color = "black"),
# #     plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"), 
# #     axis.title = element_blank(), 
# #     legend.position = "top",
# #     legend.title = element_blank(), 
# #     legend.text = element_text(size = 12, face = "bold")
# #   )
# # ggsave("../output/GDP_per_capita_agency_Saba.jpg",  width = 10, height = 7)
# 
# 
# ggplot(avg_country_year, aes(YEAR_WAVE)) +
#   geom_line(aes(y = solidarity, color = "Solidarity"), size = 0.6)+ 
#   geom_line(aes(y = agency, color = "Agency"), size = 0.6) + facet_wrap(~COUNTRYNEW) + 
#   labs(title = "Solidarity and Agency for G20 countries between 2006-2023") +
#   scale_color_manual(values = c("Solidarity" = "blue", "Agency" = "orange")) +
#   theme_minimal() + 
#   theme(
#     strip.text = element_text(size = 12, face = 'bold'),
#     axis.text = element_text(size = 11, color = "black"),
#     plot.title = element_text(hjust = 0, vjust = 1, size = 16, face = "bold"), 
#     axis.title = element_blank(), 
#     legend.position = "top",
#     legend.title = element_blank(), 
#     legend.text = element_text(size = 12, face = "bold"))
# ggsave("../output/solidarity_agency.jpg",  width = 10, height = 7)

# scale_factor  <- diff(range(avg_country_year$solidarity)) / diff(range(avg_country_year$GDP_per_capita))
# ggplot(avg_country_year, aes(YEAR_WAVE)) +
#   geom_line(aes(y = solidarity, color = "solidarity"))+
#   geom_line(aes(y = agency, color = "agency"))+
#   geom_line(aes(y = (GDP_per_capita - min(GDP_per_capita)) * scale_factor + min(solidarity), 
#                 color = "GDP_per_capita")) + facet_wrap(~COUNTRYNEW) +
#   scale_y_continuous(
#     name = "solidarity/agency",
#     sec.axis = sec_axis(
#       transform = ~ (. - min(avg_country_year$solidarity)) / scale_factor + 
#         min(avg_country_year$GDP_per_capita),
#       name = "GDP_per_capita"
#     )
#   ) + 
#   labs(title = "Solidarity, Agnecy and GDP_per_capita (in constant 2015$),  G20 countries, 2006-2023") +
#   scale_color_manual(values = c("solidarity" = "blue", "GDP_per_capita" = "red", "agency" = "#F4A460")) +
#   theme_minimal() +  theme(
#     strip.text = element_text(size = 11, face = 'bold'),
#     axis.text = element_text(size = 11, color = "black"),
#     plot.title = element_text(hjust = 0, vjust = 1, size = 14, face = "bold"), 
#     legend.position = "top",
#     legend.title = element_blank(), 
#     legend.text = element_text(size = 12, face = "bold"), 
#     panel.spacing = unit(1, "lines"))
# 
# ggsave("../output/GDP_per_capita_solidarity_agency.jpg", width = 15, height = 9)
# 
#############################################################

### Regressions ####
model <- lm(cor_sol ~ type, data = country_stat)
summary(model)

model <- lm(cor_sol_lag ~ type, data = country_stat)
summary(model)

model <- lm(cor_agency ~ type, data =  country_stat %>% filter(COUNTRYNEW!= "Turkey"))
summary(model)

model <- lm(cor_agency_lag ~ type, data = country_stat %>% filter(COUNTRYNEW!= "Turkey"))
summary(model)

model <- lm(cor_sol_lag ~ first_solidarity, data = country_stat)
summary(model)

model <- lm(cor_sol ~ first_solidarity, data = country_stat)
summary(model)

model <- lm(cor_sol ~ income_group, data = country_stat)
summary(model)

model <- lm(cor_sol_lag ~ income_group, data = country_stat)
summary(model)

model <- lm(cor_agency ~ income_group, data = country_stat)
summary(model)


### This correlation can be good - agency is correlated with GDP for developing countries. 
model <- lm(agency ~   Type*GDP_per_capita + COUNTRYNEW , data = avg_country_year)
summary(model)

model <- lm(agency ~   Type*GDP_lag + COUNTRYNEW , data = avg_country_year)
summary(model)

### solidarity is correlated with GDP for developing countries, it is also correlated for developed countries. 
model <- lm(solidarity ~  Type*GDP_lag + COUNTRYNEW  , data = avg_country_year)
summary(model)

model <- lm(solidarity ~  Type*GDP_per_capita + COUNTRYNEW  , data = avg_country_year)
summary(model)

model_sol <- lm(
  solidarity ~ Type * GDP_per_capita+ factor(YEAR_WAVE)+ factor(COUNTRYNEW) ,
  data = avg_country_year
)
round(coeftest(model_sol, vcov = vcovCL, cluster = ~COUNTRYNEW),4)

model_agency <- lm(
  agency ~ Type * GDP_per_capita + factor(YEAR_WAVE)+ factor(COUNTRYNEW), 
  data = avg_country_year  %>% filter(COUNTRYNEW!= "Turkey")
)
round(coeftest(model_agency, vcov = vcovCL, cluster = ~COUNTRYNEW),4)

model_agency <- lm(
  agency ~ Type * GDP_per_capita + factor(YEAR_WAVE)+ factor(COUNTRYNEW), 
  data = avg_country_year)
round(coeftest(model_agency, vcov = vcovCL, cluster = ~COUNTRYNEW),4)


model_agency <- lm(
  agency ~ Type + factor(YEAR_WAVE)+ factor(COUNTRYNEW), 
  data = avg_country_year
  )
round(coeftest(model_agency, vcov = vcovCL, cluster = ~COUNTRYNEW),4)

model_sol <- lm(
  solidarity ~ Type + factor(YEAR_WAVE)+ factor(COUNTRYNEW), 
  data = avg_country_year
)
round(coeftest(model_sol, vcov = vcovCL, cluster = ~COUNTRYNEW),4)


## Average per country bar charts ####
### Solidarity and GDP per capita correlations bar charts by developing/developed ####
p1 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW,cor_sol), y = cor_sol, fill = type)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = paste(cor_sol_p.value, ",", round(cor_sol,2) )),
            position = position_dodge(width = 0.7), vjust = 0.5, hjust = 0.5, 
            size = 6, color = "black", fontface = "bold") +  # White text with bold font
  scale_fill_brewer(palette = "Set2") + 
  labs(
    title = str_wrap("Correlation of Solidarity and GDP per capita 2006-2023", width = 100),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", color = "gray40"),  # Subtitle style
    legend.title =  element_blank(),
    axis.text = element_text(size = 15, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16), 
    legend.position = "top")
ggsave("../pictures/correlations_sol_GDP_barchart.jpg",  width = 17, height = 8, plot = p1)


p2 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW, cor_sol_lag), y = cor_sol_lag, fill = type)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = paste0(cor_sol_lag_p.value, ",", round(cor_sol_lag,2))),
            position = position_dodge(width = 0.7), vjust = 0.5, hjust = 0.5,
            size = 6, color = "black", fontface = "bold") + 
  scale_fill_brewer(palette = "Set2") + 
  # Labels & Titles
  labs(
    title = str_wrap("Correlation of Solidarity and GDP per capita with one lag 2006-2023", width = 100),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_blank(),
    legend.title =  element_blank(),
    axis.text = element_text(size = 15, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "top")
ggsave("../pictures/correlations_sol_GDP_lag_barchart.jpg",  width = 17, height = 8, plot = p2)


p3 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW, cor_sol_diff), y = cor_sol_diff, fill = type)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = paste0(cor_sol_diff_p.value, ",", round(cor_sol_diff,2))),
            position = position_dodge(width = 0.7), vjust = 0.5, hjust = 0.5,
            size = 6, color = "black", fontface = "bold") + 
  scale_fill_brewer(palette = "Set2") + 
  # Labels & Titles
  labs(
    title = str_wrap("Correlation of Solidarity and GDP per capita first difference 2006-2023", width = 100),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_blank(),
    legend.title =  element_blank(),
    axis.text = element_text(size = 15, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "top")
ggsave("../pictures/correlations_sol_GDP_diff_barchart.jpg",  width = 17, height = 8, plot = p3)


p4 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW, cor_sol_growth), y = cor_sol_growth, fill = type)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = paste0(cor_sol_growth_p.value, ",", round(cor_sol_growth,2))),
            position = position_dodge(width = 0.7), vjust = 0.5, hjust = 0.5,
            size = 6, color = "black", fontface = "bold") + 
  scale_fill_brewer(palette = "Set2") + 
  # Labels & Titles
  labs(
    title = str_wrap("Correlation of Solidarity and GDP growth 2006-2023", width = 100),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_blank(),
    legend.title =  element_blank(),
    axis.text = element_text(size = 15, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "top")
ggsave("../pictures/correlations_sol_GDP_growth_barchart.jpg",  width = 17, height = 8, plot = p4)

# ggplot(country_stat, aes(x = GDP_per_capita , y = cor_agency)) +
#   geom_point(color = "blue", size =1) +
#   geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "solid") +
#   theme_minimal() + labs(title = "Correlation between GDP_per_capita and Solidarity for each year across G20 countries") +
#   geom_text(aes(label = country_code), vjust = -1, size = 2) +
#   theme(
#     strip.text = element_text(size = 15, face = 'bold'),
#     axis.text = element_text(size = 11, color = "black"),
#     plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
#     legend.text = element_text(size = 12, face = "bold")
#   )



#cor.test(country_stat$cor_agency_lag, country_stat$type)

### Agency and GDP per capita correlations bar charts by developing/developed ####
p1 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW,cor_agency), y = cor_agency, fill = type)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = paste(cor_agency_p.value, ",", round(cor_agency,2) )),
            position = position_dodge(width = 0.7), vjust = 0.5, hjust = 0.5, 
            size = 6, color = "black", fontface = "bold") +  # White text with bold font
  scale_fill_brewer(palette = "Set2") + 
  labs(
    title = str_wrap("Correlation of Agency and GDP per capita 2006-2023", width = 100),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", color = "gray40"),  # Subtitle style
    legend.title =  element_blank(),
    axis.text = element_text(size = 15, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16), 
    legend.position = "top")

ggsave("../pictures/correlations_agency_GDP_barchart.jpg",  width = 17, height = 8, plot = p1)

p2 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW, cor_agency_lag), y = cor_agency_lag, fill = type)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = paste0(cor_agency_lag_p.value, ",", round(cor_agency_lag,2))),
            position = position_dodge(width = 0.7), vjust = 0.5, hjust = 0.5,
            size = 6, color = "black", fontface = "bold") + 
  scale_fill_brewer(palette = "Set2") + 
  # Labels & Titles
  labs(
    title = str_wrap("Correlation of Agency and GDP per capita with one lag 2006-2023", width = 100),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_blank(),
    legend.title =  element_blank(),
    axis.text = element_text(size = 15, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "top")
ggsave("../pictures/correlations_agency_GDP_lag_barchart.jpg",  width = 17, height = 8, plot = p2)

p3 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW,cor_agency_diff), y = cor_agency_diff, fill = type)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = paste(cor_agency_diff_p.value, ",", round(cor_agency_diff,2) )),
            position = position_dodge(width = 0.7), vjust = 0.5, hjust = 0.5, 
            size = 6, color = "black", fontface = "bold") +  # White text with bold font
  scale_fill_brewer(palette = "Set2") + 
  labs(
    title = str_wrap("Correlation of Agency and GDP per capita first difference 2006-2023", width = 100),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", color = "gray40"),  # Subtitle style
    legend.title =  element_blank(),
    axis.text = element_text(size = 15, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16), 
    legend.position = "top")

ggsave("../pictures/correlations_agency_GDP_diff_barchart.jpg",  width = 17, height = 8, plot = p3)

p4 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW,cor_agency_growth), y = cor_agency_growth, fill = type)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = paste(cor_agency_growth_p.value, ",", round(cor_agency_growth,2) )),
            position = position_dodge(width = 0.7), vjust = 0.5, hjust = 0.5, 
            size = 6, color = "black", fontface = "bold") +  # White text with bold font
  scale_fill_brewer(palette = "Set2") + 
  labs(
    title = str_wrap("Correlation of Agency and GDP per capita growth 2006-2023", width = 100),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", color = "gray40"),  # Subtitle style
    legend.title =  element_blank(),
    axis.text = element_text(size = 15, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16), 
    legend.position = "top")
ggsave("../pictures/correlations_agency_growth_barchart.jpg",  width = 17, height = 8, plot = p4)



p2 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW, D_A), y = D_A, fill = type)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = round(D_A,2)),
            position = position_dodge(width = 0.7), vjust = 0.5, 
            size = 5, color = "black", fontface = "bold") + 
  scale_fill_brewer(palette = "Set2") + 
  # Labels & Titles
  labs(
    title = str_wrap("D_A for G20 countries 2006-2023", width = 50),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 14, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_blank(),
    legend.title =  element_blank(),
    axis.text = element_text(size = 12, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "top")
a <- grid.arrange(p1, p2, ncol = 2)
ggsave("../Output/D_S_A_barchart.jpg",  width = 12, height = 7, plot = a)

p1 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW, D_A_growth), y = D_A_growth, fill = type)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = round(D_A_growth,2)),
            position = position_dodge(width = 0.7), vjust = 0.5, 
            size = 5, color = "black", fontface = "bold") + 
  scale_fill_brewer(palette = "Set2") + 
  # Labels & Titles
  labs(
    title = str_wrap("D_A_growth for G20 countries 2006-2023", width = 50),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 14, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_blank(),
    legend.title =  element_blank(),
    axis.text = element_text(size = 12, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "top")

p2 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW, D_S_growth), y = D_S_growth, fill = type)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = round(D_S_growth,2)),
            position = position_dodge(width = 0.7), vjust = 0.5, 
            size = 5, color = "black", fontface = "bold") + 
  scale_fill_brewer(palette = "Set2") + 
  # Labels & Titles
  labs(
    title = str_wrap("D_S_growth for G20 countries 2006-2023", width = 50),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 14, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_blank(),
    legend.title =  element_blank(),
    axis.text = element_text(size = 12, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "top")
a <- grid.arrange(p1, p2, ncol = 2)
ggsave("../output/D_S_A_growth.jpg", plot = a, width = 12, height = 6)

# p <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW,adjusted_R2_decoupling), y = adjusted_R2_decoupling, fill = type)) +
#   geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
#   geom_text(aes(label = round(adjusted_R2_decoupling,2)),
#             position = position_dodge(width = 0.7), vjust = 0.5, 
#             size = 5, color = "black", fontface = "bold") +  # White text with bold font
#   scale_fill_brewer(palette = "Set2") + 
#   labs(
#     title = str_wrap("Adjusted R2 Decoupling for G20 countries 2006-2023", width = 50),  # Wrapping title for better display
#     y = str_wrap("", width = 50)) +
#   theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
#   theme(
#     plot.title = element_text(hjust = 0.5, vjust = 1, size = 14, face = "bold", color = "darkblue"),  # Emphasize title
#     plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", color = "gray40"),  # Subtitle style
#     legend.title =  element_blank(),
#     axis.text = element_text(size = 14, face = "bold", angle = 20),  # Subtitle style
#     axis.title = element_blank(),
#     legend.text = element_text(size = 16), 
#     legend.position = "top")
#ggsave("../output/D_R2.jpg", plot = p, width = 10, height = 5)


### Solidarity and GDP per capita correlations bar charts by income groups ####
p1 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW,cor_sol), y = cor_sol, fill = income_group)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = paste(cor_sol_p.value, ",", round(cor_sol,2) )),
            position = position_dodge(width = 0.7), vjust = 0.5, hjust = 0.5, 
            size = 6, color = "black", fontface = "bold") +  # White text with bold font
  scale_fill_brewer(palette = "Set2") + 
  labs(
    title = str_wrap("Correlation of Solidarity and GDP per capita 2006-2023", width = 100),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", color = "gray40"),  # Subtitle style
    legend.title =  element_blank(),
    axis.text = element_text(size = 15, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16), 
    legend.position = "top")
ggsave("../pictures/correlations_sol_GDP_barchart_income.jpg",  width = 17, height = 8, plot = p1)


p2 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW, cor_sol_lag), y = cor_sol_lag, fill = income_group)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = paste0(cor_sol_lag_p.value, ",", round(cor_sol_lag,2))),
            position = position_dodge(width = 0.7), vjust = 0.5, hjust = 0.5,
            size = 6, color = "black", fontface = "bold") + 
  scale_fill_brewer(palette = "Set2") + 
  # Labels & Titles
  labs(
    title = str_wrap("Correlation of Solidarity and GDP per capita with one lag 2006-2023", width = 100),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_blank(),
    legend.title =  element_blank(),
    axis.text = element_text(size = 15, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "top")
ggsave("../pictures/correlations_sol_GDP_lag_barchart_income.jpg",  width = 17, height = 8, plot = p2)


p3 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW, cor_sol_diff), y = cor_sol_diff, fill = income_group)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = paste0(cor_sol_diff_p.value, ",", round(cor_sol_diff,2))),
            position = position_dodge(width = 0.7), vjust = 0.5, hjust = 0.5,
            size = 6, color = "black", fontface = "bold") + 
  scale_fill_brewer(palette = "Set2") + 
  # Labels & Titles
  labs(
    title = str_wrap("Correlation of Solidarity and GDP per capita first difference 2006-2023", width = 100),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_blank(),
    legend.title =  element_blank(),
    axis.text = element_text(size = 15, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "top")
ggsave("../pictures/correlations_sol_GDP_diff_barchart_income.jpg",  width = 17, height = 8, plot = p3)


p4 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW, cor_sol_growth), y = cor_sol_growth, fill = income_group)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = paste0(cor_sol_growth_p.value, ",", round(cor_sol_growth,2))),
            position = position_dodge(width = 0.7), vjust = 0.5, hjust = 0.5,
            size = 6, color = "black", fontface = "bold") + 
  scale_fill_brewer(palette = "Set2") + 
  # Labels & Titles
  labs(
    title = str_wrap("Correlation of Solidarity and GDP growth 2006-2023", width = 100),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_blank(),
    legend.title =  element_blank(),
    axis.text = element_text(size = 15, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "none")
ggsave("../pictures/correlations_sol_GDP_growth_barchart_income.jpg",  width = 17, height = 8, plot = p4)



## Agency and GDP per capita correlations bar charts by income groups ####
p1 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW,cor_agency), y = cor_agency, fill = income_group)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = paste(cor_agency_p.value, ",", round(cor_agency,2) )),
            position = position_dodge(width = 0.7), vjust = 0.5, hjust = 0.5, 
            size = 6, color = "black", fontface = "bold") +  # White text with bold font
  scale_fill_brewer(palette = "Set2") + 
  labs(
    title = str_wrap("Correlation of Agency and GDP per capita 2006-2023", width = 100),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", color = "gray40"),  # Subtitle style
    legend.title =  element_blank(),
    axis.text = element_text(size = 15, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16), 
    legend.position = "top")

ggsave("../pictures/correlations_agency_GDP_barchart_income.jpg",  width = 17, height = 8, plot = p1)

p2 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW, cor_agency_lag), y = cor_agency_lag, fill = income_group)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = paste0(cor_agency_lag_p.value, ",", round(cor_agency_lag,2))),
            position = position_dodge(width = 0.7), vjust = 0.5, hjust = 0.5,
            size = 6, color = "black", fontface = "bold") + 
  scale_fill_brewer(palette = "Set2") + 
  # Labels & Titles
  labs(
    title = str_wrap("Correlation of Agency and GDP per capita with one lag 2006-2023", width = 100),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_blank(),
    legend.title =  element_blank(),
    axis.text = element_text(size = 15, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "top")
ggsave("../pictures/correlations_agency_GDP_lag_barchart_income.jpg",  width = 17, height = 8, plot = p2)

p3 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW,cor_agency_diff), y = cor_agency_diff, fill = income_group)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = paste(cor_agency_diff_p.value, ",", round(cor_agency_diff,2) )),
            position = position_dodge(width = 0.7), vjust = 0.5, hjust = 0.5, 
            size = 6, color = "black", fontface = "bold") +  # White text with bold font
  scale_fill_brewer(palette = "Set2") + 
  labs(
    title = str_wrap("Correlation of Agency and GDP per capita first difference 2006-2023", width = 100),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", color = "gray40"),  # Subtitle style
    legend.title =  element_blank(),
    axis.text = element_text(size = 15, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16), 
    legend.position = "top")

ggsave("../pictures/correlations_agency_GDP_diff_barchart_income.jpg",  width = 17, height = 8, plot = p3)

p4 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW,cor_agency_growth), y = cor_agency_growth, fill = income_group)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = paste(cor_agency_growth_p.value, ",", round(cor_agency_growth,2) )),
            position = position_dodge(width = 0.7), vjust = 0.5, hjust = 0.5, 
            size = 6, color = "black", fontface = "bold") +  # White text with bold font
  scale_fill_brewer(palette = "Set2") + 
  labs(
    title = str_wrap("Correlation of Agency and GDP per capita growth 2006-2023", width = 100),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", color = "gray40"),  # Subtitle style
    legend.title =  element_blank(),
    axis.text = element_text(size = 15, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16), 
    legend.position = "top")
ggsave("../pictures/correlations_agency_growth_barchart_income.jpg",  width = 17, height = 8, plot = p4)

### Solidarity and Agency correlations bar charts by developing/developed ####
p1 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW,cor_sol_agency), y = cor_sol_agency, fill = type)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = paste(cor_sol_agency_p.value, ",", round(cor_sol_agency,2) )),
            position = position_dodge(width = 0.7), vjust = 0.5, hjust = 0.5, 
            size = 6, color = "black", fontface = "bold") +  # White text with bold font
  scale_fill_brewer(palette = "Set2") + 
  labs(
    title = str_wrap("Correlation of Solidarity and Agency 2006-2023", width = 100),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", color = "gray40"),  # Subtitle style
    legend.title =  element_blank(),
    axis.text = element_text(size = 15, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16), 
    legend.position = "top")
ggsave("../pictures/correlations_sol_agency_barchart.jpg",  width = 17, height = 8, plot = p1)


p2 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW,cor_sol_agency), y = cor_sol_agency, fill = income_group)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = paste(cor_sol_agency_p.value, ",", round(cor_sol_agency,2) )),
            position = position_dodge(width = 0.7), vjust = 0.5, hjust = 0.5, 
            size = 6, color = "black", fontface = "bold") +  # White text with bold font
  scale_fill_brewer(palette = "Set2") + 
  labs(
    title = str_wrap("Correlation of Solidarity and Agency 2006-2023", width = 100),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", color = "gray40"),  # Subtitle style
    legend.title =  element_blank(),
    axis.text = element_text(size = 15, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16), 
    legend.position = "top")
ggsave("../pictures/correlations_sol_agency_barchart_income.jpg",  width = 17, height = 8, plot = p2)


## Average per country box plots ######
ggplot(country_stat %>% pivot_longer(
  cols = c("mean_solidarity", "mean_agency"), 
  names_to = "Score",
  values_to = "Value"), aes(x = type, y = Value, fill = type)) +
  geom_boxplot() + facet_wrap(~Score) + 
  labs(
    title = "Box plots of Scores for developed vs developing countries",
    x = "",
    y = "Score") + theme(
      strip.text = element_text(size = 15, face = 'bold'),
      axis.text = element_text(size = 12, color = "black"),
      plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"), 
      legend.position = "top",
      legend.title = element_blank(), 
      legend.text = element_text(size = 12, face = "bold")) + 
  geom_text_repel(aes(label = COUNTRYNEW),
                  position = position_jitter(width = 0.1),
                  size = 4) 
ggsave("../pictures/boxplot_score_type.jpg", width = 12,height = 6)

ggplot(country_stat %>% pivot_longer(
  cols = c("mean_solidarity", "mean_agency"), 
  names_to = "Score",
  values_to = "Value"), aes(x = income_group, y = Value, fill = income_group)) +
  geom_boxplot() + facet_wrap(~Score) + 
  labs(
    title = "Box plots of Scores for income groups",
    x = "",
    y = "Score") + theme(
      strip.text = element_text(size = 15, face = 'bold'),
      axis.text = element_text(size = 12, color = "black"),
      plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"), 
      legend.position = "top",
      legend.title = element_blank(), 
      legend.text = element_text(size = 12, face = "bold")) + 
  geom_text_repel(aes(label = COUNTRYNEW),
                  position = position_jitter(width = 0.1),
                  size = 4) 
ggsave("../pictures/boxplot_score_income.jpg", width = 12,height = 6)

ggplot(country_stat %>% pivot_longer(
  cols = c("cor_agency", "cor_sol"), 
  names_to = "Score",
  values_to = "Value"), aes(x = type, y = Value, fill = type)) +
  geom_boxplot() + facet_wrap(~Score) + 
  labs(x = "",
    y = "Score") + theme(
      strip.text = element_text(size = 15, face = 'bold'),
      axis.text = element_text(size = 12, color = "black"),
      plot.title = element_blank(),
      legend.position = "top",
      legend.title = element_blank(), 
      legend.text = element_text(size = 12, face = "bold")) + 
  labs(
    title = "Box plots of Scores for developed vs developing countries",
    x = "",
    y = "Score") + theme(
      strip.text = element_text(size = 15, face = 'bold'),
      axis.text = element_text(size = 12, color = "black"),
      plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"), 
      legend.position = "top",
      legend.title = element_blank(), 
      legend.text = element_text(size = 12, face = "bold")) + 
  geom_text_repel(aes(label = COUNTRYNEW),
                  position = position_jitter(width = 0.1),
                  size = 4) 
ggsave("../pictures/boxplot_correlation_type.jpg", width = 12,height = 6)

ggplot(country_stat %>% pivot_longer(
  cols = c("cor_agency", "cor_sol"), 
  names_to = "Score",
  values_to = "Value"), aes(x = income_group, y = Value, fill = income_group)) +
  geom_boxplot() + facet_wrap(~Score) + 
  labs(x = "",
       y = "Score") + theme(
         strip.text = element_text(size = 15, face = 'bold'),
         axis.text = element_text(size = 12, color = "black"),
         plot.title = element_blank(),
         legend.position = "top",
         legend.title = element_blank(), 
         legend.text = element_text(size = 12, face = "bold")) + 
  labs(
    title = "Box plots of Scores for income groups",
    x = "",
    y = "Score") + theme(
      strip.text = element_text(size = 15, face = 'bold'),
      axis.text = element_text(size = 12, color = "black"),
      plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"), 
      legend.position = "top",
      legend.title = element_blank(), 
      legend.text = element_text(size = 12, face = "bold")) + 
  geom_text_repel(aes(label = COUNTRYNEW),
                  position = position_jitter(width = 0.1),
                  size = 4) 
ggsave("../pictures/boxplot_correlation_income.jpg", width = 12,height = 6)

# ggplot(country_stat %>% pivot_longer(
#   cols = c("D", "D_S", "D", "D_A"), 
#   names_to = "Score",
#   values_to = "Value"), aes(x = income_group, y = Value, fill = income_group)) +
#   geom_boxplot() + facet_wrap(~Score) + 
#   labs(
#     title = "Box plots of Scores for different income groups",
#     x = "",
#     y = "Score") + theme(
#       strip.text = element_text(size = 15, face = 'bold'),
#       axis.text = element_text(size = 12, color = "black", angle = 20),
#       plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"), 
#       legend.position = "top",
#       legend.title = element_blank(), 
#       legend.text = element_text(size = 12, face = "bold"))
# ggsave("../output/boxplot_decoupling_income.jpg", width = 10,height = 5)
# 

## Across countries indices #### 
# for(i in 2006:2023)
# {
#   dd <- avg_country_year %>% filter(YEAR_WAVE==i)
#   model <- lm(GDP_per_capita ~ solidarity+agency, data = dd)
#   print(summary(model))
#   across_countries[across_countries$YEAR_WAVE==i, "D_R2"] <- 1- summary(model)$adj.r.squared
# }
# write.xlsx(across_countries, "../Data/across_countries_decoupling.xlsx")
# 

# for(i in 2006:2023){
#   
#   dd1 <- avg_country_year %>% filter(YEAR_WAVE==i, Type %in% 'Developed')
#   model1 <- lm(GDP_per_capita ~ solidarity+agency, data = dd1)
#   across_countries_type[across_countries_type$YEAR_WAVE==i &
#                           across_countries_type$Type  %in% 'Developed'
#                         , "R2"] <- 1- summary(model1)$adj.r.squared
#   
#   dd2 <- avg_country_year %>% filter(YEAR_WAVE==i, Type %in% 'Developing')
#   model2 <- lm(GDP_per_capita ~ solidarity+agency, data = dd2)
#   across_countries_type[across_countries_type$YEAR_WAVE==i &
#                           across_countries_type$Type  %in% 'Developing'
#                         , "R2"] <- 1- summary(model2)$adj.r.squared
#   
# }



### Time series across counties for all ####
across_countries <- avg_country_year %>% ungroup() %>% group_by(YEAR_WAVE) %>% 
  summarise(cor_sol_1 = cor(solidarity, GDP_per_capita, use = "complete.obs"),
            cor_agency = cor(agency, GDP_per_capita, use = "complete.obs"), 
            cor_sol_growth = cor(solidarity, growth, use = "pairwise.complete.obs"), 
            cor_agency_growth = cor(agency, growth, use = "pairwise.complete.obs"), 
            D_S = 1- cor_sol_1, 
            D_A = 1- cor_agency, 
            D = (D_S + D_A)/2, 
            D_S_growth = 1 - cor_sol_growth, 
            D_A_growth = 1 - cor_agency_growth,
            D_growth = (D_S_growth + D_A_growth)/2)

corr_long <- across_countries %>% pivot_longer(cols = c("D_S", "D_A", "D"), 
                                               names_to = "variable", values_to = "value")
last_points <- corr_long %>%
  group_by(variable) %>%
  filter(YEAR_WAVE == max(YEAR_WAVE)) %>% ungroup()
ggplot(corr_long, aes(x = YEAR_WAVE, y = value, color = variable)) +
  geom_line(linewidth = 0.8) +
  theme_minimal() +   
  geom_smooth(method = "lm", se = TRUE, aes(fill = variable), alpha = 0.2, show.legend = TRUE) +
  labs(
    title = "Decoupling indices from 2006-2023 across G20 countries",
    x = "Year",
    y = "value", 
    color = "variable", 
    fill = "variable"
  ) + 
  geom_text(
    data = last_points,
    aes(label = variable, color = variable),           
    size = 4,
    hjust = 0,
    vjust = 0,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_color_hue(l = 40, c = 100) +
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"), 
    legend.position = "none",
    legend.title = element_blank(), 
    legend.text = element_text(size = 12, face = "bold")
  ) + expand_limits(x = max(corr_long$YEAR_WAVE) + 1)  
ggsave("../Output/Decoupling_time_all.jpg",  width = 10, height = 4)

corr_long <- across_countries %>% pivot_longer(cols = c("D_S_growth", "D_A_growth", "D_growth"), 
                                               names_to = "variable", values_to = "value")
last_points <- corr_long %>%
  group_by(variable) %>%
  filter(YEAR_WAVE == max(YEAR_WAVE)) %>% ungroup()
ggplot(corr_long, aes(x = YEAR_WAVE, y = value, color = variable)) +
  geom_line(linewidth = 0.8) +
  theme_minimal() +   
  geom_smooth(method = "lm", se = TRUE, aes(fill = variable), alpha = 0.2, show.legend = TRUE) +
  labs(
    title = "Decoupling growth indices from 2006-2023 across G20 countries",
    x = "Year",
    y = "value", 
    color = "variable", 
    fill = "variable"
  ) + 
  geom_text(
    data = last_points,
    aes(label = variable, color = variable),           
    size = 4,
    hjust = 0,
    vjust = 0,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_color_hue(l = 40, c = 100) +
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"), 
    legend.position = "none",
    legend.title = element_blank(), 
    legend.text = element_text(size = 12, face = "bold")
  ) + expand_limits(x = max(corr_long$YEAR_WAVE) + 1) 

ggsave("../output/Decoupling_growth_time_all.jpg",  width = 8, height = 4)


# corr_long <- across_countries %>% pivot_longer(cols = c("D", "D_growth"), 
#                                                names_to = "variable", values_to = "value")
# last_points <- corr_long %>%
#   group_by(variable) %>%
#   filter(YEAR_WAVE == max(YEAR_WAVE)) %>% ungroup()
# 
# 
# ggplot(corr_long, aes(x = YEAR_WAVE, y = value, color = variable)) +
#   geom_line(linewidth = 0.8) +
#   theme_minimal() +   
#   geom_smooth(method = "lm", se = TRUE, aes(fill = variable), alpha = 0.2, show.legend = TRUE) +
#   labs(
#     title = "Decoupling indices from 2006-2023 across G20 countries",
#     x = "Year",
#     y = "value", 
#     color = "variable", 
#     fill = "variable"
#   ) + 
#   geom_text(
#     data = last_points,
#     aes(label = variable, color = variable),           
#     size = 4,
#     hjust = 0.2,
#     vjust = 1,
#     fontface = "bold",
#     show.legend = FALSE
#   ) +
#   scale_color_hue(l = 40, c = 100) +
#   theme(
#     strip.text = element_text(size = 15, face = 'bold'),
#     axis.text = element_text(size = 11, color = "black"),
#     plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"), 
#     legend.position = "none",
#     legend.title = element_blank(), 
#     legend.text = element_text(size = 12, face = "bold")
#   ) + expand_limits(x = max(corr_long$YEAR_WAVE) + 1)  
# 
# ggsave("../Output/Decoupling_R2_D_time_all_countries.jpg",  width = 8, height = 4)



### Time series across countries for Developed vs Developing ####
across_countries_type <- avg_country_year %>% ungroup() %>% group_by(YEAR_WAVE, Type) %>% 
  summarise(cor_sol_1 = cor(solidarity, GDP_per_capita, use = "complete.obs"),
            cor_agency = cor(agency, GDP_per_capita, use = "complete.obs"), 
            cor_sol_growth = cor(solidarity, growth, use = "pairwise.complete.obs"), 
            cor_agency_growth = cor(agency, growth, use = "pairwise.complete.obs"), 
            D_S = 1- cor_sol_1, 
            D_A = 1- cor_agency, 
            D = (D_S + D_A)/2, 
            D_S_growth = 1 - cor_sol_growth, 
            D_A_growth = 1 - cor_agency_growth,
            D_growth = (D_S_growth + D_A_growth)/2)
p1 <- ggplot(across_countries_type, aes(x = YEAR_WAVE, y = D_A, color = Type)) + 
  geom_line(linewidth = 0.6) +
  theme_minimal() +   
  geom_smooth(method = "lm", se = TRUE, aes(fill = Type), alpha = 0.2, show.legend = TRUE) +
  labs(
    title = str_wrap("D_A (Agency Decoupling) from 2006-2023 across G20 countries", width = 30),
    x = "Year",
    y = "value", 
    color = "variable", 
    fill = "variable"
  ) + theme_minimal()+
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 14, face = "bold"), 
    legend.position = "top",
    legend.title = element_blank(), 
    legend.text = element_text(size = 12, face = "bold")
  ) + expand_limits(x = max(corr_long$YEAR_WAVE) + 1)  

p2 <- ggplot(across_countries_type, aes(x = YEAR_WAVE, y = D_S, color = Type)) + 
  geom_line(linewidth = 0.6) +
  theme_minimal() +   
  geom_smooth(method = "lm", se = TRUE, aes(fill = Type), alpha = 0.2, show.legend = TRUE) +
  labs(
    title = str_wrap("D_S (Solidarity Decoupling) from 2006-2023 across G20 countries", width = 30),
    x = "Year",
    y = "value", 
    color = "variable", 
    fill = "variable"
  ) + theme_minimal()+
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 14, face = "bold"), 
    legend.position = "top",
    legend.title = element_blank(), 
    legend.text = element_text(size = 12, face = "bold")
  ) + expand_limits(x = max(corr_long$YEAR_WAVE) + 1)  
a <- grid.arrange(p1, p2, ncol=2)

ggsave("../output/D_S_A_developed_developing.jpg", plot = a, width = 10, height = 4)

p3 <- ggplot(across_countries_type, aes(x = YEAR_WAVE, y = D, color = Type)) + 
  geom_line(linewidth = 0.6) +
  theme_minimal() +   
  geom_smooth(method = "lm", se = TRUE, aes(fill = Type), alpha = 0.2, show.legend = TRUE) +
  labs(
    title = str_wrap("Decoupling index (D) from 2006-2023 across G20 countries", width = 30),
    x = "Year",
    y = "value", 
    color = "variable", 
    fill = "variable"
  ) + theme_minimal()+
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 14, face = "bold"), 
    legend.position = "top",
    legend.title = element_blank(), 
    legend.text = element_text(size = 12, face = "bold")
  ) + expand_limits(x = max(corr_long$YEAR_WAVE) + 1)

p4 <- ggplot(across_countries_type, aes(x = YEAR_WAVE, y = D_growth, color = Type)) + 
  geom_line(linewidth = 0.6) +
  theme_minimal() +   
  geom_smooth(method = "lm", se = TRUE, aes(fill = Type), alpha = 0.2, show.legend = TRUE) +
  labs(
    title = str_wrap("Decoupling index (D_growth) from 2006-2023 across G20 countries", width = 30),
    x = "Year",
    y = "value", 
    color = "variable", 
    fill = "variable"
  ) + theme_minimal()+
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 14, face = "bold"), 
    legend.position = "top",
    legend.title = element_blank(), 
    legend.text = element_text(size = 12, face = "bold")
  ) + expand_limits(x = max(corr_long$YEAR_WAVE) + 1)
a <- grid.arrange(p3, p4, ncol=2)
ggsave("../output/Decoupling_developed_developing.jpg", plot = a, width = 10, height = 4)


p5 <- ggplot(across_countries_type, aes(x = YEAR_WAVE, y = D_A_growth, color = Type)) + 
  geom_line(linewidth = 0.6) +
  theme_minimal() +   
  geom_smooth(method = "lm", se = TRUE, aes(fill = Type), alpha = 0.2, show.legend = TRUE) +
  labs(
    title = str_wrap("D_A_growth from 2006-2023 across G20 countries", width = 30),
    x = "Year",
    y = "value", 
    color = "variable", 
    fill = "variable"
  ) + theme_minimal()+
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 14, face = "bold"), 
    legend.position = "top",
    legend.title = element_blank(), 
    legend.text = element_text(size = 12, face = "bold")
  ) + expand_limits(x = max(corr_long$YEAR_WAVE) + 1)  

p6 <- ggplot(across_countries_type, aes(x = YEAR_WAVE, y = D_S_growth, color = Type)) + 
  geom_line(linewidth = 0.6) +
  theme_minimal() +   
  geom_smooth(method = "lm", se = TRUE, aes(fill = Type), alpha = 0.2, show.legend = TRUE) +
  labs(
    title = str_wrap("D_S_growth from 2006-2023 across G20 countries", width = 30),
    x = "Year",
    y = "value", 
    color = "variable", 
    fill = "variable"
  ) + theme_minimal()+
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 14, face = "bold"), 
    legend.position = "top",
    legend.title = element_blank(), 
    legend.text = element_text(size = 12, face = "bold")
  ) + expand_limits(x = max(corr_long$YEAR_WAVE) + 1)  
a <- grid.arrange(p5, p6, ncol=2)

ggsave("../output/D_S_A_growth_developed_developing.jpg", plot = a, width = 10, height = 4)


### D_S_growth and D_A_growth across counties for Developed vs Developing ##
# p1 <- ggplot(across_countries_type, aes(x = YEAR_WAVE, y = D_growth, color = Type)) + 
#   geom_line(linewidth = 0.6) +
#   theme_minimal() +   
#   geom_smooth(method = "lm", se = TRUE, aes(fill = Type), alpha = 0.2, show.legend = TRUE) +
#   labs(
#     title = str_wrap("Decoupling growth from 2006-2023 across G20 countries", width = 30),
#     x = "Year",
#     y = "value", 
#     color = "variable", 
#     fill = "variable"
#   ) + theme_minimal()+
#   theme(
#     strip.text = element_text(size = 15, face = 'bold'),
#     axis.text = element_text(size = 11, color = "black"),
#     plot.title = element_text(hjust = 0.5, vjust = 1, size = 14, face = "bold"), 
#     legend.position = "top",
#     legend.title = element_blank(), 
#     legend.text = element_text(size = 12, face = "bold")
#   ) + expand_limits(x = max(corr_long$YEAR_WAVE) + 1)  
# 
# ggsave("../output/decoulpling_growth_type.jpg", plot = p1, width = 8, height = 4)


### Time series for Decoupling across counties for Income groups ####
across_countries_income <- avg_country_year %>% ungroup() %>% group_by(YEAR_WAVE, income_group) %>% 
  summarise(cor_sol_1 = cor(solidarity, GDP_per_capita, use = "complete.obs"),
            cor_agency = cor(agency, GDP_per_capita, use = "complete.obs"), 
            D_S = 1- cor_sol_1, 
            D_A = 1- cor_agency, 
            D = (D_S + D_A)/2, 
            cor_sol_growth = cor(solidarity, growth, use = "pairwise.complete.obs"), 
            cor_agency_growth = cor(agency, growth, use = "pairwise.complete.obs"), 
            D_S_growth = 1 - cor_sol_growth, 
            D_A_growth = 1 - cor_agency_growth,
            D_growth = (D_S_growth + D_A_growth)/2)
across_countries_income <- across_countries_income %>% 
  filter(income_group %in% c("High-income " , "Upper-middle-income "))
p1 <- ggplot(across_countries_income, aes(x = YEAR_WAVE, y = D_A, color = str_wrap(income_group, width = 20))) + 
  geom_line(linewidth = 0.6) +
  theme_minimal() +   
  geom_smooth(method = "lm", se = TRUE, aes(fill = str_wrap(income_group, width = 20)), alpha = 0.2, show.legend = TRUE) +
  labs(
    title = str_wrap("D_A (Agency Decoupling) from 2006-2023 across G20 countries", width = 30),
    x = "Year",
    y = "value", 
    color = "variable", 
    fill = "variable"
  ) + theme_minimal()+
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 14, face = "bold"), 
    legend.position = "top",
    legend.title = element_blank(), 
    legend.text = element_text(size = 12, face = "bold")
  ) + expand_limits(x = max(corr_long$YEAR_WAVE) + 1)  


p2 <- ggplot(across_countries_income, aes(x = YEAR_WAVE, y = D_S, color = str_wrap(income_group, width = 20))) + 
  geom_line(linewidth = 0.6) +
  theme_minimal() +   
  geom_smooth(method = "lm", se = TRUE, aes(fill = str_wrap(income_group, width = 20)), alpha = 0.2, show.legend = TRUE) +
  labs(
    title = str_wrap("D_S (Solidarity Decoupling) from 2006-2023 across G20 countries", width = 30),
    x = "Year",
    y = "value", 
    color = "variable", 
    fill = "variable"
  ) + theme_minimal()+
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 14, face = "bold"), 
    legend.position = "none",
    legend.title = element_blank(), 
    legend.text = element_text(size = 12, face = "bold")
  ) + expand_limits(x = max(corr_long$YEAR_WAVE) + 1)  
a <- grid.arrange(p1, p2, ncol=2)
ggsave("../output/D_S_A_income.jpg", plot = a, width = 10, height = 4)


p3 <- ggplot(across_countries_income, aes(x = YEAR_WAVE, y = D, color = str_wrap(income_group, width = 20))) + 
  geom_line(linewidth = 0.6) +
  theme_minimal() +   
  geom_smooth(method = "lm", se = TRUE, aes(fill = str_wrap(income_group, width = 20)), alpha = 0.2, show.legend = TRUE) +
  labs(
    title = str_wrap("Decoupling index (D) from 2006-2023 across G20 countries", width = 30),
    x = "Year",
    y = "value", 
    color = "variable", 
    fill = "variable"
  ) + theme_minimal()+
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 14, face = "bold"), 
    legend.position = "top",
    legend.title = element_blank(), 
    legend.text = element_text(size = 12, face = "bold")
  ) + expand_limits(x = max(corr_long$YEAR_WAVE) + 1)  


p4 <- ggplot(across_countries_income, aes(x = YEAR_WAVE, y = D_growth, color = str_wrap(income_group, width = 20))) + 
  geom_line(linewidth = 0.8) +
  theme_minimal() +   
  geom_smooth(method = "lm", se = TRUE, aes(fill = str_wrap(income_group, width = 20)), alpha = 0.2, show.legend = TRUE) +
  labs(
    title = str_wrap("Decoupling growth index (D_growth) from 2006-2023 across G20 countries", width = 30),
    x = "Year",
    y = "value", 
    color = "variable", 
    fill = "variable"
  ) + theme_minimal()+
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 14, face = "bold"), 
    legend.position = "top",
    legend.title = element_blank(), 
    legend.text = element_text(size = 12, face = "bold")
  ) + expand_limits(x = max(corr_long$YEAR_WAVE) + 1)  

a <- grid.arrange(p3, p4, ncol = 2)
ggsave("../output/decoulpling_growth_income.jpg", plot = a, width = 8, height = 4)

### Across countries per year scatter plots ####
ggplot(avg_country_year, aes(x = GDP_per_capita, y = solidarity)) +
  geom_point(color = "blue", size =1) + facet_wrap(~YEAR_WAVE) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "solid") +
  theme_minimal() + labs(title = "Correlation between GDP_per_capita and Solidarity for each year across G20 countries") +
  geom_text(aes(label = Country.Code), vjust = -1, size = 2) +
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
    legend.text = element_text(size = 12, face = "bold")
  )
ggsave("../Output/Solidarity_GDP_per_capita_countries.jpg",  width = 10, height = 7)


ggplot(avg_country_year, aes(x = GDP_per_capita, y = agency)) +
  geom_point(color = "blue", size =1) + facet_wrap(~YEAR_WAVE) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "solid") +
  theme_minimal() + labs(title = "Correlation between GDP_per_capita and Agency for each year across G20 countries") +
  geom_text(aes(label = Country.Code), vjust = -1, size = 2) +
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
    legend.text = element_text(size = 12, face = "bold")
  )
ggsave("../Output/Agency_GDP_per_capita_countries.jpg",  width = 10, height = 7)



## Decoupling indices
# ggplot(across_countries, aes(x = YEAR_WAVE, y = D)) +
#   geom_line(size = 1.2, color = "blue") +
#   scale_x_continuous(
#     breaks = seq(2006,
#                  2025,
#                  by = 5)) +
#   geom_point(color = "blue", size =1) +
#   geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "solid") +
#   theme_minimal() + labs(title = "Decoupling across G20 countries from 2006-2023") +
#   theme(
#     strip.text = element_text(size = 15, face = 'bold'),
#     axis.text = element_text(size = 11, color = "black"),
#     plot.title = element_text(hjust = 0.5, vjust = 1, size = 14, face = "bold"),
#     legend.position = "top",
#     legend.title = element_blank(),
#     legend.text = element_text(size = 12, face = "bold")
#   )
# ggsave("Output/Decoupling_time_sd.jpg",  width = 8, height = 4)
# 





