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
library(survey)
library(stargazer)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#formula <- as.formula(paste(question, "~", paste(vars, collapse = "+"))) 
df <- read.xlsx("../Data/average_country_year.xlsx")

country_stat <- read.xlsx("../output/country_statistics.xlsx")

df <- df %>% group_by(COUNTRYNEW) %>% mutate(
  GDP_z = (GDP - mean(GDP)), 
  solidarity_z = (mean_solidarity_1- mean(mean_solidarity_1)), 
  agency_z = (agency_score_1- mean(agency_score_1)), 
  year = factor(YEAR_WAVE))%>% ungroup()


#### GDP_z
m1 <- summary(lm(GDP_z ~  solidarity_z + agency_z, data = df))
capture.output(m1, file = "../output/within-R2.txt")
summary(lm(GDP_z ~  solidarity_z + agency_z + YEAR_WAVE, data = df))
summary(lm(GDP_z ~ solidarity_z + agency_z +year, data = df))
summary(lm(GDP_z ~ solidarity_z + agency_z + Country.Code + YEAR_WAVE, data = df))
summary(lm(GDP_z ~ mean_solidarity_1 + agency_score_1 + YEAR_WAVE + Country.Code, data = df))
summary(lm(GDP~ mean_solidarity_1 + YEAR_WAVE, data = df))

#### GDP
summary(lm(GDP ~ Country.Code + mean_solidarity_1 + agency_score_1, data = df))
summary(lm(GDP ~ Country.Code + mean_solidarity_1 + agency_score_1 + YEAR_WAVE, data = df))
summary(lm(GDP ~  mean_solidarity_1 + agency_score_1, data = df))
summary(lm(GDP ~  mean_solidarity_1 + agency_score_1 + YEAR_WAVE, data = df))


## GDP_growth
m1 <- summary(lm(growth ~ growth_solidarity + growth_agency, data = df))
summary(lm(growth ~  growth_solidarity + growth_agency + Country.Code, data = df))
summary(lm(growth ~ growth_solidarity + growth_agency + Country.Code + YEAR_WAVE, data = df))

summary(lm(growth ~  mean_solidarity_1 + agency_score_1, data = df))
summary(lm(growth ~ Country.Code + mean_solidarity_1 + agency_score_1, data = df))

capture.output(m1, file = "../output/within-R.txt")

#capture.output(summary(model), file = "output/model_summary.docx")
#stargazer(model, type = "text", out = "output/model_report.txt")
#table <- as.data.frame(summary(model)$coefficients)
#cor(country_stat$mean_solidarity, country_stat$GDP) 
#cor(country_stat$mean_agency, country_stat$GDP)

G20_countries <- c("Argentina", "Australia", "Brazil", "Canada", "China", "France", 
                   "Germany", "India", "Indonesia", "Italy", "Japan", "Mexico", "Russia", 
                   "Saudi Arabia", "South Africa", "South Korea", "Turkey", 
                   "United Kingdom", "United States")

for (country in G20_countries){
  dd <- df %>% filter(COUNTRYNEW %in% country)
  model <- lm(GDP ~ mean_solidarity_1+agency_score_1, data = dd)
  
  country_stat [country_stat$COUNTRYNEW %in% country, "adjusted_R2"] <- 
    summary(model)$adj.r.squared
  
  country_stat [country_stat$COUNTRYNEW %in% country, "R2"] <- 
    summary(model)$r.squared
  
}
country_stat$R2_decoupling <- 1- country_stat$R2
country_stat$adjusted_R2_decoupling <- 1- country_stat$adjusted_R2


write.xlsx(country_stat, "output/country_statistics_new.xlsx")



### check for demographics #### - continue this tomorrow - I should change the basic_stat to a short format data. 
df_indv <- read.csv("../Data/G20_solidarity_agency_demographics.csv")
df_indv$age_group <- cut(
  df_indv$age,
  breaks = c(15, 25, 35, 45, 55, 70, 101),
  right = FALSE,
  labels = c("15–24","25–34","35–44","45–54","55–69", "70+")
)
demographics <- c('Employment', 'gender', 'age_group','Marital_Status', 'ReligionRecorded',
                  'education')
tt <- df_indv %>% filter(COUNTRYNEW %in% "Argentina", YEAR_WAVE==2006)

basic_stat <- df_indv %>% group_by(COUNTRYNEW, YEAR_WAVE) %>% dplyr:: select(all_of(c(demographics, "YEAR_WAVE","COUNTRYNEW", "WGT"))) %>%  
  reshape2::melt(c("YEAR_WAVE","COUNTRYNEW", "WGT"))%>% group_by(variable,value, COUNTRYNEW, YEAR_WAVE)%>% summarise(n = n(), w = sum(WGT,  na.rm = TRUE))%>% 
  ungroup() %>% group_by(COUNTRYNEW, YEAR_WAVE, variable) %>% 
  mutate(freq = round(n/sum(n)*100), freq_w = round(w/sum(w)*100)) %>% filter(variable %in% "gender")


basic_stat_wide <- basic_stat %>% dplyr:: select(-n) %>% pivot_wider(names_from = c("YEAR_WAVE"), values_from = c("freq"))
my_order <- c(colnames(basic_stat_wide)[1:3], "2006", "2007", "2008", colnames(basic_stat_wide)[4:18])
basic_stat_wide <- basic_stat_wide[, my_order]
write.xlsx(basic_stat_wide, "Data/sample_demographics.xlsx")
basic_stat_wide <- basic_stat_wide[!is.na(basic_stat_wide$value), ]
a <-as.data.frame(t(apply(basic_stat_wide[, c(colnames(basic_stat_wide)[4:21])], 1, diff)))
basic_stat_wide$diff <-  rowMeans(a, na.rm = TRUE)
summary(basic_stat_wide$diff)