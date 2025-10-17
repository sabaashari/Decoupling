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
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(haven)
library(memisc)

sav_file <- spss.system.file("GWPall.sav")
solidarity <- c("WP27", "WP110", "WP108", "WP109", "WP103", "WP105", "WP106", "WP61", "WP129")
Agency <- c("WP138", "WP139", "WP146", "WP134", "WP93", "WP97", "WP98", "WP92", "WP91")
G20_countries <- c("Argentina", "Australia", "Brazil", "Canada", "China", "France", 
                   "Germany", "India", "Indonesia", "Italy", "Japan", "Mexico", "Russia", 
                   "Saudi Arabia", "South Africa", "South Korea", "Turkey", 
                   "United Kingdom", "United States")

subset_agency <- sav_file[c("COUNTRYNEW", "YEAR_WAVE", Agency)]
data_agency <- as.data.set(subset_agency) 
df_agency <- data.frame(data_agency)
dd_agency <- df_agency %>% filter(COUNTRYNEW %in% G20_countries)

subset_solidarity <- sav_file[c("COUNTRYNEW", "YEAR_WAVE", solidarity)]
data_solidarity <- as.data.set(subset_solidarity) 
df_solidarity <- data.frame(data_solidarity)
dd_solidarity <- df_solidarity %>% filter(COUNTRYNEW %in% G20_countries)
df <- cbind(dd_agency, dd_solidarity %>% dplyr::select(-c(COUNTRYNEW, YEAR_WAVE)))
write.csv(df, "Data/G20_solidarity_agency.csv")

