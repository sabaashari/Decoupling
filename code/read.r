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
library(haven)
library(memisc)
library(Hmisc)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


info <- spss.get("../Data/GWPall.sav", use.value.labels = FALSE, to.data.frame = FALSE)

### Read the file ####
sav_file <- spss.system.file("../Data/GWPall.sav")

### Demographic variable selections ####
variables <- data.frame(description(sav_file))
demographic_keywords <- c('Age', 'Gender', 'education', 'income', 'household', 'employment', 'job', 
                          'occupation', 'region', 'residence', 'religion', 'ethnicity')

variables$education <- sapply(variables$description, function(x) grepl("Education", x, ignore.case = TRUE))
variables$religion <- sapply(variables$description, function(x) grepl("Religion", x, ignore.case = TRUE))
variables$gender <- sapply(variables$description, function(x) grepl("Gender", x, ignore.case = TRUE))
variables$marital <- sapply(variables$description, function(x) grepl("Marital", x, ignore.case = TRUE))
variables$job <- sapply(variables$description, function(x) grepl("Job", x, ignore.case = TRUE))
variables$occupation <- sapply(variables$description, function(x) grepl("occupation", x, ignore.case = TRUE))
variables$household <- sapply(variables$description, function(x) grepl("household", x, ignore.case = TRUE))
variables$employment <- sapply(variables$description, function(x) grepl("employment", x, ignore.case = TRUE))
variables$age <- sapply(variables$description, function(x) grepl("Age", x, ignore.case = TRUE))
variables$income <- sapply(variables$description, function(x) grepl("Income", x, ignore.case = TRUE))
write.xlsx(variables, 'Data/variables.xlsx')

### variables selected ####
EMP_2010 <- 'Employment Status'
INCOME_1 <- 'Annual Household Income in Local Currency'
WP1219 <- 'Gender'
WP1220 <- 'Age'
WP1223 <- 'Marital Status'
WP1233 <- 'Religion'
WP1233RECODED <- 'Religion'
WP22463 <- 'Employment Status'
WP3117 <- 'Education Level'
WP9081 <- 'Employment (2009)'


#####
solidarity <- c("WP27", "WP110", "WP108", "WP109", "WP103", "WP105", "WP106", "WP61", "WP129")
Agency <- c("WP138", "WP139", "WP146", "WP134", "WP93", "WP97", "WP98", "WP92", "WP91")
G20_countries <- c("Argentina", "Australia", "Brazil", "Canada", "China", "France", 
                   "Germany", "India", "Indonesia", "Italy", "Japan", "Mexico", "Russia", 
                   "Saudi Arabia", "South Africa", "South Korea", "Turkey", 
                   "United Kingdom", "United States")
demographics <- c('WP1223', 'WP1233', 'WP1233RECODED', 'WP1219', 'WP3117', 'EMP_2010', 
                  'WP22463', 'WP9081', 'WP1220', 'INCOME_1')
subset <- sav_file[c("COUNTRYNEW", "YEAR_WAVE", "WGT", Agency, solidarity,demographics )]
data <- as.data.set(subset)
df <- data.frame(subset)

dd <- df %>% filter(COUNTRYNEW %in% G20_countries)
colnames(dd)[names(dd) %in% demographics] <- 
  c('Employment', 'income_household', 'gender', 'age','Marital_Status', 'Religion', 'ReligionRecorded',
    'Employment_Status','education', 'Employment_2009')

write.csv(df, "Data/all_solidarity_agency_demographics.csv")
write.csv(dd, "Data/G20_solidarity_agency_demographics.csv")


#subset_solidarity <- sav_file[c("COUNTRYNEW", "YEAR_WAVE", solidarity)]
#data_solidarity <- as.data.set(subset_solidarity) 
#df_solidarity <- data.frame(data_solidarity)
#dd_solidarity <- df_solidarity %>% filter(COUNTRYNEW %in% G20_countries)
#df <- cbind(dd_agency, dd_solidarity %>% dplyr::select(-c(COUNTRYNEW, YEAR_WAVE)))
#write.csv(df, "Data/G20_solidarity_agency.csv")

