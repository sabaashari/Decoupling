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

# 1:Yes, 2: No, 3: DK,  4:Refused
# 1:Satisfied, 2:unsatisfied, 3: DK, 4: Refused
### Read data sample UK, USA, SA  ####
df <- read_csv("Data/G20_solidarity_agency.csv")
mapping <- read.xlsx("SAGE_code_book.xlsx")

## In some countries, some questions have not been asked, e.g. Saudi Arabia - WP103, WP105, WP106 (Russia as well) 
## WP110, WP108, WP109,  same in terms of NA values for G20 countries. 
solidarity <- c("WP27", "WP110", "WP108", "WP109", "WP103", "WP105", "WP106", "WP61", "WP129")
Agency <- c("WP138", "WP139", "WP146", "WP134", "WP93", "WP97", "WP98", "WP92", "WP91")
All_index <- Agency
result <- data.frame()
for (i in 1: length(All_index))
{
  index <- All_index[i]
  temp <- df %>% dplyr::select(COUNTRYNEW, YEAR_WAVE, !!sym(index)) %>% 
    group_by(COUNTRYNEW, YEAR_WAVE) %>% summarise(num_samples = n(), num_omitted = sum(is.na(!!sym(index))))
  #temp$index <- index
  colnames(temp)[3:4] <- c(paste("n_all_", index), paste("n_na_",index))
  if (i==1)
    result <- temp
  else 
    result <- merge(result, temp)
  #result <- rbind(result, temp)
}
result <- result %>% group_by(COUNTRYNEW) %>% mutate(sum_country = sum(`n_all_ WP138`))
#result <- merge(result, mapping, by.x = "index", by.y = "GWP.Indicator.Code")
write.xlsx(result, "output/omitted_variables.xlsx")

## country, variable, Year
## Question - To explore: why correlations change when I change the coding?
dd <- df %>% dplyr:: mutate(across(all_of(solidarity), 
               ~ case_when(
                 .%in% c("Yes", "Good place", "Satisfied")   ~ 1,
                 .%in% c("No", "Not a good place", "Dissatisfied")  ~ 2,
                 .%in% c("(DK)")  ~ 3,
                 .%in% c("(Refused)")  ~ 4,
                 TRUE ~ NA_integer_
               )))

a <- dd  %>% dplyr:: select(all_of(solidarity))
corr_matrix <- round(dd  %>% dplyr:: select(all_of(solidarity)) %>% 
                       cor(use="pairwise.complete.obs"),1)

p <- corr_matrix %>% ggcorrplot::ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, 
                                            lab_size=8, method = "square")  + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 24, face = "bold"),
        axis.text.y = element_text(size = 24, face = "bold"), axis.title = element_blank())
ggsave("output/solidairty_correlations.jpg", p)


dd <- df %>% dplyr:: mutate(across(all_of(c("WP138", "WP139", "WP134", 
                                            "WP93", "WP97", "WP98", "WP92", "WP91")), 
                                   ~ case_when(
                                     .%in% c("Yes", "Good place", "Satisfied")   ~ 1,
                                     .%in% c("No", "Not a good place", "Dissatisfied")  ~ 2,
                                     .%in% c("(DK)")  ~ 3,
                                     .%in% c("(Refused)")  ~ 4,
                                     TRUE ~ NA_integer_
                                   )))

corr_matrix <- round(dd  %>% dplyr:: select(all_of(Agency)) %>% 
                       cor(use="pairwise.complete.obs"),1)

p <- corr_matrix %>% ggcorrplot::ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, 
                                            lab_size=8, method = "square")  + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 24, face = "bold"),
        axis.text.y = element_text(size = 24, face = "bold"), axis.title = element_blank())
ggsave("output/agency_correlations.jpg", p)

