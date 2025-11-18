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

## To do: add regressions and T statistics for growth ####

# Read clean data ####
avg_country_year <- read.xlsx("../Data/average_country_year.xlsx")
avg_country_year <- avg_country_year %>% group_by(COUNTRYNEW) %>%
  mutate(income_group = names(which.max(table(World.Bank.s.income.classification))))
avg_country_year$income_group <- gsub("countries", "", avg_country_year$income_group)

avg_country_year <- avg_country_year %>% group_by(COUNTRYNEW) %>% 
  mutate(growth_solidarity = ((mean_solidarity_1 - lag(mean_solidarity_1)) / lag(mean_solidarity_1)), 
         growth_agency = ((agency_score_1 - lag(agency_score_1)) / lag(agency_score_1)))


# Per country/year plots ####
GDP_solidarity <- avg_country_year %>%
  select(COUNTRYNEW, YEAR_WAVE, solidarity_z_1, GDP_z) %>% 
  pivot_longer(cols = c("GDP_z", "solidarity_z_1"), 
               names_to = "score", values_to = "value")

GDP_agency <- avg_country_year %>% 
  select(COUNTRYNEW, YEAR_WAVE,agency_z_1, GDP_z) %>% 
  pivot_longer(cols = c("agency_z_1", "GDP_z"), 
               names_to = "score", values_to = "value")

ggplot(GDP_solidarity, aes(x = YEAR_WAVE, y = value, color = score)) +
  geom_line(size = 1.2) + facet_wrap(~COUNTRYNEW) + 
  scale_x_continuous(
    breaks = seq(2006, 
                 2025, 
                 by = 5)) +
  theme_minimal() + labs(title = "Solidarity and GDP for G20 countries between 2006-2023") +
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"), 
    axis.title = element_blank(), 
    legend.position = "top",
    legend.title = element_blank(), 
    legend.text = element_text(size = 12, face = "bold")
  )
ggsave("output/Solidarity_GDP.jpg",  width = 10, height = 7)

ggplot(GDP_agency, aes(x = YEAR_WAVE, y = value, color = score)) +
  geom_line(size = 1.2) + facet_wrap(~COUNTRYNEW) + 
  scale_x_continuous(
    breaks = seq(2006, 
                 2025, 
                 by = 5)) +
  theme_minimal() + labs(title = "Agency and GDP for G20 countries between 2006-2023") +
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.text = element_text(size = 11, color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"), 
    axis.title = element_blank(), 
    legend.position = "top",
    legend.title = element_blank(), 
    legend.text = element_text(size = 12, face = "bold")
  )
ggsave("output/Agency_GDP.jpg",  width = 10, height = 7)


## per country/year time series ####
last_points <- avg_country_year %>%
  group_by(COUNTRYNEW) %>%
  filter(YEAR_WAVE == max(YEAR_WAVE))

## Solidarity Time series, Developed vs Developing, income groups
ggplot(avg_country_year, aes(x = YEAR_WAVE, y = mean_solidarity_1, color = COUNTRYNEW)) +
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
    title = "Solidarity score from 2006-2023 for G20 countries, Developing vs Developed countries",
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
ggsave("Output/solidarity_time_series.jpg",  width = 12, height = 7)

ggplot(avg_country_year, aes(x = YEAR_WAVE, y = mean_solidarity_1, color = COUNTRYNEW)) +
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
ggsave("Output/solidarity_time_series_income.jpg",  width = 12, height = 7)


## Agency time series, Developed vs Developing
ggplot(avg_country_year, aes(x = YEAR_WAVE, y = agency_score_1, color = COUNTRYNEW)) +
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
    title = "Agency score from 2006-2023 for G20 countries, Developing vs Developed countries",
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
ggsave("Output/Agency_time_series.jpg",  width = 12, height = 7)

## Agency time series, Income groups
ggplot(avg_country_year, aes(x = YEAR_WAVE, y = agency_score_1, color = COUNTRYNEW)) +
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
ggsave("Output/Agency_time_series_income.jpg",  width = 12, height = 7)






#### Average Per country index calculation ####
country_stat <- avg_country_year %>% ungroup() %>% 
  group_by(COUNTRYNEW) %>% summarise(type = first(Type), 
                      country_code = first(Country.Code),
                        mean_solidarity = mean(mean_solidarity_1), 
                       sd_solidarity = sd(mean_solidarity_1), 
                       mean_agency = mean(agency_score_1, na.rm = TRUE), 
                       sd_agency = sd(agency_score_1, na.rm = TRUE), 
                       cv_solidarity = abs(sd(mean_solidarity_1)/mean_solidarity ),
                       cv_agency = abs(sd_agency/mean_agency), 
                       cor_sol_1 = cor(mean_solidarity_1, GDP, use = "complete.obs"), 
                       cor_agency_1 = cor(agency_score_1, GDP, use = "complete.obs"), 
                      cor_sol_growth = cor(growth_solidarity, growth,  use = "complete.obs"),
                      cor_agency_growth = cor(growth_agency, growth,  use = "complete.obs"),
                       D_S = 1- cor_sol_1, 
                       D_A = 1 - cor_agency_1, 
                       D = 1- (cor_agency_1 + cor_sol_1)/2, 
                       GDP = mean(GDP), 
                      D_S_growth = 1 - cor_sol_growth, 
                      D_A_growth = 1 - cor_agency_growth,
                      D_growth = (D_S_growth + D_A_growth)/2,
                      
                      income_group =  names(which.max(table(World.Bank.s.income.classification))) 
                      ) 
country_stat$income_group <- gsub("countries", "", country_stat$income_group)

G20_countries <- c("Argentina", "Australia", "Brazil", "Canada", "China", "France", 
                   "Germany", "India", "Indonesia", "Italy", "Japan", "Mexico", "Russia", 
                   "Saudi Arabia", "South Africa", "South Korea", "Turkey", 
                   "United Kingdom", "United States")
for (country in G20_countries){
  dd <- avg_country_year %>% filter(COUNTRYNEW %in% country)
  model <- lm(GDP ~ mean_solidarity_1+agency_score_1, data = dd)
  
  country_stat [country_stat$COUNTRYNEW %in% country, "adjusted_R2"] <- 
    summary(model)$adj.r.squared
  
  country_stat [country_stat$COUNTRYNEW %in% country, "R2"] <- 
    summary(model)$r.squared
  
}
country_stat$R2_decoupling <- 1- country_stat$R2
country_stat$adjusted_R2_decoupling <- 1- country_stat$adjusted_R2
write.xlsx(country_stat,"../Data/country_statistics.xlsx")

## Average per country bar charts ####
p1 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW, D), y = D, fill = type)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = round(D,2)),
            position = position_dodge(width = 0.7), vjust = 0.5, 
            size = 5, color = "black", fontface = "bold") +  # White text with bold font
  scale_fill_brewer(palette = "Set2") + 
  labs(
    title = str_wrap("D for G20 countries 2006-2023", width = 50),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 14, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", color = "gray40"),  # Subtitle style
    legend.title =  element_blank(),
    axis.text = element_text(size = 14, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16), 
    legend.position = "top")

p2 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW,adjusted_R2_decoupling), y = adjusted_R2_decoupling, fill = type)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = round(adjusted_R2_decoupling,2)),
            position = position_dodge(width = 0.7), vjust = 0.5, 
            size = 5, color = "black", fontface = "bold") +  # White text with bold font
  scale_fill_brewer(palette = "Set2") + 
  labs(
    title = str_wrap("Adjusted R2 Decoupling for G20 countries 2006-2023", width = 50),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 14, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", color = "gray40"),  # Subtitle style
    legend.title =  element_blank(),
    axis.text = element_text(size = 14, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16), 
    legend.position = "top")

a <- grid.arrange(p1,p2, ncol = 2)
ggsave("../Output/R2_D_barchart.jpg",  width = 15, height = 7, plot = a)

p1 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW, D_S), y = D_S, fill = type)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = round(D_S,2)),
            position = position_dodge(width = 0.7), vjust = 0.5, 
            size = 5, color = "black", fontface = "bold") +  # White text with bold font
  scale_fill_brewer(palette = "Set2") + 
  # Labels & Titles
  labs(
    title = str_wrap("D_S for G20 countries 2006-2023", width = 50),  # Wrapping title for better display
    y = str_wrap("", width = 50)) +
  theme_minimal() +  coord_flip()+ # Cleaner background with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 14, face = "bold", color = "darkblue"),  # Emphasize title
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", color = "gray40"),  # Subtitle style
    legend.title =  element_blank(),
    axis.text = element_text(size = 14, face = "bold", angle = 20),  # Subtitle style
    axis.title = element_blank(),
    legend.text = element_text(size = 16), 
    legend.position = "top")

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
ggsave("Output/D_S_A_barchart.jpg",  width = 12, height = 7, plot = a)

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

p3 <- ggplot(country_stat, aes(x = reorder(COUNTRYNEW, D_growth), y = D_growth, fill = type)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
  geom_text(aes(label = round(D_growth,2)),
            position = position_dodge(width = 0.7), vjust = 0.5, 
            size = 5, color = "black", fontface = "bold") + 
  scale_fill_brewer(palette = "Set2") + 
  # Labels & Titles
  labs(
    title = str_wrap("D_growth for G20 countries 2006-2023", width = 50),  # Wrapping title for better display
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
ggsave("../output/D_growth.jpg", plot = p3, width = 10, height = 5)

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
      legend.text = element_text(size = 12, face = "bold"))
ggsave("output/boxplot1.jpg", width = 12,height = 7)

ggplot(country_stat %>% pivot_longer(
  cols = c("D", "D_S", "D", "D_A"), 
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
      legend.text = element_text(size = 12, face = "bold"))
ggsave("output/boxplot2.jpg", width = 12,height = 7)


ggplot(country_stat %>% pivot_longer(
  cols = c("mean_solidarity", "mean_agency"), 
  names_to = "Score",
  values_to = "Value"), aes(x = income_group, y = Value, fill = income_group)) +
  geom_boxplot() + facet_wrap(~Score) + 
  labs(
    title = "Box plots of Scores for different income groups",
    x = "",
    y = "Score") + theme(
      strip.text = element_text(size = 15, face = 'bold'),
      axis.text = element_text(size = 12, color = "black", angle = 20),
      plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"), 
      legend.position = "top",
      legend.title = element_blank(), 
      legend.text = element_text(size = 12, face = "bold"))
ggsave("output/boxplot3.jpg", plot = p1, width = 12,height = 7)
ggplot(country_stat %>% pivot_longer(
  cols = c("D", "D_S", "D", "D_A"), 
  names_to = "Score",
  values_to = "Value"), aes(x = income_group, y = Value, fill = income_group)) +
  geom_boxplot() + facet_wrap(~Score) + 
  labs(
    title = "Box plots of Scores for different income groups",
    x = "",
    y = "Score") + theme(
      strip.text = element_text(size = 15, face = 'bold'),
      axis.text = element_text(size = 12, color = "black", angle = 20),
      plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"), 
      legend.position = "top",
      legend.title = element_blank(), 
      legend.text = element_text(size = 12, face = "bold"))
ggsave("output/boxplot4.jpg", plot = p2, width = 12,height = 7)









##  T-tests for developing vs developed countries and income groups ####
shapiro.test(country_stat$mean_agency) #check if solidarity and agency are normally distributed
shapiro.test(country_stat$mean_solidarity)
result <- data.frame()
scores <- c('mean_solidarity', 'mean_agency', 'D_S', 'D_A', 'D', 'GDP', 
            'D_S_growth', 'D_A_growth', 'D_growth')
for (score in scores)
{
  t1 <- t.test(country_stat[country_stat$type=="Developed", score],
               country_stat[country_stat$type=="Developing", score])
  
  t2 <- t.test(country_stat[country_stat$income_group %in% "High-income ", score],
               country_stat[country_stat$income_group=="Upper-middle-income ", score])
  
  estimates_1 <- unname(t1$estimate)
  estimates_2 <- unname(t2$estimate)
  result <- rbind(result, data.frame(metric = score, p_value_type = t1$p.value, 
                                     mean_developed = estimates_1[1], 
                                     mean_developing = estimates_1[2], 
                                     p_value_income = t2$p.value, 
                                     mean_high_income = estimates_2[1], 
                                     mean_low_income = estimates_2[2]))
}

scores <- c('mean_solidarity_1', 'agency_score_1')
for (score in scores)
{
  t1 <- t.test(avg_country_year[avg_country_year$Type=="Developed", score],
               avg_country_year[avg_country_year$Type=="Developing", score])
  
  t2 <- t.test(avg_country_year[avg_country_year$income_group %in% "High-income ", score],
               avg_country_year[avg_country_year$income_group=="Upper-middle-income ", score])
  
  estimates_1 <- unname(t1$estimate)
  estimates_2 <- unname(t2$estimate)
  result <- rbind(result, data.frame(metric = score, p_value_type = t1$p.value, 
                                     mean_developed = estimates_1[1], 
                                     mean_developing = estimates_1[2], 
                                     p_value_income = t2$p.value, 
                                     mean_high_income = estimates_2[1], 
                                     mean_low_income = estimates_2[2]))
}

result[colnames(result)[-1]] <- round(result[colnames(result)[-1]], 4)

result <- result %>% dplyr:: mutate(across(all_of(c("p_value_income", "p_value_type")), 
                                           ~ case_when(
                                             . < 0.01 ~ paste(p_value_income,'**'),
                                             . < 0.05 ~ paste(p_value_income,'*'),
                                             . < 0.1 ~ paste(p_value_income,'~') ,
                                             TRUE ~ as.character(p_value_income))))

result <- result %>% mutate(
  p_value_income = case_when(
    p_value_income < 0.01 ~ paste(p_value_income,'**'), 
    p_value_income < 0.05 ~ paste(p_value_income,'*'),
    p_value_income < 0.1 ~ paste(p_value_income,'~'),
    TRUE ~ as.character(p_value_income)))

result <- result %>% mutate(
  p_value_type = case_when(
    p_value_type < 0.01 ~ paste(p_value_type,'**'), 
    p_value_type < 0.05 ~ paste(p_value_type,'*'),
    p_value_type < 0.1 ~ paste(p_value_type,'~'),
    TRUE ~ as.character(p_value_type)))

write.xlsx(result, "../output/developing_developed_stats.xlsx")


## Across countries indices #### 
across_countries <- avg_country_year %>% ungroup() %>% group_by(YEAR_WAVE) %>% 
  summarise(cor_sol_1 = cor(mean_solidarity_1, GDP, use = "complete.obs"),
            cor_sol_2 = cor(mean_solidarity_2, GDP, use = "complete.obs"),
            cor_sol_3 = cor(mean_solidarity_3, GDP, use = "complete.obs"),
            cor_agency_1 = cor(agency_score_1, GDP, use = "complete.obs"), 
            cor_agency_2 = cor(agency_score_2, GDP, use = "complete.obs"),
            cor_agency_3 = cor(agency_score_3, GDP, use = "complete.obs"),
            cor_sol_growth = cor(growth_solidarity, growth, use = "pairwise.complete.obs"), 
            cor_agency_growth = cor(growth_agency, growth, use = "pairwise.complete.obs"), 
            D_S = 1- cor_sol_1, 
            D_A = 1- cor_agency_1, 
            D = (D_S + D_A)/2, 
            D_S_growth = 1 - cor_sol_growth, 
            D_A_growth = 1 - cor_agency_growth,
            D_growth = (D_S_growth + D_A_growth)/2)

for(i in 2006:2023)
{
  dd <- avg_country_year %>% filter(YEAR_WAVE==i)
  model <- lm(GDP ~ mean_solidarity_1+agency_score_1, data = dd)
  print(summary(model))
  across_countries[across_countries$YEAR_WAVE==i, "D_R2"] <- 1- summary(model)$adj.r.squared
}
write.xlsx(across_countries, "../Data/across_countries_decoupling.xlsx")


across_countries_type <- avg_country_year %>% ungroup() %>% group_by(YEAR_WAVE, Type) %>% 
  summarise(cor_sol_1 = cor(mean_solidarity_1, GDP, use = "complete.obs"),
            cor_sol_2 = cor(mean_solidarity_2, GDP, use = "complete.obs"),
            cor_sol_3 = cor(mean_solidarity_3, GDP, use = "complete.obs"),
            cor_agency_1 = cor(agency_score_1, GDP, use = "complete.obs"), 
            cor_agency_2 = cor(agency_score_2, GDP, use = "complete.obs"),
            cor_agency_3 = cor(agency_score_3, GDP, use = "complete.obs"), 
            cor_sol_growth = cor(growth_solidarity, growth, use = "pairwise.complete.obs"), 
            cor_agency_growth = cor(growth_agency, growth, use = "pairwise.complete.obs"), 
            D_S = 1- cor_sol_1, 
            D_A = 1- cor_agency_1, 
            D = (D_S + D_A)/2, 
            D_S_growth = 1 - cor_sol_growth, 
            D_A_growth = 1 - cor_agency_growth,
            D_growth = (D_S_growth + D_A_growth)/2)

for(i in 2006:2023){
  
  dd1 <- avg_country_year %>% filter(YEAR_WAVE==i, Type %in% 'Developed')
  model1 <- lm(GDP ~ mean_solidarity_1+agency_score_1, data = dd1)
  across_countries_type[across_countries_type$YEAR_WAVE==i &
                          across_countries_type$Type  %in% 'Developed'
                        , "R2"] <- 1- summary(model1)$adj.r.squared
  
  dd2 <- avg_country_year %>% filter(YEAR_WAVE==i, Type %in% 'Developing')
  model2 <- lm(GDP ~ mean_solidarity_1+agency_score_1, data = dd2)
  across_countries_type[across_countries_type$YEAR_WAVE==i &
                          across_countries_type$Type  %in% 'Developing'
                        , "R2"] <- 1- summary(model2)$adj.r.squared
  
}

across_countries_income <- avg_country_year %>% ungroup() %>% group_by(YEAR_WAVE, income_group) %>% 
  summarise(cor_sol_1 = cor(mean_solidarity_1, GDP, use = "complete.obs"),
            cor_sol_2 = cor(mean_solidarity_2, GDP, use = "complete.obs"),
            cor_sol_3 = cor(mean_solidarity_3, GDP, use = "complete.obs"),
            cor_agency_1 = cor(agency_score_1, GDP, use = "complete.obs"), 
            cor_agency_2 = cor(agency_score_2, GDP, use = "complete.obs"),
            cor_agency_3 = cor(agency_score_3, GDP, use = "complete.obs"), 
            D_S = 1- cor_sol_1, 
            D_A = 1- cor_agency_1, 
            D = (D_S + D_A)/2, 
            cor_sol_growth = cor(growth_solidarity, growth, use = "pairwise.complete.obs"), 
            cor_agency_growth = cor(growth_agency, growth, use = "pairwise.complete.obs"), 
            D_S_growth = 1 - cor_sol_growth, 
            D_A_growth = 1 - cor_agency_growth,
            D_growth = (D_S_growth + D_A_growth)/2)

### Time series for Decoupling across counties ####
corr_long <- across_countries %>% pivot_longer(cols = c("R2", "D", "D_growth"), 
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
    hjust = 0.2,
    vjust = 1,
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

ggsave("../Output/Decoupling_R2_D_time_all_countries.jpg",  width = 8, height = 4)

corr_long <- across_countries %>% pivot_longer(cols = c("D_S_growth", "D_A_growth"), 
                                               names_to = "variable", values_to = "value")
last_points <- corr_long %>%
  group_by(variable) %>%
  filter(YEAR_WAVE == max(YEAR_WAVE)) %>% ungroup()
p1 <- ggplot(corr_long, aes(x = YEAR_WAVE, y = value, color = variable)) +
  geom_line(linewidth = 0.8) +
  theme_minimal() +   
  geom_smooth(method = "lm", se = TRUE, aes(fill = variable), alpha = 0.2, show.legend = TRUE) +
  labs(
    title = str_wrap("Decoupling growth indices from 2006-2023 across G20 countries", width = 30),
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

corr_long <- across_countries %>% pivot_longer(cols = c("D_S", "D_A", "D"), 
                                               names_to = "variable", values_to = "value")
last_points <- corr_long %>%
  group_by(variable) %>%
  filter(YEAR_WAVE == max(YEAR_WAVE)) %>% ungroup()
p2 <- ggplot(corr_long, aes(x = YEAR_WAVE, y = value, color = variable)) +
  geom_line(linewidth = 0.8) +
  theme_minimal() +   
  geom_smooth(method = "lm", se = TRUE, aes(fill = variable), alpha = 0.2, show.legend = TRUE) +
  labs(
    title = str_wrap("Decoupling indices from 2006-2023 across G20 countries", width = 30),
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
a <- grid.arrange(p2, p1,ncol =2)
ggsave("../Output/D_S_A_all.jpg",  width = 10, height = 4, plot = a)



### D_S and D_A across counties for Developed vs Developing ####
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

ggsave("output/D_S_A_developed_developing.jpg", plot = a, width = 8, height = 4)

### D and 1-R2 across counties for Developed vs Developing ####
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

p4 <- ggplot(across_countries_type, aes(x = YEAR_WAVE, y = R2, color = Type)) + 
  geom_line(linewidth = 0.6) +
  theme_minimal() +   
  geom_smooth(method = "lm", se = TRUE, aes(fill = Type), alpha = 0.2, show.legend = TRUE) +
  labs(
    title = str_wrap("Decoupling index (1-R2) from 2006-2023 across G20 countries", width = 30),
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


p1 <- ggplot(across_countries_type, aes(x = YEAR_WAVE, y = D_A_growth, color = Type)) + 
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

p2 <- ggplot(across_countries_type, aes(x = YEAR_WAVE, y = D_S_growth, color = Type)) + 
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

ggsave("output/D_S_A_developed_developing.jpg", plot = a, width = 8, height = 4)


### D_S_growth and D_A_growth across counties for Developed vs Developing ####
p1 <- ggplot(across_countries_type, aes(x = YEAR_WAVE, y = D_growth, color = Type)) + 
  geom_line(linewidth = 0.6) +
  theme_minimal() +   
  geom_smooth(method = "lm", se = TRUE, aes(fill = Type), alpha = 0.2, show.legend = TRUE) +
  labs(
    title = str_wrap("Decoupling growth from 2006-2023 across G20 countries", width = 30),
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

ggsave("../output/decoulpling_growth_type.jpg", plot = p1, width = 8, height = 4)


### Time series for Decoupling across counties for Income groups ####
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

a <- grid.arrange(p1, p2, ncol=2)
ggsave("output/D_S_A_income_groups.jpg", plot = a, width = 8, height = 4)
ggsave("output/Decoupling_income_groups.jpg", plot = p3, width = 8, height = 4)


p1 <- ggplot(across_countries_income, aes(x = YEAR_WAVE, y = D_growth, color = str_wrap(income_group, width = 20))) + 
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

ggsave("../output/decoulpling_growth_income.jpg", plot = p1, width = 8, height = 4)

# ggplot(avg_country_year, aes(x = GDP, y = mean_solidarity_1)) +
#   geom_point(color = "blue", size =1) + facet_wrap(~YEAR_WAVE) +  
#   geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "solid") +
#   theme_minimal() + labs(title = "Correlation between GDP and Solidarity for each year across G20 countries") +
#   geom_text(aes(label = Country.Code), vjust = -1, size = 2) + 
#   theme(
#     strip.text = element_text(size = 15, face = 'bold'),
#     axis.text = element_text(size = 11, color = "black"),
#     plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"), 
#     legend.text = element_text(size = 12, face = "bold")
#   )
# ggsave("Output/Solidarity_GDP_countries.jpg",  width = 10, height = 7)
# 
# 
# ggplot(avg_country_year, aes(x = GDP, y = agency_score_1)) +
#   geom_point(color = "blue", size =1) + facet_wrap(~YEAR_WAVE) +  
#   geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "solid") +
#   theme_minimal() + labs(title = "Correlation between GDP and Agency for each year across G20 countries") +
#   geom_text(aes(label = Country.Code), vjust = -1, size = 2) + 
#   theme(
#     strip.text = element_text(size = 15, face = 'bold'),
#     axis.text = element_text(size = 11, color = "black"),
#     plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"), 
#     legend.text = element_text(size = 12, face = "bold")
#   )
# ggsave("Output/Agency_GDP_countries.jpg",  width = 10, height = 7)



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


ggsave("Output/Decoupling_time_all_countries.jpg",  width = 8, height = 4)







