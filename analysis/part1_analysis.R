# STUDY 1 PRESCREEN 

# Install packages --------------------------------------------------------

library(tidyverse)
library(data.table)

# Load data --------------------------------------------------------------

prescreen_df <- read_csv("data/cleaned_part1.csv")

# EDA ---------------------------------------------------------------------

# Duration: ~5min
summary(prescreen_df$`Duration (in seconds)`)

# Gender
table(prescreen_df$Gender)

# FirstGen
table(prescreen_df$FirstGen)

#1st_immigrant
table(prescreen_df$`1st_immigrant`)

#STEMMajor
table(prescreen_df$STEMMajor)

# Distributions -----------------------------------------------------------

# Look at all AGQ averages
prescreen_AGQ_all <- prescreen_df %>%
  pivot_longer(cols = c(M_Approach, M_Avoidance, P_Approach, P_Avoidance),
               names_to = "AGQ_all", 
               values_to = "AGQ_values_all")

ggplot(prescreen_AGQ_all, aes(x = AGQ_values_all, color = AGQ_all, fill = AGQ_all)) +
  geom_density(alpha = 0.5) +
  scale_color_manual(values = c("green", "orange", "blue", "red")) +
  scale_fill_manual(values = c("green", "orange","blue", "red")) +
  theme_minimal() +
  labs(title = "Prescreen Goal Orientations",
       x = "Average",
       y = "Density",
       color = "AGQ",
       fill = "AGQ") 

# Only performance approach/avoidance

prescreen_AGQ <- prescreen_df %>%
  pivot_longer(cols = c(P_Approach, P_Avoidance),
               names_to = "AGQ", 
               values_to = "AGQ_values")

ggplot(prescreen_AGQ, aes(x = AGQ_values, color = AGQ, fill = AGQ)) +
  geom_density(alpha = 0.5) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal() +
  labs(title = "Prescreen Performance Goal Orientations",
       x = "Average",
       y = "Density",
       color = "AGQ",
       fill = "AGQ") 

# Racial Centrality

ggplot(prescreen_AGQ, aes(x = Racial_ID)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Prescreen Racial Centrality",
            x = "Average",
            y = "Density")


