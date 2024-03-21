# STUDY 1 PRESCREEN 

# Install packages --------------------------------------------------------

library(tidyverse)
library(data.table)

# Clean data --------------------------------------------------------------

raw_prescreen <- read_csv("raw_prescreen.csv")
#View(raw_prescreen)
#colnames(raw_prescreen)

filter_df <- raw_prescreen |> 
  select(23,5,24,26:30,32:66)

clean_prescreen <- 
        filter_df |> 
          # Recode columns
          mutate(Gender = 
                   recode_factor(Gender,
                                 "1" = "Woman",
                                 "2" = "Man",
                                 "3" = "Non-binary",
                                 "4" = "Other"),
                 FirstGen = 
                   recode_factor(FirstGen,
                                 "1" = "Yes",
                                 "2" = "No",
                                 #Not sure is coded as no
                                 "3" = "No"),
                 FirstGen_US = 
                   recode_factor(FirstGen_US,
                                 "1" = "Yes",
                                 "2" = "No",
                                 #Not sure is coded as no
                                 "3" = "No"),
                 `1st_immigrant` = 
                   recode_factor(`1st_immigrant`,
                                 "1" = "Yes",
                                 "2" = "No",
                                 #Not sure is coded as no
                                 "3" = "No"),
                 `2nd_immigrant` = 
                   recode_factor(`2nd_immigrant`,
                                 "1" = "Yes",
                                 "2" = "No",
                                 #Not sure is coded as no
                                 "3" = "No"),
                 HighestDegree = 
                   recode_factor(HighestDegree,
                                 "1" = "HS/GED",
                                 "2" = "Associate",
                                 "3" = "Bachelor",
                                 "4" = "Master",
                                 "5" = "Doctorate"),
                 STEMMajor = 
                   recode_factor(STEMMajor,
                                 "1" = "STEM",
                                 "2" = "Not STEM",
                                 "3" = "Not STEM")) |> 
             # Recode participant error
              mutate(`1st_immigrant` = replace(`1st_immigrant`, 34, "No"),
                     `1st_age` = replace(`1st_age`, 34, NA)) |> 
             # Recode scale error for Math/Reading items
              mutate_at(vars(Reading5, Math5, Reading6, Math6), 
                        ~recode(., 
                                "23" = "1",
                                "24" = "2",
                                "25" = "3",
                                "26" = "4",
                                "27" = "5")) |> 
             # Convert character to numeric
               mutate_at(c(2,7,13:43), as.numeric) 

# EDA ---------------------------------------------------------------------

prescreen_df <- clean_prescreen |> 
                    slice(-c(1:2))

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


# Baseline avgs ----------------------------------------------------------

prescreen_avg <- prescreen_df |> 
                    rowwise() |> 
                    mutate(M_Approach = round(mean(c_across(c(14,16,20)), na.rm = TRUE), 3),
                           M_Avoidance = round(mean(c_across(c(18,22,24)), na.rm = TRUE), 3),
                           P_Approach = round(mean(c_across(c(15,17,21)), na.rm = TRUE), 3),
                           P_Avoidance = round(mean(c_across(c(19,23,25)), na.rm = TRUE), 3),
                           Math_ID = round(mean(c_across(c(27,29,31,33,35,37)), na.rm = TRUE), 3),
                           Reading_ID = round(mean(c_across(c(26,28,30,32,34,26)), na.rm = TRUE), 3),
                           Racial_ID = round(mean(c_across(38:40), na.rm = TRUE), 3),
                           Gender_ID = round(mean(c_across(41:43), na.rm = TRUE), 3)
                           )

# Distributions -----------------------------------------------------------

# Look at all AGQ averages
prescreen_AGQ_all <- prescreen_avg %>%
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

prescreen_AGQ <- prescreen_avg %>%
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


