# STUDY 1 (Part 2) 

# Install packages --------------------------------------------------------

library(tidyverse)
library(data.table)

# Load data ---------------------------------------------------------------

prescreen_df <- read_csv("data/cleaned_prescreen.csv")
all_data <- read_csv("data/data_0326.csv")

# Extract IDs from either column
all_data <- all_data |> 
  mutate(ProlificID = coalesce(ProlificID, PROLIFIC_PID))

# Filter data -------------------------------------------------------------

# Filter out incomplete surveys + those that don't want their data included
filter_data <- all_data |> 
        filter(Finished == "1" & 
                 # Include data 
                 debrief_include_data == "1") 

print(paste("Number of participants BEFORE MC:", nrow(filter_data)))

# Manipulation check
data <- filter_data |>  
  filter(!((mc_task != "1" &
              mc_mathability != "1" &
              mc_diagnostic != "1")))   

print(paste("Number of participants AFTER MC:", nrow(data)))
# n = 210

# Make sure same people are doing both parts
part1_participants <- prescreen_df$ProlificID
part2_participants <- data$ProlificID 
all(part2_participants[-c(1:2)] %in% part1_participants)

# Clean data -----------------------------------------

  # Turn empty strings to na's
  data <- data |> mutate_all(~na_if(., ""))
  
  # Add a column for which condition participant was assigned to
  data <- data |>
    mutate(Condition = ifelse(!is.na(Control_common_1), "Control",
                              ifelse(!is.na(Q1), "Implicit",
                                     "Explicit")))
  
  # Calculate task performance
  correct_answers <- c(
    '6_150_R' = 2,
    '7_150_R' = 2,
    '8_150_R' = 2,
    '9_150' = 1,
    '10_150_R' = 2,
    '11_150' = 1,
    '12_150' = 1,
    '13_150' = 1,
    '14_150_R' = 2,
    '15_150' = 1,
    '16_150_R' = 2,
    '17_150_R' = 2,
    '18_150_R' = 2,
    '19_150' = 1,
    '20_150_R' = 2
  )
  
  # Count the number of matches for each row
  data$Score <- rowSums(sapply(names(correct_answers), 
                               function(col) data[[col]] == correct_answers[col] & 
                                 !is.na(data[[col]])))
  
  # Reorder the "Condition" factor variable
  order <- c("Control", "Implicit", "Explicit")
  data$Condition <- factor(data$Condition, levels = order)
  
  # Recode factor variables
  data <- data |>
    mutate(Comparison = 
             recode_factor(Comparison,
                           "1" = "10th percentile",
                           "2" = "25th percentile",
                           "3" = "50th percentile",
                           "4" = "75th percentile",
                           "5" = "90th percentile")
    )

  
  clean_data <- data |> 
    # Manually fix errors
    mutate(`post-expectation` = case_when(
      `post-expectation` == "4-5" ~ "5",
      `post-expectation` == "At least 10" ~ "10",
      TRUE ~ `post-expectation`)) |>
    # Convert columns to numeric
    mutate_at(vars(`Duration (in seconds)`,
                   Score, 
                   `pre-expectation`, 
                   `post-expectation`,
                   Q6, Age), as.numeric) |> 
    # Only grab necessary columns
    dplyr::select(Condition, Score, Age, `pre-expectation`, `post-expectation`, Comparison,
           Control_common_1, Control_common_2, Control_common_3, Control_common_4, Control_common_5,
           Control_true_1, Control_true_2, Control_true_3, Control_true_4, Control_true_5,
           Q1_1_TEXT, `Q2...35`, Q3_1_TEXT, Q4_1_TEXT, Q5, Q6, 
           Blatant_common_1, Blatant_common_2, Blatant_common_3, Blatant_common_4, Blatant_common_5, Blatant_common_6, 
           Blatant_true_1, Blatant_true_2, Blatant_true_3, Blatant_true_4, Blatant_true_5, Blatant_true_6, 
           `STAI_1...54`, `STAI_2...55`, `STAI_3...56`, `STAI_4...57`, `STAI_5...58`, `STAI_6...59`, 
           Approach2, Approach1, Avoidance3, Approach3, Avoidance2, Avoidance1,
           mc_task, mc_mathability, mc_diagnostic, 
           `STAI_1...170`, `STAI_2...171`, `STAI_3...172`, `STAI_4...173`, `STAI_5...174`, `STAI_6...175`, 
           task_stressful, Pressure, task_opinion, Math_abilities1, Math_abilities2, 
           RIT1, RIT2, RIT3, RIT4, RIT5, RIT6, 
           B1, B2, B3_Reverse, 
           Centrality1, Centrality2, Centrality3, 
           `Duration (in seconds)`, Feedback, Purpose, ProlificID) |> 
    #Rename
    rename(FamilyLanguage = Q1_1_TEXT,
           OtherLanguage = `Q2...35`,
           LanguageatHome = Q3_1_TEXT,
           LanguageonCampus = Q4_1_TEXT,
           StudentOrg = Q5,
           NumGenerations = Q6,
           Pre_Expectation = `pre-expectation`,
           Post_Expectation = `post-expectation`)
  
  #Average DV's
  avg_data <- clean_data |>
    # convert to numeric
    mutate(across(c(7:16, 21:72), as.numeric)) |>   
    rowwise() |>  
    # calculate row means and round to 3 decimal places
    mutate(Control_common = round(mean(c_across(7:11), na.rm = TRUE), 3),
           Control_true = round(mean(c_across(12:16), na.rm = TRUE), 3),
           Blatant_common = round(mean(c_across(23:28), na.rm = TRUE), 3),
           Blatant_true = round(mean(c_across(29:34), na.rm = TRUE), 3),
           STAI_pre = round(mean(c_across(35:40), na.rm = TRUE), 3),
           AGQ_approach = round(mean(c_across(c(41,42,44)), na.rm = TRUE), 3),
           AGQ_avoidance = round(mean(c_across(c(43,45,46)), na.rm = TRUE), 3),
           STAI_post = round(mean(c_across(50:55), na.rm = TRUE), 3),
           RIT = round(mean(c_across(61:66), na.rm = TRUE), 3),
           Belonging = round(mean(c_across(67:69), na.rm = TRUE), 3),
           Centrality = round(mean(c_across(70:72), na.rm = TRUE), 3)
    ) |>
    dplyr::select(Condition, Score, AGQ_approach, AGQ_avoidance,Pre_Expectation,
                  Post_Expectation, RIT, Belonging, Centrality, STAI_pre, STAI_post,
                  ProlificID)
    
  
  # Bind prescreen data 
  
  prescreen_df <- prescreen_df |> 
                    #Selecting necessary columns
                    dplyr::select(ProlificID, Gender, M_Approach, M_Avoidance,P_Approach, P_Avoidance,
                                  Reading_ID, Racial_ID, Gender_ID,
                                  FirstGen, FirstGen_US, `1st_immigrant`,
                                  `1st_age`, `2nd_immigrant`, HighestDegree, Major, STEMMajor, GPA)
  
  full_data <- merge(avg_data, prescreen_df, by = "ProlificID", all = FALSE)
              

# EDA ---------------------------------------------------------------------

# Conditions
table(full_data$Condition)
  
# Score distribution
  ggplot(data = full_data, aes(x = Score)) +
    geom_histogram() +
    labs(title = "Distribution of Performance") +
    theme_bw() +
    # Add vertical line for mean
    geom_vline(aes(xintercept = mean(Score)), color = "red", linetype = "dashed", size = 1) +
    # Annotate with numeric mean value
    annotate("text", x = mean(full_data$Score), y = 25, 
             label = paste("Mean =", round(mean(full_data$Score), 2)), vjust = 1.5, color = "red")
  
# Analysis ------------------------------------------------------------------

# Primary ANOVA (condition x score)
summary(aov(Score ~ Condition, data = full_data))
  
  ggplot(data = full_data, aes(x = Condition, y = Score)) +
    geom_boxplot() +
    theme_minimal()
  
  ggplot(data = full_data, aes(x = Pre_Expectation, y = Score)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    theme_minimal()+
    facet_wrap(~ Condition)
  
# T-tests
  control_only <- subset(full_data, full_data$Condition == "Control")
  implicit_only <- subset(full_data, full_data$Condition == "Implicit")
  explicit_only <- subset(full_data, full_data$Condition == "Explicit")
  
  t.test(implicit_only$Score, explicit_only$Score, paired = TRUE)
  
  
# summary(aov(Score ~ Condition + Condition*Pre_Expectation, data = full_data))
  
##########

# AGQ as a mediator
# Does the condition lead to AGQ differences? 
summary(aov(AGQ_approach ~ Condition, 
            data = full_data))
summary(aov(AGQ_avoidance ~ Condition, data = full_data))
# Does AGQ lead to diff in scores? 
summary(lm(Score ~ AGQ_approach + Pre_Expectation, data = full_data))
summary(lm(Score ~ AGQ_avoidance+ Pre_Expectation, data = full_data))

  ggplot(data = full_data, aes(x = P_Approach, y = Score)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    theme_minimal()+
    facet_wrap(~ Condition)
  
  ggplot(data = full_data, aes(x = AGQ_approach, y = Score)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    theme_minimal()+
    facet_wrap(~ Condition)
  
  
  mean(full_data$P_Approach - full_data$AGQ_approach)
  
  ggplot(data = full_data, aes(x = P_Avoidance, y = Score)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    theme_minimal()+
    facet_wrap(~ Condition)
  
  
# Change in Centrality
  full_data |> 
    group_by(Condition) |> 
    summarize(Pre_Centrality = mean(Racial_ID),
              Post_Centrality = mean(Centrality),
              Drop = mean(Centrality) - mean(Racial_ID))
  
  full_data <- full_data |> 
                  mutate(Diff_Centrality = Centrality - Racial_ID)
  
  ggplot(data = full_data, aes(x = Diff_Centrality)) +
    geom_density()
  
  
  centrality_model <- lm(Centrality ~ Condition + Racial_ID + Condition:Racial_ID, 
                         data = full_data)
  summary(centrality_model)
  
  

  
  
  
  
  



