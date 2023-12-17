# STUDY 1 ANALYSIS

# Install packages --------------------------------------------------------

library(tidyverse)
library(data.table)

# Write function that cleans data -----------------------------------------

clean_csv <- function(file_path) {
  # Read the CSV file
  data <- read.csv(file_path)
  
  # Remove first 2 header rows
  data <- data[-c(1, 2),]
  # Turn empty strings to na's
  data <- data %>% mutate_all(~na_if(., ""))
  
  # Filter out rows that weren't finished
  data <- data %>%
    filter(Finished == 1)
  
  # Add a column for which condition participant was assigned to
  data <- data %>%
    mutate(Condition = ifelse(!is.na(Control_common_1), "Control",
                              ifelse(!is.na(Q1), "Subtle",
                                     "Blatant")))
  
  # Calculate task performance
  correct_answers <- c(
    'X6_150_R' = 2,
    'X7_150_R' = 2,
    'X8_150_R' = 2,
    'X9_150' = 1,
    'X10_150_R' = 2,
    'X11_150' = 1,
    'X12_150' = 1,
    'X13_150' = 1,
    'X14_150_R' = 2,
    'X15_150' = 1,
    'X16_150_R' = 2,
    'X17_150_R' = 2,
    'X18_150_R' = 2,
    'X19_150' = 1,
    'X20_150_R' = 2
  )
  
  # Count the number of matches for each row
  data$Score <- rowSums(sapply(names(correct_answers), 
                               function(col) data[[col]] == correct_answers[col] & 
                                 !is.na(data[[col]])))
  
  # Reorder the "Condition" factor variable
  order <- c("Control", "Subtle", "Blatant")
  data$Condition <- factor(data$Condition, levels = order)
  
  # Recode factor variables
  data <- data %>%
    mutate(Comparison = 
             recode_factor(Comparison,
                           "1" = "10th percentile",
                           "2" = "25th percentile",
                           "3" = "50th percentile",
                           "4" = "75th percentile",
                           "5" = "90th percentile"),
           StudentStatus = 
             recode_factor(StudentStatus,
                           "1" = "Undergraduate",
                           "2" = "Masters",
                           "3" = "Doctoral",
                           "4" = "Other"),
           Gender = 
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
           X1st_immigrant = 
             recode_factor(X1st_immigrant,
                           "1" = "Yes",
                           "2" = "No",
                           #Not sure is coded as no
                           "3" = "No"),
           X2nd_immigrant = 
             recode_factor(X2nd_immigrant,
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
           CurrentMajor = 
             recode_factor(CurrentMajor,
                           "1" = "STEM",
                           "2" = "Not STEM",
                           "3" = "Not STEM")
    )
  
  clean_data <- data %>% 
    # Convert columns to numeric
    mutate_at(vars(Duration..in.seconds.,
                   Score, 
                   Q6,
                   pre.expectation, 
                   post.expectation,
                   Age,
                   X1st_age,
                   X2nd_age,
                   GPA), as.numeric) %>% 
    # Only grab necessary columns
    select(Condition, Score, Age, Gender, CurrentMajor, StudentStatus, pre.expectation, post.expectation, Comparison,
           UndergradYear, MastersYear, DoctoralYear,
           Control_common_1, Control_common_2, Control_common_3, Control_common_4, Control_common_5,
           Control_true_1, Control_true_2, Control_true_3, Control_true_4, Control_true_5,
           Q1_1_TEXT, Q2.1, Q3_1_TEXT, Q4_1_TEXT, Q5, Q6, 
           Blatant_common_1, Blatant_common_2, Blatant_common_3, Blatant_common_4, Blatant_common_5, Blatant_common_6, 
           Blatant_true_1, Blatant_true_2, Blatant_true_3, Blatant_true_4, Blatant_true_5, Blatant_true_6, 
           STAI_1, STAI_2, STAI_3, STAI_4, STAI_5, STAI_6, 
           AGQ_1, AGQ_2, AGQ_3, AGQ_4, AGQ_5, AGQ_6,
           mc_task, mc_mathability, mc_diagnostic, 
           STAI_1.1, STAI_2.1, STAI_3.1, STAI_4.1, STAI_5.1, STAI_6.1, 
           task_stressful, Pressure, task_opinion, Math_abilities1, Math_abilities2, 
           Q3.1, Q4.1, Q5.1, Q7, Q8, Q9, Q11, Q12, Q13, Q14_1, Q14_2, Q14_3, Q14_4, Q14_5, Q14_6, Q14_7, Q14_8, Q14_9, 
           RIT1, RIT2, RIT3, RIT4, RIT5, RIT6, 
           B1, B2, B3_Reverse, 
           Centrality1, Centrality2, Centrality3, 
           Race, FirstGen, FirstGen_US, 
           X1st_immigrant, X1st_age, X2nd_immigrant, X2nd_age, 
           HighestDegree, GPA, 
           Duration..in.seconds., Finished, Feedback, Purpose) %>% 
    #Rename
    rename(FamilyLanguage = Q1_1_TEXT,
           OtherLanguage = Q2.1,
           LanguageatHome = Q3_1_TEXT,
           LanguageonCampus = Q4_1_TEXT,
           StudentOrg = Q5,
           NumGenerations = Q6,
           Pre_Expectation = pre.expectation,
           Post_Expectation = post.expectation)
  
  
  #Average DV's
  avg_data <- clean_data %>%
    # convert to numeric
    mutate(across(c(13:22, 29:52, 56:61, 85:96), as.numeric)) %>%  
    rowwise() %>%  
    # calculate row means and round to 3 decimal places
    mutate(Control_common = round(mean(c_across(13:17), na.rm = TRUE), 3),
           Control_true = round(mean(c_across(18:22), na.rm = TRUE), 3),
           Blatant_common = round(mean(c_across(29:34), na.rm = TRUE), 3),
           Blatant_true = round(mean(c_across(35:40), na.rm = TRUE), 3),
           STAI_pre = round(mean(c_across(41:46), na.rm = TRUE), 3),
           Overall_AGQ = round(mean(c_across(47:52), na.rm = TRUE), 3),
           AGQ_approach = round(mean(c_across(47:49), na.rm = TRUE), 3),
           #Questions 4,5,6 in AGQ are reverse coded: revert back to get avoidance mindset
           AGQ_avoidance = 6 - (round(mean(c_across(50:52), na.rm = TRUE), 3)),
           STAI_post = round(mean(c_across(56:61), na.rm = TRUE), 3),
           RIT = round(mean(c_across(85:90), na.rm = TRUE), 3),
           Belonging = round(mean(c_across(91:93), na.rm = TRUE), 3),
           Centrality = round(mean(c_across(94:96), na.rm = TRUE), 3)
    )
  
  return(avg_data)
                           
}

# Run function
cleaned_prolific <- clean_csv("raw_prolific.csv")
cleaned_cmu <- clean_csv("raw_cmu.csv")
cleaned_combined <- bind_rows(cleaned_prolific, cleaned_cmu)
  
# write.csv(df_prolific,"cleaned_prolific.csv", row.names = FALSE)
# write.csv(df_cmu,"cleaned_cmu.csv", row.names = FALSE)
# write.csv(df_combined,"cleaned_combined.csv", row.names = FALSE)


# Analysis ------------------------------------------------------------------

# Checking if AGQ means from the samples are statistically different
t.test(cleaned_prolific$Score, cleaned_cmu$Score) 

# Primary ANOVA (condition x score)
summary(aov(Score ~ Condition, data = cleaned_combined))
# fail to reject = insufficient evidence to conclude that there are significant differences in means between the groups. 
# not enough evidence to suggest that at least one group mean is different from the others

# Gender Differences
summary(aov(Score ~ Gender, data = cleaned_combined))
summary(aov(Pre_Expectation ~ Gender, data = cleaned_combined))
summary(aov(Post_Expectation ~ Gender, data = cleaned_combined))
summary(aov(AGQ_approach ~ Gender, data = cleaned_combined))
summary(aov(AGQ_avoidance ~ Gender, data = cleaned_combined))

# AGQ as a mediator
# Does the condition lead to AGQ differences? No
summary(aov(AGQ_approach ~ Condition, data = cleaned_combined))
summary(aov(AGQ_avoidance ~ Condition, data = cleaned_combined))
# Does AGQ lead to diff in scores? No
summary(lm(Score ~ AGQ_approach, data = cleaned_combined))
summary(lm(Score ~ AGQ_avoidance, data = cleaned_combined))

# Centrality interaction
summary(lm(Score ~ Centrality*Condition, data = cleaned_combined))


