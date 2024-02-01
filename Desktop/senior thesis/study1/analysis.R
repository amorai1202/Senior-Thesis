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
cleaned_prolific <- clean_csv("data/raw_prolific.csv")
cleaned_cmu <- clean_csv("data/raw_cmu.csv")
# Add dataset labels
cleaned_prolific$Dataset <- "Prolific"
cleaned_cmu$Dataset <- "CMU"
cleaned_combined <- bind_rows(cleaned_prolific, cleaned_cmu)
  
# write.csv(df_prolific,"cleaned_prolific.csv", row.names = FALSE)
# write.csv(df_cmu,"cleaned_cmu.csv", row.names = FALSE)
# write.csv(df_combined,"cleaned_combined.csv", row.names = FALSE)

# Comparing datasets ------------------------------------------------------

# Checking if AGQ means from the samples are statistically different - NO
t.test(cleaned_prolific$Score, cleaned_cmu$Score) 

# Demographic distributions

prop.table(table(cleaned_prolific$Gender)) #47% W, 51% M
# More women in CMU dataset
prop.table(table(cleaned_cmu$Gender)) #60% W, 37% M
t.test(cleaned_prolific$Age, cleaned_cmu$Age) # 24 years old vs 20 years old
prop.table(table(cleaned_prolific$CurrentMajor)) #56% STEM
prop.table(table(cleaned_cmu$CurrentMajor)) #33% STEM
prop.table(table(cleaned_prolific$StudentStatus)) #75% UG
prop.table(table(cleaned_cmu$StudentStatus)) #98% UG
prop.table(table(cleaned_prolific$FirstGen)) #75% UG
prop.table(table(cleaned_cmu$FirstGen)) #98% UG

# DV's
ggplot(data = cleaned_combined, aes(x = Centrality, fill = Dataset)) +
  geom_density(position = "dodge") +
  theme_bw()
t.test(cleaned_prolific$Centrality, cleaned_cmu$Centrality)  # Significantly lower centrality in CMU dataset
t.test(cleaned_prolific$AGQ_approach, cleaned_cmu$AGQ_approach) 
t.test(cleaned_prolific$AGQ_avoidance, cleaned_cmu$AGQ_avoidance) 
t.test(cleaned_prolific$RIT, cleaned_cmu$RIT) #Significantly lower RIT for CMU
t.test(cleaned_prolific$Belonging, cleaned_cmu$Belonging) 

# Final combined dataset --------------------------------------------------

# only asian, remove outliers (z <= -3)

#Split dataset into low/high centrality (if less than 3.3, then low)
# median/mean = 3.3

cleaned_combined$Centrality_F <- ifelse (cleaned_combined$Centrality < 3.3, "Low", "High")
cleaned_cmu$Centrality_F <- ifelse (cleaned_cmu$Centrality < 3.3, "Low", "High")
cleaned_prolific$Centrality_F <- ifelse (cleaned_prolific$Centrality < 3.3, "Low", "High")

# Manipulation check

cleaned_combined <- cleaned_combined %>% 
                      filter(!((mc_task != "1" &
                                    mc_mathability != "1" &
                                    mc_diagnostic != "1")))    

# Asian only
cleaned_combined <- cleaned_combined %>%
  filter(str_detect(Race, "2"))

# Standardize scores
cleaned_combined$z_Score <- 
  (cleaned_combined$Score - mean(cleaned_combined$Score, na.rm = TRUE)) / sd(cleaned_combined$Score, na.rm = TRUE)

ggplot(data = cleaned_combined, aes(x = Score)) +
  geom_density() +
  labs(title = "Distribution of Performance") +
  theme_bw() +
  # Add vertical line for mean
  geom_vline(aes(xintercept = mean(Score)), color = "red", linetype = "dashed", size = 1) +
  # Annotate with numeric mean value
  annotate("text", x = mean(cleaned_combined$Score), y = 0.03, label = paste("Mean =", round(mean(cleaned_combined$Score), 2)), vjust = 1.5, color = "red")

# None Z <= -3
#sort(cleaned_combined$z_Score)

sort(cleaned_combined$Score)

#Final sample size = 264
table(cleaned_combined$Condition)
# write.csv(df_combined,"cleaned_combined.csv", row.names = FALSE)

# Analysis ------------------------------------------------------------------

# Primary ANOVA (condition x score)
summary(aov(Score ~ Condition, data = cleaned_combined))
# fail to reject = insufficient evidence to conclude that there are significant differences in means between the groups. 
# not enough evidence to suggest that at least one group mean is different from the others

summary(aov(Score ~ Condition, data = cleaned_cmu))

# Gender Differences
summary(aov(Score ~ Gender, data = cleaned_combined))
summary(aov(Pre_Expectation ~ Gender, data = cleaned_combined))
summary(aov(Post_Expectation ~ Gender, data = cleaned_combined))
summary(aov(AGQ_approach ~ Gender, data = cleaned_combined))
summary(aov(AGQ_avoidance ~ Gender, data = cleaned_combined))

# Do the effects of the manipulations look different for women and men? No
W_only <- subset(cleaned_combined, cleaned_combined$Gender == "Woman")
M_only <- subset(cleaned_combined, cleaned_combined$Gender == "Man")
summary(aov(Score ~ Condition, data = M_only))


# AGQ as a mediator
# Does the condition lead to AGQ differences? No
summary(aov(AGQ_approach ~ Condition, data = cleaned_combined))
summary(aov(AGQ_avoidance ~ Condition, data = cleaned_combined))
# Does AGQ lead to diff in scores? No
summary(lm(Score ~ AGQ_approach, data = cleaned_combined))
summary(lm(Score ~ AGQ_avoidance, data = cleaned_combined))

# Centrality interaction
centrality_mod <- aov(Score ~ Condition, data = cleaned_combined)
summary(centrality_mod)
plot(fitted(centrality_mod))


###  --- check differences for low vs high centrality samples separately -- ###

#Combined

low_Centrality <- subset(cleaned_combined, cleaned_combined$Centrality_F == "Low")
high_Centrality <-subset(cleaned_combined, cleaned_combined$Centrality_F == "High")
centrality_mod <- aov(Score ~ Condition, data = low_Centrality)
summary(centrality_mod)

low_subtle <- subset(low_Centrality, low_Centrality$Condition == "Subtle")
low_blatant <- subset(low_Centrality, low_Centrality$Condition == "Blatant")
low_control <- subset(low_Centrality, low_Centrality$Condition == "Control")
  
t.test(low_subtle$Score, low_blatant$Score) 
t.test(low_subtle$Score, low_control$Score)
t.test(low_blatant$Score, low_control$Score)

ggplot(data = low_Centrality, aes(x = Condition, y = Score, fill = Centrality_F)) +
  geom_boxplot() +
  theme_bw()

ggplot(data = low_Centrality, aes(x = Condition, y = AGQ_avoidance, fill = Centrality_F)) +
  geom_boxplot() +
  theme_bw()

ggplot(data = low_Centrality, aes(x = Condition, y = AGQ_avoidance, fill = Centrality_F)) +
  geom_boxplot() +
  theme_bw()


#Prolific
low_Centrality_p <- subset(cleaned_prolific, cleaned_prolific$Centrality_F == "Low")
high_Centrality_p <-subset(cleaned_prolific, cleaned_prolific$Centrality_F == "High")
centrality_mod_p <- aov(Score ~ Condition, data = low_Centrality_p)
summary(centrality_mod_p)

ggplot(data = low_Centrality_p, aes(x = Condition, y = Score, fill = Centrality_F)) +
  geom_boxplot() +
  theme_bw()

ggplot(data = low_Centrality_cmu, aes(x = Condition, y = Score, fill = Centrality_F)) +
  geom_boxplot() +
  theme_bw()

#CMU
low_Centrality_cmu <- subset(cleaned_cmu, cleaned_cmu$Centrality_F == "Low")
high_Centrality_cmu <-subset(cleaned_cmu, cleaned_cmu$Centrality_F == "High")
centrality_mod_cmu <- aov(Score ~ Condition, data = low_Centrality_cmu)
summary(centrality_mod_cmu)


#the relationship between Condition x Score depends on Centrality
# for each additional increase in centrality, the avg score in blatant is predicted to decrease compared than control


# Regression --------------------------------------------------------------

summary(lm(Score ~ Condition + Centrality + Condition:Centrality + Condition*Gender, data = cleaned_combined))
summary(lm(Score ~ Condition + Gender + Condition:Gender, data = cleaned_combined))

summary(lm(Score ~ Condition + AGQ_approach + AGQ_avoidance + Gender + Centrality*Condition +
             RIT + FirstGen + Age + CurrentMajor, data = cleaned_combined))

summary(lm(Score ~ Condition + Age + CurrentMajor + Centrality*Condition
           + Gender, data = cleaned_combined))


# Gender ------------------------------------------------------------------

#--- Plot score by gender
cleaned_combined %>% ggplot(aes(x = Score, fill = Gender)) + 
  geom_histogram(bins = 50)
# Significant gender difference
summary(aov(Score ~ Gender, data = cleaned_combined))
summary(aov(Score ~ Condition * Gender, data = cleaned_combined))


women_only <- subset(cleaned_combined, cleaned_combined$Gender == "Woman")

ggplot(data = women_only, aes(x = Condition, y = Score)) +
  geom_boxplot() +
  theme_bw()

#Main Effect
summary(aov(Score ~ Condition, data = women_only))
#Other DV's
summary(aov(Pre_Expectation ~ Condition, data = women_only))
summary(aov(Post_Expectation ~ Condition, data = women_only))
summary(aov(STAI_pre ~ Condition, data = women_only))
summary(aov(STAI_post ~ Condition, data = women_only))
summary(aov(AGQ_approach ~ Condition, data = women_only))
summary(aov(AGQ_avoidance ~ Condition, data = women_only))
###
summary(aov(RIT ~ Condition, data = women_only))
###
ggplot(data = women_only, aes(x = Condition, y = STAI_pre)) +
  geom_boxplot() +
  theme_bw()
###
summary(aov(Belonging ~ Condition, data = women_only))

# AGQ as a mediator
summary(lm(Score ~ AGQ_approach, data = women_only))
summary(lm(Score ~ AGQ_avoidance, data = women_only))

# T-Tests

c_women <- subset(women_only, women_only$Condition == "Control")
s_women <- subset(women_only, women_only$Condition == "Subtle")
b_women <- subset(women_only, women_only$Condition == "Blatant")

t.test(c_women$RIT, s_women$RIT) #signifiant
t.test(c_women$RIT, b_women$RIT) #signifiant
t.test(s_women$RIT, b_women$RIT)



# Racial Identity (Centrality) --------------------------------------------

ggplot(data = cleaned_combined, aes(x = Centrality, fill = Dataset)) +
  geom_density(position = "dodge") +
  theme_bw()

low_Centrality <- subset(cleaned_combined, cleaned_combined$Centrality_F == "Low")
high_Centrality <-subset(cleaned_combined, cleaned_combined$Centrality_F == "High")

t.test(low_Centrality$Score, high_Centrality$Score) 

ggplot(data = cleaned_combined, aes(x = Condition, y = Score, fill = Centrality_F)) +
  geom_boxplot() +
  theme_bw()

ggplot(data = cleaned_combined, aes(x = Condition, y = AGQ_approach, fill = Centrality_F)) +
  geom_boxplot() +
  theme_bw()
# Low centrality folks are significantly less likely to adopt an approach mindset
t.test(low_Centrality$AGQ_approach, high_Centrality$AGQ_approach) 

ggplot(data = cleaned_combined, aes(x = Condition, y = AGQ_avoidance, fill = Centrality_F)) +
  geom_boxplot() +
  theme_bw()

# But not significantly more likely to adopt an avoidance
t.test(low_Centrality$AGQ_avoidance, high_Centrality$AGQ_avoidance) 

# T-tests
low_control <- 
low_subtle <- 
low_blatant
  


# ANOVA -------------------------------------------------------------------

summary(aov(Score ~ Condition, data = cleaned_combined))
summary(aov(Pre_Expectation ~ Condition, data = cleaned_combined))
summary(aov(Post_Expectation ~ Condition, data = cleaned_combined))
summary(aov(STAI_pre ~ Condition, data = cleaned_combined))
summary(aov(STAI_post ~ Condition, data = cleaned_combined))
summary(aov(AGQ_approach ~ Condition, data = cleaned_combined))
summary(aov(AGQ_avoidance ~ Condition, data = cleaned_combined))
summary(aov(RIT ~ Condition, data = cleaned_combined))
summary(aov(Belonging ~ Condition, data = cleaned_combined))


summary(lm(Score ~ AGQ_approach, data = cleaned_combined))
summary(lm(Score ~ AGQ_avoidance, data = cleaned_combined))
summary(lm(STAI_post ~ AGQ_avoidance, data = cleaned_combined))

summary(lm(Score ~ Condition*Centrality + Gender, data = cleaned_combined))



