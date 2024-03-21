# STUDY 1A CMU EDA

# Install packages --------------------------------------------------------

library(tidyverse)
library(data.table)

# Load & filter data -------------------------------------------------------

all_cmu <- read.csv("init_cmu.csv")

# Remove first 2 header rows
cmu <- all_cmu[-c(1, 2),]
# Turn empty strings to na's
cmu <- cmu %>% mutate_all(~na_if(., ""))

# Add a column for which condition participant was assigned to
cmu <- cmu %>%
  mutate(Condition = ifelse(!is.na(Control_common_1), "Control",
                            ifelse(!is.na(Q1), "Subtle",
                                   "Blatant")))

# Num of finished participants
nrow(subset(cmu, cmu$Finished == 1))

# Filter out those that didn't finish and all 3 attention check q's wrong
# Also filtering out non-Asians
cmu_filter <- cmu %>%
  filter(Finished == 1 & 
           !((mc_task != "1" &
                mc_mathability != "1" &
                mc_diagnostic != "1")) &
           #Asians only
           Race == "2")

# How many didn't pass manipulation check
nrow(cmu) - nrow(cmu_filter)

# Final sample size
nrow(cmu_filter)
table(cmu_filter$Condition)

# Calculate task performance ------------------------------------------------

# Task responses (if NA, converted to 0)
spatial_rotation <- cmu_filter %>%
  mutate_at(vars(X6_150_R, X7_150_R, X8_150_R, X9_150, X10_150_R,
                 X11_150, X12_150, X13_150, X14_150_R, X15_150,  
                 X16_150_R, X17_150_R, X18_150_R, X19_150, X20_150_R),
            ~coalesce(as.numeric(.), 0)) %>% 
  select(X6_150_R, X7_150_R, X8_150_R, X9_150, X10_150_R,
         X11_150, X12_150, X13_150, X14_150_R, X15_150,  
         X16_150_R, X17_150_R, X18_150_R, X19_150, X20_150_R)

# Define the correct sequence
correct_sequence <- c(2, 2, 2, 1, 2, 
                      1, 1, 1, 2, 1, 
                      2, 2, 2, 1, 2)


# New column 'Score' represents the num of correct responses the participant got out of 15
cmu_filter$Score <- rowSums(spatial_rotation == correct_sequence)


# Average DV's ------------------------------------------------------------

cmu_avg <- cmu_filter %>%
  # convert to numeric
  mutate(across(c(5, 24, 29:38, 48:71, 176:218), as.numeric)) %>%  
  rowwise() %>%  
  # calculate row means and round to 3 decimal places
  mutate(Control_common = round(mean(c_across(29:33), na.rm = TRUE), 3),
         Control_true = round(mean(c_across(34:38), na.rm = TRUE), 3),
         Blatant_common = round(mean(c_across(48:53), na.rm = TRUE), 3),
         Blatant_true = round(mean(c_across(54:59), na.rm = TRUE), 3),
         STAI_pre = round(mean(c_across(60:65), na.rm = TRUE), 3),
         Overall_AGQ = round(mean(c_across(66:71), na.rm = TRUE), 3),
         AGQ_approach = round(mean(c_across(66:68), na.rm = TRUE), 3),
         #Questions 4,5,6 in AGQ are reverse coded: revert back to get avoidance mindset
         AGQ_avoidance = 6 - (round(mean(c_across(69:71), na.rm = TRUE), 3)),
         STAI_post = round(mean(c_across(176:181), na.rm = TRUE), 3),
         Belonging = round(mean(c_across(213:215), na.rm = TRUE), 3),
         Centrality = round(mean(c_across(216:218), na.rm = TRUE), 3)
  )



# Duration distribution ---------------------------------------------------

# Calculate the average & convert to minutes
cmu_avg$Minutes <- (as.numeric(cmu_avg$Duration..in.seconds.) / 60)

# Calculate the average in minutes
average_minutes <- mean(cmu_avg$Minutes)
# Plot distribution
duration <- ggplot(cmu_avg, aes(x = Minutes)) +
  geom_boxplot() +
  theme_bw()
# Add the average text label
duration + geom_text(aes(x = average_minutes, 
                         label = paste("Avg duration: ", round(average_minutes, 2), " minutes")),
                     x = 45, y = 0.0, vjust = 2, hjust = 1, color = "red", size = 4)


# Performance distribution ------------------------------------------------

# By Condition

order <- c("Control", "Subtle", "Blatant")
# Reorder the "Condition" factor variable
cmu_avg$Condition <- factor(cmu_avg$Condition, levels = order)

# Boxplot
ggplot(cmu_avg, aes(x = Condition, y = Score, fill = Condition)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Performance on Task by Condition (CMU)",
       x = "Condition",
       y = "Score (out of 15)") +
  scale_y_continuous(limits = c(0, 15)) + 
  theme_minimal()


# Expectation
ggplot(cmu_avg, aes(x = as.numeric(pre.expectation), y = Score, 
                   color = Condition)) +
  geom_point(alpha = 0.9) +
  labs(title = "Expectation vs Performance By Condition",
       x = "Pre-Task Expectation",
       y = "Score (out of 15)") +
  scale_y_continuous(limits = c(0, 15)) + 
  theme_minimal()

# By Goal Orientation: APPROACH
ggplot(cmu_avg, aes(x = Score, y = AGQ_approach)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Performance vs Approach Orientation",
       x = "Score (out of 15)",
       y = "Approach") +
  scale_x_continuous(limits = c(0, 15)) + 
  theme_minimal()

# By Goal Orientation: AVOIDANCE
ggplot(cmu_avg, aes(x = Score, y = AGQ_avoidance)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Performance vs Avoidance Orientation",
       x = "Score (out of 15)",
       y = "Avoidance") +
  scale_x_continuous(limits = c(0, 15)) + 
  theme_minimal()


# ANOVA
summary(aov(Score ~ Condition, data = cmu_avg))
# fail to reject = insufficient evidence to conclude that there are significant differences in means between the groups. 
# not enough evidence to suggest that at least one group mean is different from the others


# Comparing 2 conditions: t-tests 

# control vs subtle
control_subtle <- cmu_avg %>%
  filter(Condition %in% c("Control", "Subtle"))
t_test_control_subtle <- t.test(Score ~ Condition, data = control_subtle)
# control vs blatant
control_blatant <- cmu_avg %>%
  filter(Condition %in% c("Control", "Blatant"))
t_test_control_blatant <- t.test(Score ~ Condition, data = control_blatant)
# subtle vs blatant
subtle_blatant <- cmu_avg %>%
  filter(Condition %in% c("Subtle", "Blatant"))
t_test_subtle_blatant <- t.test(Score ~ Condition, data = subtle_blatant)

# Check the results
t_test_control_subtle
t_test_control_blatant
t_test_subtle_blatant


# Demographics ------------------------------------------------------------

#Age
C_age <- mean(as.numeric(subset(cmu_avg, Condition == "Control")$Age))
S_age <- mean(as.numeric(subset(cmu_avg, Condition == "Subtle")$Age))
B_age <- mean(as.numeric(subset(cmu_avg, Condition == "Blatant")$Age))
C_age_sd <- sd(as.numeric(subset(cmu_avg, Condition == "Control")$Age))
S_age_sd <- sd(as.numeric(subset(cmu_avg, Condition == "Subtle")$Age))
B_age_sd <- sd(as.numeric(subset(cmu_avg, Condition == "Blatant")$Age))

#Gender

num_women.C <- sum(cmu_avg$Gender == "1" & cmu_avg$Condition == "Control")
num_women.S <- sum(cmu_avg$Gender == "1" & cmu_avg$Condition == "Subtle")
num_women.B <- sum(cmu_avg$Gender == "1" & cmu_avg$Condition == "Blatant")
num_men.C <- sum(cmu_avg$Gender == "2" & cmu_avg$Condition == "Control")
num_men.S <- sum(cmu_avg$Gender == "2" & cmu_avg$Condition == "Subtle")
num_men.B <- sum(cmu_avg$Gender == "2" & cmu_avg$Condition == "Blatant")
num_nonbinary.C <- sum(cmu_avg$Gender == "3" & cmu_avg$Condition == "Control")
num_nonbinary.S <- sum(cmu_avg$Gender == "3" & cmu_avg$Condition == "Subtle")
num_nonbinary.B <- sum(cmu_avg$Gender == "3" & cmu_avg$Condition == "Blatant")

#FirstGen & STEM

binaryX.C <- subset(cmu_avg, Condition == "Control",
                    select = c(FirstGen, FirstGen_US, 
                               X1st_immigrant, 
                               CurrentMajor))
binaryX.S <- subset(cmu_avg, Condition == "Subtle",
                    select = c(FirstGen, FirstGen_US, 
                               X1st_immigrant, 
                               CurrentMajor))
binaryX.B <- subset(cmu_avg, Condition == "Blatant",
                    select = c(FirstGen, FirstGen_US, 
                               X1st_immigrant,
                               CurrentMajor))

C_num <- apply(binaryX.C, MARGIN = 2, function(x) sum(as.numeric(x[x == 1])))
S_num <- apply(binaryX.S, MARGIN = 2, function(x) sum(as.numeric(x[x == 1])))
B_num <- apply(binaryX.B, MARGIN = 2, function(x) sum(as.numeric(x[x == 1])))

#Table 1
quant_bar <- cbind(C_age, S_age, B_age)
quant_sd <- cbind(C_age_sd, S_age_sd, B_age_sd)
num_women <- cbind(num_women.C, num_women.S, num_women.B)
num_men <- cbind(num_men.C, num_men.S, num_men.B)
num_nonbinary <- cbind(num_nonbinary.C, num_nonbinary.S, num_nonbinary.B)
binary_num <- cbind(C_num, S_num, B_num)

table <- rbind(round(quant_bar,2),
               round(quant_sd,2),
               num_women,
               num_men,
               num_nonbinary,
               binary_num)
colnames(table)[1:3] = c("Control", "Subtle", "Blatant")
rownames(table)[1:9] = c("Age - Mean",
                         "Age - Std Dev",
                         "Women",
                         "Men",
                         "Nonbinary",
                         "FirstGen",
                         "FirstGen - US",
                         "Immigrant",
                         "STEM")
#data.frame(table1)





