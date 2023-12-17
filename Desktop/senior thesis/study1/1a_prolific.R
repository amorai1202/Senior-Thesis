# STUDY 1A PROLIFIC EDA

# Install packages --------------------------------------------------------

library(tidyverse)
library(data.table)

# Load & filter data -------------------------------------------------------

all_prolific <- read.csv("init_prolific.csv")
# View(all_prolific)
# colnames(all_prolific)
  
# Remove first 2 header rows
df <- all_prolific[-c(1, 2),]
# Turn empty strings to na's
df <- df %>% mutate_all(~na_if(., ""))

# Add a column for which condition participant was assigned to
df <- df %>%
  mutate(Condition = ifelse(!is.na(Control_common_1), "Control",
                     ifelse(!is.na(Q1), "Subtle",
                     "Blatant")))

# Num of finished participants
nrow(subset(df, df$Finished == 1))

# Filter out those that didn't finish and all 3 attention check q's wrong
df_filter <- df %>%
  filter(Finished == 1 & 
                  !((mc_task != "1" &
                      mc_mathability != "1" &
                      mc_diagnostic != "1")))

# How many didn't pass manipulation check
nrow(df) - nrow(df_filter)

# Final sample size
nrow(df_filter)
table(df_filter$Condition)

# Calculate task performance ------------------------------------------------

# Task responses (if NA, converted to 0)
spatial_rotation <- df_filter %>%
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
df_filter$Score <- rowSums(spatial_rotation == correct_sequence)


# Average DV's ------------------------------------------------------------

df_avg <- df_filter %>%
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
df_avg$Minutes <- (df_avg$Duration..in.seconds. / 60)

# Calculate the average in minutes
average_minutes <- mean(df_avg$Minutes)
# Plot distribution
duration <- ggplot(df_avg, aes(x = Minutes)) +
  geom_boxplot() +
  theme_bw()
# Add the average text label
duration + geom_text(aes(x = average_minutes, 
                  label = paste("Avg duration: ", round(average_minutes, 2), " minutes")),
                  x = 45, y = 0.0, vjust = 2, hjust = 1, color = "red", size = 4)


# Performance distribution ------------------------------------------------

# By Condition
participants <- nrow(df_avg)
order <- c("Control", "Subtle", "Blatant")
# Reorder the "Condition" factor variable
df_avg$Condition <- factor(df_avg$Condition, levels = order)

# Boxplot
ggplot(df_avg, aes(x = Condition, y = Score, fill = Condition)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Performance on Task by Condition (Prolific)",
       x = "Condition",
       y = "Score (out of 15)") +
  scale_y_continuous(limits = c(0, 15)) + 
  theme_minimal()


# Expectation
ggplot(df_avg, aes(x = as.numeric(pre.expectation), y = Score, 
                   color = Condition)) +
  geom_point(alpha = 0.9) +
  labs(title = "Expectation vs Performance By Condition",
       x = "Pre-Task Expectation",
       y = "Score (out of 15)") +
  scale_y_continuous(limits = c(0, 15)) + 
  theme_minimal()

# By Goal Orientation: APPROACH
ggplot(df_avg, aes(x = Score, y = AGQ_approach)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Performance vs Approach Orientation",
       x = "Score (out of 15)",
       y = "Approach") +
  scale_x_continuous(limits = c(0, 15)) + 
  theme_minimal()

# By Goal Orientation: AVOIDANCE
ggplot(df_avg, aes(x = Score, y = AGQ_avoidance)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Performance vs Avoidance Orientation",
       x = "Score (out of 15)",
       y = "Avoidance") +
  scale_x_continuous(limits = c(0, 15)) + 
  theme_minimal()


# ANOVA
summary(aov(Score ~ Condition, data = df_avg))
# fail to reject = insufficient evidence to conclude that there are significant differences in means between the groups. 
# not enough evidence to suggest that at least one group mean is different from the others


# Comparing 2 conditions: t-tests 

# control vs subtle
control_subtle <- df_avg %>%
  filter(Condition %in% c("Control", "Subtle"))
t_test_control_subtle <- t.test(Score ~ Condition, data = control_subtle)
# control vs blatant
control_blatant <- df_avg %>%
  filter(Condition %in% c("Control", "Blatant"))
t_test_control_blatant <- t.test(Score ~ Condition, data = control_blatant)
# subtle vs blatant
subtle_blatant <- df_avg %>%
  filter(Condition %in% c("Subtle", "Blatant"))
t_test_subtle_blatant <- t.test(Score ~ Condition, data = subtle_blatant)

# Check the results
t_test_control_subtle
t_test_control_blatant
t_test_subtle_blatant


# Demographics ------------------------------------------------------------

#Age
quantX.C <- subset(df_avg, Condition == "Control",
                   select = c(Age))
quantX.S <- subset(df_avg, Condition == "Subtle",
                   select = c(Age))
quantX.B <- subset(df_avg, Condition == "Blatant",
                   select = c(Age))
#Gender
quantX.C_avg <- apply(quantX.C, MARGIN = 2, FUN = mean)
quantX.S_avg <- apply(quantX.S, MARGIN = 2, FUN = mean)
quantX.B_avg <- apply(quantX.C, MARGIN = 2, FUN = mean)
quantX.C_sd <- apply(quantX.C, MARGIN = 2, FUN = sd)
quantX.S_sd <- apply(quantX.S, MARGIN = 2, FUN = sd)
quantX.B_sd <- apply(quantX.C, MARGIN = 2, FUN = sd)

num_women.C <- sum(df_avg$Gender == "1" & df_avg$Condition == "Control")
num_women.S <- sum(df_avg$Gender == "1" & df_avg$Condition == "Subtle")
num_women.B <- sum(df_avg$Gender == "1" & df_avg$Condition == "Blatant")
num_men.C <- sum(df_avg$Gender == "2" & df_avg$Condition == "Control")
num_men.S <- sum(df_avg$Gender == "2" & df_avg$Condition == "Subtle")
num_men.B <- sum(df_avg$Gender == "2" & df_avg$Condition == "Blatant")
num_nonbinary.C <- sum(df_avg$Gender == "3" & df_avg$Condition == "Control")
num_nonbinary.S <- sum(df_avg$Gender == "3" & df_avg$Condition == "Subtle")
num_nonbinary.B <- sum(df_avg$Gender == "3" & df_avg$Condition == "Blatant")

#FirstGen & STEM

binaryX.C <- subset(df_avg, Condition == "Control",
                    select = c(FirstGen, FirstGen_US, 
                               X1st_immigrant, 
                               CurrentMajor))
binaryX.S <- subset(df_avg, Condition == "Subtle",
                    select = c(FirstGen, FirstGen_US, 
                               X1st_immigrant, 
                               CurrentMajor))
binaryX.B <- subset(df_avg, Condition == "Blatant",
                    select = c(FirstGen, FirstGen_US, 
                               X1st_immigrant,
                               CurrentMajor))

C_num <- apply(binaryX.C, MARGIN = 2, function(x) sum(as.numeric(x[x == 1])))
S_num <- apply(binaryX.S, MARGIN = 2, function(x) sum(as.numeric(x[x == 1])))
B_num <- apply(binaryX.B, MARGIN = 2, function(x) sum(as.numeric(x[x == 1])))

#Table 1
quant_bar <- cbind(quantX.C_avg, quantX.S_avg, quantX.B_avg)
quant_sd <- cbind(quantX.C_sd, quantX.S_sd, quantX.B_sd)
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





