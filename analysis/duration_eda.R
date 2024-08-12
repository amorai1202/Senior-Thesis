# EDA of Duration on Each Task Question

durations <- data

correctness_data <- as.data.frame(sapply(names(correct_answers), 
       function(col) data[[col]] == correct_answers[col] & 
         !is.na(data[[col]])))

time_data <- clean_data |> 
                  select(c(ends_with("First Click"), Condition)) |> 
                  select(-c(1:5, `debrief_timer_First Click`)) |> 
                  mutate(across(where(is.character), as.numeric))

# Calculate average duration by Condition
Q_duration_condition <- time_data %>%
  group_by(Condition) %>%
  summarise(across(starts_with("timer"), ~ mean(.x, na.rm = TRUE)))

print(average_duration)

# Reshape the data to long format
time_data_long <- time_data %>%
  pivot_longer(cols = starts_with("timer"), names_to = "Question", values_to = "Duration")

# Ensure Condition is a factor
time_data_long$Condition <- as.factor(time_data_long$Condition)

# Perform ANOVA
anova_result <- aov(Duration ~ Condition, data = time_data_long)
summary(anova_result)

# Perform Tukey's HSD post-hoc test if ANOVA is significant
if(summary(anova_result)[[1]]["Pr(>F)"][1] < 0.05) {
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
}

# Duration by condition
ggplot(time_data_long, aes(x = Condition, y = Duration, fill = Condition)) +
  geom_boxplot() +
  labs(title = "Duration by Condition", x = "Condition", y = "Duration (seconds)") +
  theme_minimal()

# Checking duration for correct vs incorrect answers

time_data <- as.data.frame(lapply(time_data, function(x) as.numeric(as.character(x))))

# Combine the datasets
combined_data <- cbind(correctness_data, time_data)

# Calculate average times for correct and incorrect answers for each individual
average_times <- data.frame(
  Participant = 1:nrow(correctness_data),
  Avg_Time_Correct = apply(combined_data, 1, function(row) {
    correct_indices <- 1:ncol(correctness_data)
    mean(row[correct_indices & row[correct_indices]], na.rm = TRUE)
  }),
  Avg_Time_Incorrect = apply(combined_data, 1, function(row) {
    incorrect_indices <- 1:ncol(correctness_data)
    mean(row[incorrect_indices & !row[incorrect_indices]], na.rm = TRUE)
  })
)

t_test_result <- t.test(average_times$Avg_Time_Correct, average_times$Avg_Time_Incorrect, paired = TRUE, na.action = na.omit)
print(t_test_result)

# Visualize the comparison using a box plot
library(ggplot2)
average_times_long <- reshape2::melt(average_times, id.vars = "Individual", variable.name = "Type", value.name = "Avg_Time")

ggplot(average_times_long, aes(x = Type, y = Avg_Time, fill = Type)) +
  geom_boxplot() +
  labs(title = "Comparison of Average Time Taken for Correct and Incorrect Answers", 
       x = "Answer Type", y = "Average Time (seconds)") +
  theme_minimal()

