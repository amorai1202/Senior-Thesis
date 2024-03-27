# STUDY 1 ANALYSIS

# Install packages --------------------------------------------------------

library(tidyverse)
library(crosstable)
library(multcompView)
library(corrplot)

# Load data ---------------------------------------------------------------

part1_df <- read_csv("data/cleaned_part1.csv")
part2_df <- read_csv("data/cleaned_part2.csv")

# Make sure same people are doing both parts
part1_participants <- part1_df$ProlificID
part2_participants <- part2_df$ProlificID 
all(part2_participants %in% part1_participants)

full_data <- read_csv("data/cleaned_full.csv")

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
  
# Gender distribution
  round(prop.table(table(full_data$Gender)),2)
# Age range
  summary(full_data$Age)
  mean(full_data$Age)
  sd(full_data$Age)
# Highest Degree  
table(full_data$HighestDegree)
# STEM major
round(prop.table(table(full_data$STEMMajor)),2)
# First gen & immigrant
round(prop.table(table(full_data$FirstGen)),2)
round(prop.table(table(full_data$`1st_immigrant`)),3)


# Means/SDs ---------------------------------------------------------------

# Summary stats - PRESCREEN
crosstable(full_data, 
           cols = c(Gender, Age, GPA, HighestDegree, STEMMajor, 
                    FirstGen, `1st_immigrant`, `2nd_immigrant`), 
           num_digits = 2,
           showNA =  "no") |> 
  as_flextable()

crosstable(full_data, 
           cols = c(M_Approach, M_Avoidance, P_Approach, P_Avoidance, 
                    Math_ID, Reading_ID,
                    Racial_ID, Gender_ID), 
           by = Condition,
           num_digits = 2,
           showNA =  "no") |> 
  as_flextable()

# Summary stats - PART 2

crosstable(full_data, 
           cols = c(Score, AGQ_approach, AGQ_avoidance,
                    RIT, Belonging, Centrality), 
           by = Condition,
           num_digits = 2,
           showNA =  "no") |> 
  as_flextable()


# ANOVA ------------------------------------------------------------------

# Primary ANOVA (condition x score)
summary(aov(Score ~ Condition, data = full_data))
  
  ggplot(data = full_data, aes(x = Condition, y = Score)) +
    geom_boxplot() +
    theme_minimal()
  
# Secondary ANOVA (condition x DVs)
summary(aov(AGQ_approach ~ Condition, data = full_data))
summary(aov(AGQ_avoidance ~ Condition, data = full_data))
  
  ggplot(data = full_data, aes(x = Condition, y = AGQ_approach)) +
    geom_boxplot() +
    theme_minimal()  
  
  ggplot(data = full_data, aes(x = Condition, y = AGQ_avoidance)) +
    geom_boxplot() +
    theme_minimal()  
  
summary(aov(Pre_Expectation ~ Condition, data = full_data))
summary(aov(Post_Expectation ~ Condition, data = full_data))

  ggplot(data = full_data, aes(x = Condition, y = Pre_Expectation)) +
    geom_boxplot() +
    theme_minimal()  
  
summary(aov(STAI_pre ~ Condition, data = full_data))
summary(aov(STAI_post ~ Condition, data = full_data))

  ggplot(data = full_data, aes(x = Condition, y = STAI_pre)) +
    geom_boxplot() +
    theme_minimal()  
  
summary(aov(RIT ~ Condition, data = full_data))
summary(aov(Belonging ~ Condition, data = full_data))
summary(aov(Centrality ~ Condition, data = full_data))
  
  ggplot(data = full_data, aes(x = Condition, y = Centrality)) +
    geom_boxplot() +
    theme_minimal()  
  

# T-tests for part 1/2 comparisons ----------------------------------------

t.test(full_data$P_Approach, full_data$AGQ_approach, paired = TRUE)
t.test(full_data$P_Avoidance, full_data$AGQ_avoidance, paired = TRUE)
t.test(full_data$Racial_ID, full_data$Centrality, paired = TRUE)


# Correlations ------------------------------------------------------------

correlations_data <- full_data |> 
  select(Score, Pre_Approach = P_Approach, Pre_Avoidance = P_Avoidance,
         Pre_Centrality = Racial_ID, 
         Approach = AGQ_approach, Avoidance = AGQ_avoidance,
         RIT, Belonging, Centrality)
rquery.cormat(correlations_data)

# Extra -------------------------------------------------------------------
  
  # T-tests
  control_only <- subset(full_data, full_data$Condition == "Control")
  implicit_only <- subset(full_data, full_data$Condition == "Implicit")
  explicit_only <- subset(full_data, full_data$Condition == "Explicit")
  
  t.test(implicit_only$Score, explicit_only$Score, paired = TRUE)
  
  
# summary(aov(Score ~ Condition + Condition*Pre_Expectation, data = full_data))
  
  ggplot(data = full_data, aes(x = Pre_Expectation, y = Score)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    theme_minimal()+
    facet_wrap(~ Condition)
  
# Are the conditions leading to differences in pre-expectation? 
  
  ggplot(data = full_data, aes(x = Condition, y = Pre_Expectation)) +
    geom_boxplot() +
    theme_minimal()
  
# Pairwise comparisons  
  expectation_test <- aov(Pre_Expectation ~ Condition, data = full_data)
  Tukey <- TukeyHSD(x=expectation_test, conf.level=0.95) 
  plot(Tukey, las=1, asp = 0.5, cex.axis = 0.5) 
  title(main="Pre-Expectation", 
        col.main="red", cex.main=0.8, line = 1)
  

  
  ggplot(data = full_data, aes(x = Pre_Expectation, y = Score)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    theme_minimal()
  
  
  
  
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
  
  

  
  
  
  
  



