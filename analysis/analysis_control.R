# STUDY 1 ANALYSIS WITH SEPARATE CONTROL DATA

# Install packages --------------------------------------------------------

library(tidyverse)
library(crosstable)
library(multcompView)
library(corrplot)
library(psych)
library(broom)
library(rempsyc)
library(bootES)
library(modelbased)
library(jtools)
library(ggpubr)

# Load data ---------------------------------------------------------------

full_data <- read_csv("data/cleaned_full.csv")
full_control <- read_csv("data/cleaned_control.csv")
full_withoutcontrol <- full_data |> filter(Condition != "Control")

# Control EDA -------------------------------------------------------------
# No scores 3sd away from mean
full_control$Score[abs(full_control$Score - mean(full_control$Score)) > 3 * sd(full_control$Score)]
# No outliers in duration
which(abs(full_control$`Duration (in seconds)` - mean(full_control$`Duration (in seconds)`)) > 3 * sd(full_control$`Duration (in seconds)`))
# Score distribution
ggplot(data = full_control, aes(x = Score)) +
  geom_histogram() +
  labs(title = "Distribution of Performance - CONTROL ONLY") +
  theme_bw() +
  # Add vertical line for mean
  geom_vline(aes(xintercept = mean(Score)), color = "red", linetype = "dashed", size = 1) +
  # Annotate with numeric mean value
  annotate("text", x = mean(full_data$Score), y = 25, 
           label = paste("Mean =", round(mean(full_control$Score), 2)), vjust = 1.5, color = "red")
# Gender distribution
round(prop.table(table(full_control$Gender)),2)
# Age range
summary(full_control$Age)

# Create new combined df --------------------------------------------------

withoutcontrol <- full_withoutcontrol |> select(Condition, Score, AGQ_approach, AGQ_avoidance,
                                                Pre_Expectation, Post_Expectation, STAI_pre, RIT, 
                                                Centrality)
control <- full_control |> select(Condition, Score, AGQ_approach, AGQ_avoidance,
                                  Pre_Expectation, Post_Expectation, STAI_pre, RIT, 
                                  Centrality)
primary_merge <- rbind(withoutcontrol, control)

order <- c("Control", "Implicit", "Explicit")
primary_merge$Condition <- factor(primary_merge$Condition, levels = order)

# Primary ANOVA (condition x score)
summary(aov(Score ~ Condition, data = primary_merge))

# Boxplot  
primary_boxplot <- ggplot(data = primary_merge, aes(x = Condition, y = Score)) +
  geom_boxplot() +
  labs(x = "Stereotype Activation",
       y = "Number Correct on Spatial Rotation Task") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, ceiling(max(primary_merge$Score)), by = 3))

primary_merge |> 
  group_by(Condition) |> 
  summarise(mean_score = mean(Score),
            sd_score = sd(Score))

# Pairwise comparisons  
primary_test <- aov(Score ~ Condition, data = primary_merge)
primary_Tukey <- TukeyHSD(x=primary_test, conf.level=0.95) 
plot(primary_Tukey, las=1, asp = 0.5, cex.axis = 0.5) 

nice_contrasts(
  response = "Score",
  group = "Condition",
  data = primary_merge
)

# Secondary ANOVA (condition x DVs)
summary(aov(AGQ_approach ~ Condition, data = primary_merge))
summary(aov(AGQ_avoidance ~ Condition, data = primary_merge))

ggplot(data = primary_merge, aes(x = Condition, y = AGQ_approach)) +
  geom_boxplot() +
  theme_minimal()  

ggplot(data = primary_merge, aes(x = Condition, y = AGQ_avoidance)) +
  geom_boxplot() +
  theme_minimal()  

nice_contrasts(
  response = "AGQ_approach",
  group = "Condition",
  data = primary_merge
)

# Descriptive tables

crosstable(primary_merge, 
           cols = c(Score, AGQ_approach, AGQ_avoidance,
                    Pre_Expectation, Post_Expectation, STAI_pre, RIT, 
                    Centrality), 
           by = Condition,
           num_digits = 2,
           showNA =  "no") |> 
  as_flextable()






