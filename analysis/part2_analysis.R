# STUDY 1 ANALYSIS

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

part1_df <- read_csv("data/cleaned_part1.csv")
part2_df <- read_csv("data/cleaned_part2.csv")
full_data <- read_csv("data/cleaned_full.csv")

#Participants who passed all 3 MC questions
MC_data <- full_data |> 
              filter((mc_task == "1" &
                      mc_mathability == "1" &
                      mc_diagnostic == "1"))
table(MC_data$Condition)

# Check for outliers ------------------------------------------------------

# Are there any scores 3sd away from mean? NO
#full_data$Score[abs(full_data$Score - mean(full_data$Score)) > 3 * sd(full_data$Score)]

# Check for outliers in duration
#duration_outliers <- which(abs(full_data$`Duration (in seconds)` - mean(full_data$`Duration (in seconds)`)) > 3 * sd(full_data$`Duration (in seconds)`))


full_data$`Duration (in seconds)`

ggplot(data = full_data, aes(x = `Duration (in seconds)`, y = Score)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_minimal()+
  facet_wrap(~ Condition)

summary(lm(Score ~ `Duration (in seconds)`, data = full_data))  


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
baseline_stats <- full_data |> 
                  rename(`First-gen student` = FirstGen,
                         `First-gen immigrant` = `1st_immigrant`,
                         `Second-gen immigrant` = `2nd_immigrant`,
                         `Performance Approach` = P_Approach,
                         `Performance Avoidance` = P_Avoidance,
                         `Math Identification` = Math_ID,
                         `Reading Identification` = Reading_ID,
                         `Gender Identification` = Gender_ID,
                         `Racial Centrality` = Racial_ID)

# Tables for manuscript  
crosstable(baseline_stats, 
           cols = c(HighestDegree, STEMMajor, 
                    `First-gen student`, `First-gen immigrant`, 
                    `Second-gen immigrant`, GPA), 
           num_digits = 2,
           showNA =  "no") |> 
  as_flextable()

crosstable(baseline_stats, 
           cols = c(`Performance Approach`, `Performance Avoidance`, 
                    `Math Identification`, `Reading Identification`,
                    `Gender Identification`, `Racial Centrality`), 
           num_digits = 2,
           showNA =  "no") |> 
  as_flextable()

# Summary stats - PART 2

part2_stats <- MC_data |> 
  rename(`Performance Approach` = AGQ_approach,
         `Performance Avoidance` = AGQ_avoidance,
         `Pre-Task Expectation` = Pre_Expectation,
         `Post-Task Expectation` = Post_Expectation,
         `Pre-Task Anxiety` = STAI_pre, 
         `Racial Identity Threat` = RIT,
         `Racial Centrality` = Centrality)

crosstable(part2_stats, 
           cols = c(Score, `Performance Approach`, 
                    `Performance Avoidance`,
                    `Pre-Task Expectation`, 
                    `Pre-Task Anxiety`,
                    `Racial Identity Threat`, `Racial Centrality`), 
           by = Condition,
           num_digits = 2,
           showNA =  "no") |> 
  as_flextable()


# ANOVA ------------------------------------------------------------------

# Reorder the "Condition" factor variable
order <- c("Control", "Implicit", "Explicit")
full_data$Condition <- factor(full_data$Condition, levels = order)

# Primary ANOVA (condition x score)
summary(aov(Score ~ Condition, data = full_data))

# Boxplot  
primary_boxplot <- ggplot(data = full_data, aes(x = Condition, y = Score)) +
                     geom_boxplot() +
                     labs(x = "Stereotype Activation",
                          y = "Number Correct on Spatial Rotation Task") +
                     theme_minimal() +
                     scale_y_continuous(breaks = seq(0, ceiling(max(full_data$Score)), by = 3))
       
 primary_boxplot + theme_apa()
 
 summary_data <- full_data |> 
   group_by(Condition) |> 
   summarise(mean_score = mean(Score),
             sd_score = sd(Score), 
             # Standard error
             se_score = sd_score / sqrt(n()))  

 
# Barplot with error bars
 primary_barplot <- ggplot(data = summary_data, aes(x = Condition, y = mean_score)) +
   geom_bar(stat = "identity", width = 0.6) + 
   labs(x = "Stereotype Activation",
        y = "Number Correct on Spatial Rotation Task") +
   geom_errorbar(aes(ymin = mean_score - se_score, 
                     ymax = mean_score + se_score), 
                 width = 0.1) +
   scale_y_continuous(breaks = seq(0, ceiling(max(summary_data$mean_score)), 
                                   by = 2))
   primary_barplot + theme_apa()
 
  
# Pairwise comparisons  
primary_test <- aov(Score ~ Condition, data = full_data)
primary_Tukey <- TukeyHSD(x=primary_test, conf.level=0.95) 
  plot(primary_Tukey, las=1, asp = 0.5, cex.axis = 0.5) 
  
# Contrasts
  control_only <- subset(full_data, full_data$Condition == "Control")
  implicit_only <- subset(full_data, full_data$Condition == "Implicit")
  explicit_only <- subset(full_data, full_data$Condition == "Explicit")
  
t.test(implicit_only$Score, control_only$Score)
t.test(explicit_only$Score, control_only$Score)

primary_contrast <- nice_contrasts(
                        response = "Score",
                        group = "Condition",
                        data = full_data
                      )

contrast_table <- nice_table(primary_contrast)


# Secondary ANOVA (condition x DVs)
summary(aov(AGQ_approach ~ Condition, data = full_data))
summary(aov(AGQ_avoidance ~ Condition, data = full_data))
summary(lm(Score ~ AGQ_approach, data = full_data))
summary(lm(AGQ_approach ~ Condition, data = full_data))  
summary(lm(Score ~ AGQ_avoidance, data = full_data))  

ggplot(data = full_data, aes(x = Condition, y = AGQ_approach)) +
  geom_boxplot() +
  theme_minimal()  

ggplot(data = full_data, aes(x = Condition, y = AGQ_avoidance)) +
  geom_boxplot() +
  theme_minimal()  
  
# Interactions!!!
summary(lm(Score ~ AGQ_approach*Condition, data = full_data))
  
ggplot(data = full_data, aes(x = AGQ_approach, y = Score)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_minimal()+
  facet_wrap(~ Condition)

summary(lm(Score ~ AGQ_avoidance*Condition, data = full_data))  

ggplot(data = full_data, aes(x = AGQ_avoidance, y = Score)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_minimal()+
  facet_wrap(~ Condition)


summary(aov(STAI_pre ~ Condition, data = full_data))

ggplot(data = full_data, aes(x = Condition, y = STAI_pre)) +
  geom_boxplot() +
  theme_minimal()  

nice_contrasts(response = "STAI_pre",
               group = "Condition",
               data = full_data)

crosstable(full_data, 
           cols = c(Score, STAI_pre, Math_abilities1, Math_abilities2,
                    task_opinion,
                    Pressure), 
           by = Condition,
           num_digits = 2,
           showNA =  "no") |> 
  as_flextable()

summary(aov(Math_abilities1 ~ Condition, data = full_data))

summary(aov(STAI_post ~ Condition, data = full_data))

ggplot(data = full_data, aes(x = Condition, y = STAI_post)) +
  geom_boxplot() +
  theme_minimal()  

nice_contrasts(response = "STAI_post",
               group = "Condition",
               data = full_data)

summary(aov(Pre_Expectation ~ Condition, data = MC_data))
summary(aov(Post_Expectation ~ Condition, data = MC_data))

  ggplot(data = full_data, aes(x = Condition, y = Post_Expectation)) +
    geom_boxplot() +
    theme_minimal()  
  
  nice_contrasts(response = "Pre_Expectation",
                  group = "Condition",
                  data = full_data)
  

  ggplot(data = filter(full_data, STEMMajor =="STEM"), aes(x = Condition, y = Centrality)) +
    geom_boxplot() +
    theme_minimal()  
  
  ggplot(data = full_data, aes(x = Condition, y = Centrality)) +
    geom_boxplot() +
    theme_minimal() 
  
# Between condition differences on additional variables
  
summary(aov(RIT ~ Condition, data = full_data))
summary(aov(Pre_Expectation ~ Condition, data = full_data))
summary(aov(Centrality ~ Condition, data = full_data))
  
Centrality_condition <- ggplot(data = full_data, aes(x = Condition, y = Centrality)) +
    geom_boxplot() +
  labs(x = "Stereotype Activation",
       y = "Racial Centrality") +
    theme_minimal()  
  
RIT_condition <-  ggplot(data = full_data, aes(x = Condition, y = RIT)) +
    geom_boxplot() +
    labs(x = "Stereotype Activation",
         y = "Racial Identity Threat") +
    theme_minimal() 

STAI_condition <-  ggplot(data = full_data, aes(x = Condition, y = STAI_pre)) +
  geom_boxplot() +
  labs(x = "Stereotype Activation",
       y = "Pre-Task Anxiety") +
  theme_minimal() 

# Combine plots
ggarrange(Centrality_condition, RIT_condition, STAI_condition,
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1,
          common.legend = TRUE, legend = "bottom") 

  

# T-tests for part 1/2 comparisons ----------------------------------------

# Create columns for differences
  full_data <-  full_data |> 
    mutate(Approach_diff = P_Approach - AGQ_approach,
           Avoidance_diff = P_Avoidance - AGQ_avoidance,
           Centrality_diff = Racial_ID - Centrality)

# Average difference by condition
aggregate(Approach_diff ~ Condition, data = full_data, FUN = mean)
aggregate(Avoidance_diff ~ Condition, data = full_data, FUN = mean)

# Are the differences significant? 
summary(aov(Approach_diff ~ Condition, data = full_data))  

t.test(full_data$P_Approach, full_data$AGQ_approach, paired = TRUE) #significantly higher part 1
t.test(full_data$P_Avoidance, full_data$AGQ_avoidance, paired = TRUE) #no diff
t.test(full_data$Racial_ID, full_data$Centrality, paired = TRUE) #significantly higher part 1

long_Approach <- full_data %>%
  rename(Part1_Approach = P_Approach,
         Part2_Approach = AGQ_approach) %>%
  pivot_longer(cols = c(Part1_Approach, Part2_Approach), 
               names_to = "Part", values_to = "Value")

ggplot(long_Approach, aes(x = Condition, y = Value, fill = Part)) +
  geom_boxplot() +
  labs(title = "Performance Approach",
       x = "Condition",
       y = "Value",
       fill = "Part") +
  theme_minimal() +
  guides(fill = guide_legend(title = NULL))

long_Avoidance <- full_data %>%
  rename(Part1_Avoidance = P_Avoidance,
         Part2_Avoidance = AGQ_avoidance) %>%
  pivot_longer(cols = c(Part1_Avoidance, Part2_Avoidance), 
               names_to = "Part", values_to = "Value")

ggplot(long_Avoidance, aes(x = Condition, y = Value, fill = Part)) +
  geom_boxplot() +
  labs(title = "Performance Avoidance",
       x = "Condition",
       y = "Value",
       fill = "Part") +
  theme_minimal() +
  guides(fill = guide_legend(title = NULL))

long_Centrality <- full_data %>%
  rename(Part1_Centrality = Racial_ID,
         Part2_Centrality = Centrality) %>%
  pivot_longer(cols = c(Part1_Centrality, Part2_Centrality), 
               names_to = "Part", values_to = "Value")

ggplot(long_Centrality, aes(x = Condition, y = Value, fill = Part)) +
  geom_boxplot() +
  labs(title = "Racial Centrality",
       x = "Condition",
       y = "Value",
       fill = "Part") +
  theme_minimal() +
  guides(fill = guide_legend(title = NULL))



# Correlation function ----------------------------------------------------

rquery.cormat<-function(x,
                        type=c('lower', 'upper', 'full', 'flatten'),
                        graph=TRUE,
                        graphType=c("correlogram", "heatmap"),
                        col=NULL, ...)
{
  library(corrplot)
  # Helper functions
  #+++++++++++++++++
  # Compute the matrix of correlation p-values
  cor.pmat <- function(x, ...) {
    mat <- as.matrix(x)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  # Get lower triangle of the matrix
  getLower.tri<-function(mat){
    upper<-mat
    upper[upper.tri(mat)]<-""
    mat<-as.data.frame(upper)
    mat
  }
  # Get upper triangle of the matrix
  getUpper.tri<-function(mat){
    lt<-mat
    lt[lower.tri(mat)]<-""
    mat<-as.data.frame(lt)
    mat
  }
  # Get flatten matrix
  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  =(cormat)[ut],
      p = pmat[ut]
    )
  }
  # Define color
  if (is.null(col)) {
    col <- colorRampPalette(
      c("#67001F", "#B2182B", "#D6604D", "#F4A582",
        "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
        "#4393C3", "#2166AC", "#053061"))(200)
    col<-rev(col)
  }
  
  # Correlation matrix
  cormat<-signif(cor(x, use = "complete.obs", ...),2)
  pmat<-signif(cor.pmat(x, ...),2)
  # Reorder correlation matrix
  ord<-corrMatOrder(cormat, order="hclust")
  cormat<-cormat[ord, ord]
  pmat<-pmat[ord, ord]
  # Replace correlation coeff by symbols
  sym<-symnum(cormat, abbr.colnames=FALSE)
  # Correlogram
  if(graph & graphType[1]=="correlogram"){
    corrplot(cormat, type=ifelse(type[1]=="flatten", "lower", type[1]),
             tl.col="black", tl.srt=45,col=col,...)
  }
  else if(graphType[1]=="heatmap")
    heatmap(cormat, col=col, symm=TRUE)
  # Get lower/upper triangle
  if(type[1]=="lower"){
    cormat<-getLower.tri(cormat)
    pmat<-getLower.tri(pmat)
  }
  else if(type[1]=="upper"){
    cormat<-getUpper.tri(cormat)
    pmat<-getUpper.tri(pmat)
    sym=t(sym)
  }
  else if(type[1]=="flatten"){
    cormat<-flattenCorrMatrix(cormat, pmat)
    pmat=NULL
    sym=NULL
  }
  list(r=cormat, p=pmat, sym=sym)
}

# Correlations ------------------------------------------------------------

correlations_data <- full_data |> 
  select(Score, Pre_Approach = P_Approach, Pre_Avoidance = P_Avoidance,
         Pre_Centrality = Racial_ID, 
         Approach = AGQ_approach, Avoidance = AGQ_avoidance,
         RIT, Belonging, Centrality, Pre_Expectation, Post_Expectation, Math_ID,
         STAI_pre, STAI_post)
rquery.cormat(correlations_data)

library(xtable)
correlations_data <- full_data |> 
  select(Score, Approach = AGQ_approach, Avoidance = AGQ_avoidance,
         RIT, Belonging, Centrality, 
         Pre_Expectation, Post_Expectation, Math_ID,
         STAI_pre, STAI_post)
cor <- round(cor(correlations_data), 2)

# cor matrix function -----------------------------------------------------
library(Hmisc)
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

  
cor_output <- corstars(correlations_data, result = "html")
write(cor_output, file = "correlation_table.html")


# Significant predictors --------------------------------------------------

summary(lm(Score ~ Pre_Expectation, full_data))
summary(lm(Score ~ Post_Expectation, full_data))
summary(lm(Score ~ STAI_pre, full_data))

###

ggplot(data = full_data, aes(x = STEMMajor, y = Score)) +
  geom_boxplot() +
  theme_minimal() +
  facet_wrap(~ Condition)

ggplot(data = full_data, aes(x = Racial_ID)) +
  geom_density()+
  geom_vline(aes(xintercept = mean(Racial_ID)), color = "red", linetype = "dashed", size = 1) 
  

ggplot(data = full_data, aes(x = Math_ID)) +
  geom_density() 

ggplot(data = full_data, aes(x = Pre_Expectation)) +
  geom_density()+
  geom_vline(aes(xintercept = mean(Pre_Expectation)), color = "red", linetype = "dashed", size = 1) 



# Split the data ----------------------------------------------------------

# STEM ONLY - yes AGQ
summary(aov(Score ~ Condition, data = filter(full_data, STEMMajor =="STEM")))
summary(lm(Score ~ AGQ_approach, data = filter(full_data, STEMMajor =="STEM")))
summary(lm(Score ~ AGQ_avoidance, data = filter(full_data, STEMMajor =="STEM")))

cor(filter(full_data, STEMMajor =="STEM")$AGQ_avoidance, filter(full_data, STEMMajor =="STEM")$Score)
summary(lm(Score ~ AGQ_approach, data = full_data))
# GENDER - none

summary(aov(Score ~ Condition, data = filter(full_data, Gender =="Woman")))
summary(lm(Score ~ P_Approach, data = filter(full_data, Gender =="Woman")))
summary(lm(Score ~ P_Avoidance, data = filter(full_data, Gender =="Woman")))

# CENTRALITY

#Split dataset into low/high centrality (if less than 3.3, then low)
# median = 3.3

ggplot(data = full_data, aes(x = Condition, y = Centrality)) +
  geom_boxplot() +
  theme_bw()

full_data$Centrality_F <- ifelse(full_data$Centrality < 3.3, "Low", "High")

low_Centrality <- subset(full_data, full_data$Centrality_F == "Low")
high_Centrality <-subset(full_data, full_data$Centrality_F == "High")

summary(aov(Score ~ Condition, data = high_Centrality))

t.test(low_Centrality$Score, high_Centrality$Score) 

ggplot(data = full_data, aes(x = Condition, y = Score, fill = Centrality_F)) +
  geom_boxplot() +
  theme_bw()

ggplot(data = full_data, aes(x = Condition, y = AGQ_approach, fill = Centrality_F)) +
  geom_boxplot() +
  theme_bw()
# Low centrality folks are significantly less likely to adopt an approach mindset
t.test(low_Centrality$AGQ_approach, high_Centrality$AGQ_approach) 

ggplot(data = full_data, aes(x = Condition, y = AGQ_avoidance, fill = Centrality_F)) +
  geom_boxplot() +
  theme_bw()

# And likely to adopt an avoidance
t.test(low_Centrality$AGQ_avoidance, high_Centrality$AGQ_avoidance) 

# Additional variables ----------------------------------------------------

# I performed as well as I could on the task
summary(full_data$Pressure)
summary(aov(Pressure ~ Condition, data = full_data))

ggplot(data = full_data, aes(x = Condition, y = Pressure)) +
  geom_boxplot() +
  theme_minimal()  

ggplot(data = full_data, aes(x = Pressure, y = Score)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_minimal()+
  facet_wrap(~ Condition)

crosstable(full_data, 
           cols = c(Score, Pressure, Pre_Expectation), 
           by = Condition,
           num_digits = 2,
           showNA =  "no") |> 
  as_flextable()

# EXPECTATION
# Do expectations differ by condition? No
summary(aov(Pre_Expectation ~ Condition, data = full_data))
ggplot(data = full_data, aes(x = Pre_Expectation, y = Score)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_minimal()+
  facet_wrap(~ Condition)
summary(aov(Post_Expectation ~ Condition, data = full_data))
ggplot(data = full_data, aes(x = Post_Expectation, y = Score)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_minimal()+
  facet_wrap(~ Condition)

# Expectations independently lead to higher scores
summary(lm(Score ~ Pre_Expectation, full_data)) #significant
summary(lm(Score ~ Post_Expectation, full_data)) #significant + stronger

# Significant interaction
# effect of pre-test expectation on score depends on the condition
summary(aov(STAI_pre ~ Condition, data = full_data))

cor(full_data$Pre_Expectation, full_data$AGQ_approach)


# MATH ID
# Low math ID lead to lower scores? No
summary(lm(Score ~ Math_ID, full_data)) 
# Controlling for math ID - no diff
summary(aov(Score ~ Condition + Math_ID, data = full_data))

# Subset only those high in math ID

full_data$Math_F <- ifelse(full_data$Math_ID < 4, "Low", "High")
MC_data$MathF <- ifelse(MC_data$Math_ID < 4, "Low", "High")

low_Math <- subset(full_data, full_data$Math_F == "Low")
high_Math <-subset(full_data, full_data$Math_F == "High")

summary(aov(Score ~ Condition, data = high_Math))
t.test(low_Math$Score, high_Math$Score) 

# No significant difference in scores between high and low math ID

# Correlation with mindsets
cor(full_data$Math_ID, full_data$AGQ_approach)
cor(low_Math$Math_ID, low_Math$AGQ_approach)
cor(high_Math$Math_ID, high_Math$AGQ_approach)

# COMPARISON

ggplot(data = full_data, aes(x = Comparison, y = Score)) +
  geom_boxplot()

crosstable(full_data, 
           cols = c(Score, Comparison), 
           by = Condition,
           num_digits = 2,
           showNA =  "no") |> 
  as_flextable()

crosstable(full_data, 
           cols = c(Score, AGQ_approach, AGQ_avoidance, Pre_Expectation), 
           by = Comparison,
           num_digits = 2,
           showNA =  "no") |> 
  as_flextable()

ggplot(data = full_data, aes(x = Comparison, y = AGQ_approach)) +
  geom_boxplot()

ggplot(data = full_data, aes(x = Comparison, y = AGQ_avoidance)) +
  geom_boxplot()

# MASTERY

summary(aov(M_Approach ~ Condition, data = full_data))
summary(aov(M_Avoidance ~ Condition, data = full_data))
summary(lm(Score ~ M_Approach, data = full_data))
summary(lm(Score ~ M_Avoidance, data = full_data))  

ggplot(data = full_data, aes(x = Condition, y = M_Approach)) +
  geom_boxplot() +
  theme_minimal()  

ggplot(data = full_data, aes(x = Condition, y = M_Avoidance)) +
  geom_boxplot() +
  theme_minimal() 

# STRESS/OPINION
crosstable(full_data, 
           cols = c(Score, task_stressful, 
                    task_opinion, Math_abilities1,
                    Math_abilities2), 
           by = Condition,
           num_digits = 2,
           showNA =  "no") |> 
  as_flextable()


# Extra -------------------------------------------------------------------

summary(lm(Score ~ Condition*Pre_Expectation + AGQ_approach, data = full_data))
  
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
  

  
  



