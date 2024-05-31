# Clean Part 1 & Part 2 raw datasets to use for analysis

library(tidyverse)

# Clean part 1 --------------------------------------------------------------

# CSV file imported after filtering out Qualtrics duplicates + bots
old_prescreen <- read_csv("data/part1_0326.csv") 
part1_raw <- read_csv("data/part1_0530.csv") 

filter_part1 <- part1_raw |> 
  filter(Finished == "1" & #complete responses
           debrief_include_data == "1") |>  #agreed to include data
  dplyr::select(23,5,24,26:30,32:66)
  
clean_part1 <- 
  filter_part1 |> 
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

# Calculate baseline averages
part1_avg <- clean_part1 |> 
  rowwise() |> 
  mutate(M_Approach = round(mean(c_across(c(14,16,20)), na.rm = TRUE), 3),
         M_Avoidance = round(mean(c_across(c(18,22,24)), na.rm = TRUE), 3),
         P_Approach = round(mean(c_across(c(15,17,21)), na.rm = TRUE), 3),
         P_Avoidance = round(mean(c_across(c(19,23,25)), na.rm = TRUE), 3),
         Approach = round(mean(c_across(c(14,16,20,15,17,21)), na.rm = TRUE), 3),
         Avoidance = round(mean(c_across(c(18,22,24,19,23,25)), na.rm = TRUE), 3),
         Math_ID = round(mean(c_across(c(27,29,31,33,35,37)), na.rm = TRUE), 3),
         Reading_ID = round(mean(c_across(c(26,28,30,32,34,36)), na.rm = TRUE), 3),
         Racial_ID = round(mean(c_across(38:40), na.rm = TRUE), 3),
         Gender_ID = round(mean(c_across(41:43), na.rm = TRUE), 3)
  )

part1_data <- part1_avg |> 
                dplyr::select(c(1:13,44:53))
# n = 303
#write.csv(part1_data,"data/cleaned_part1.csv", row.names = FALSE)

# Checking participants in part 2 --------------------------------------

part2_raw <- read_csv("data/part2_0530.csv") #n = 289

# Filter out incomplete surveys + those that don't want their data included
filter_part2 <- part2_raw |> 
  filter(Finished == "1" & 
           # Include data 
           debrief_include_data == "1")
# n = 247

# Extract IDs from either column
filter_part2 <- filter_part2 |> 
  mutate(ProlificID = coalesce(ProlificID, PROLIFIC_PID))

#print(paste("Number of participants BEFORE MC:", nrow(part2_data)))

# Manipulation check
passed_mc <- filter_part2 |>  
  filter(!((mc_task != "1" &
              mc_mathability != "1" &
              mc_diagnostic != "1")))

#print(paste("Number of participants AFTER MC:", nrow(data)))

# Clean part 2 -------------------------------------------------------

# Turn empty strings to na's
data <- passed_mc |> mutate_all(~na_if(., ""))

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
part2_avg <- clean_data |>
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
  )

part2_data <- part2_avg |> 
  dplyr::select(Condition, Score, AGQ_approach, AGQ_avoidance,Pre_Expectation,
                Post_Expectation, RIT, Belonging, Centrality, STAI_pre, STAI_post, 
                Control_common, Control_true, Blatant_common, Blatant_true, Age,
                mc_task, mc_mathability, mc_diagnostic,
                ProlificID)

# n = 239
#write.csv(part2_data,"data/cleaned_part2.csv", row.names = FALSE)


# Check for participants who completed both sessions ----------------------

# How many participants completed part 1 but not part 2 (n = 73)
not_in_part2 <- which(!(part1_data$ProlificID %in% part2_data$ProlificID))
# How many participants completed part 2 but not part 1 (n = 8)
not_in_part1 <- which(!(part2_data$ProlificID %in% part1_data$ProlificID))
# Which participants completed both parts? n = 228
participants <- intersect(part1_data$ProlificID, part2_data$ProlificID)
# Calculate attrition
n_part1 <- nrow(part1_data)
n_part2 <- length(which(part2_data$ProlificID %in% part1_data$ProlificID))
attrition <- ((n_part1 - n_part2) / n_part1) * 100

# Filter to include only participants who completed both parts
part1_final <- part1_data |> filter(ProlificID %in% participants)
part2_final <- part2_data |> filter(ProlificID %in% participants)

# Combine both sessions ---------------------------------------------------

full_data <- merge(part1_final, part2_final, by = "ProlificID", all = FALSE)

#write.csv(full_data,"data/cleaned_full.csv", row.names = FALSE)
