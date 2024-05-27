# Pick 1 winner for every 100 participants to receive a $100 gift card

# Figure out which people are eligible

participants <- read_csv("data/data_0326.csv")

# Extract ID from either ProlificID or PROLIFIC_PID columns
participants <- participants |> 
  mutate(ProlificID = coalesce(ProlificID, PROLIFIC_PID))

bonus_pool <- participants |> 
  filter(# Attention check
          Q2...16 == "3" & 
           # Finished survey
           Finished == "1" & 
            # Include data 
            debrief_include_data == "1" &
            # Manipulation check
               !((mc_task != "1" &
                    mc_mathability != "1" &
                    mc_diagnostic != "1"))) |> 
  select(ProlificID) 

# Randomly select 2 winners
set.seed(2024)
winner <- bonus_pool |> sample_n(2)
print(winner)