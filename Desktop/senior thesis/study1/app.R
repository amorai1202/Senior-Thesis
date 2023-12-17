# EDA SHINY APP

# Install packages --------------------------------------------------------

library(shiny)
library(plotly)
library(DT)
library(tidyverse)
library(data.table)
library(rsconnect)

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

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Study 1 EDA (as of 12/17)"),
  
  sidebarLayout(
    sidebarPanel(
      # Choose which file
      selectInput("select", label = "Select data", 
                  choices = list("Combined" = 1, "Prolific" = 2, "CMU" = 3),
                  selected = 1),
      checkboxInput("mc", "Passed Manipulation Check", FALSE),
      checkboxInput("asian", "Asians Only", FALSE),
      checkboxInput("ug", "Undergrads Only", FALSE),
      uiOutput("samplesize"),
      tags$hr(),
      uiOutput("var1"),
      tableOutput("tableVar1"),
      uiOutput("var2"),
      uiOutput("facet"),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", 
                 tags$div(
                   style = "padding-top: 20px",  
                   plotlyOutput("plot"))),
        tabPanel(
          "Data Preview",
          tags$div(
            style = "padding-top: 20px",  
            DTOutput("tablePreview"))
        )
      )
    )
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  dataInput <- reactiveVal()
  
  # Select dataset
  observeEvent(input$select, {
    if (input$select == 1) {
      all_data <- cleaned_combined
      dataInput(all_data)
    } else if (input$select == 2) {
      all_data <- cleaned_prolific
      dataInput(all_data)
    } else {
      all_data <- cleaned_cmu
      dataInput(all_data)
    }
    
    # Clear checkbox inputs below
    updateCheckboxInput(session, "mc", value = FALSE)
    updateCheckboxInput(session, "asian", value = FALSE)
    updateCheckboxInput(session, "ug", value = FALSE)
  })
  
  # Filter out participants who answered all 3 MC questions wrong
  observeEvent(input$mc, {
    req(dataInput())
    if (input$mc == TRUE) {
      filter_data <- dataInput() %>% 
        filter(!((mc_task != "1" &
                    mc_mathability != "1" &
                    mc_diagnostic != "1")))    
      dataInput(filter_data) 
    } else {
      if (input$select == 1) {
        all_data <- cleaned_combined
        dataInput(all_data)
      } else if (input$select == 2) {
        all_data <- cleaned_prolific
        dataInput(all_data)
      } else {
        all_data <- cleaned_cmu
        dataInput(all_data)
      }
    }
  })
  
  #Filter out non-Asians
  observeEvent(input$asian, {
    req(dataInput())
    if (input$asian == TRUE) {
      filter_data <- dataInput() %>%
        filter(str_detect(Race, "2"))
      dataInput(filter_data)
    } else {
      if (input$select == 1) {
        all_data <- cleaned_combined
        dataInput(all_data)
      } else if (input$select == 2) {
        all_data <- cleaned_prolific
        dataInput(all_data)
      } else {
        all_data <- cleaned_cmu
        dataInput(all_data)
      }
    }
  })
  
  #Filter undergrads
  observeEvent(input$ug, {
    req(dataInput())
    if (input$ug == TRUE) {
      filter_data <- dataInput() %>%
        filter(StudentStatus == "Undergraduate")
      dataInput(filter_data)
    } else {
      if (input$select == 1) {
        all_data <- cleaned_combined
        dataInput(all_data)
      } else if (input$select == 2) {
        all_data <- cleaned_prolific
        dataInput(all_data)
      } else {
        all_data <- cleaned_cmu
        dataInput(all_data)
      }
    }
  })
  
  #Display sample size
  output$samplesize <- renderPrint({
    req(dataInput())
    cat("Sample size =", nrow(dataInput()), "\n")
  })
  
  #Choose variables
  output$var1 <- renderUI({
    req(dataInput())
    selectInput("selectedVar1", "Select x variable:", 
                choices = c("Condition",
                            "Score",
                            "Pre_Expectation",
                            "Post_Expectation",
                            "AGQ_approach",
                            "AGQ_avoidance",
                            "Belonging",
                            "Centrality",
                            "STAI_pre",
                            "STAI_post",
                            "Age",
                            "Gender",
                            "HighestDegree", 
                            "FirstGen", "FirstGen_US",
                            "X1st_immigrant", "X2nd_immigrant", 
                            "CurrentMajor"), selected = "Condition")
  })
  
  # Display counts table for Condition variable
  output$tableVar1 <- renderTable({
    req(dataInput(), input$selectedVar1)
    selected_variable <- input$selectedVar1
    
    # Check if the selected variable is numeric
    if (is.numeric(dataInput()[[selected_variable]])) {
      # Display mean for numeric variable
      mean_value <- mean(dataInput()[[selected_variable]], na.rm = TRUE)
      mean_df <- data.frame(Variable = selected_variable, Mean = mean_value)
      return(mean_df)
    } else {
      # Display counts for factor variable
      table_data <- table(dataInput()[[selected_variable]])
      table_df <- as.data.frame(table_data)
      colnames(table_df) <- c(selected_variable, "Count")
      return(table_df)
    }
      # # If the selected variable is not "Condition", hide the table
      # shinyjs::runjs('$("#tableVar1").hide();')
      # return(NULL)
  })
  
  
  output$var2 <- renderUI({
    req(dataInput())
    selectInput("selectedVar2", "Select y variable:", 
                choices = c("Condition",
                            "Score",
                            "Pre_Expectation",
                            "Post_Expectation",
                            "AGQ_approach",
                            "AGQ_avoidance",
                            "Belonging",
                            "Centrality",
                            "STAI_pre",
                            "STAI_post",
                            "Age",
                            "Gender",
                            "HighestDegree", 
                            "FirstGen", "FirstGen_US",
                            "X1st_immigrant", "X2nd_immigrant", 
                            "CurrentMajor"), selected = "Score")
  })
  
  output$facet <- renderUI({
    choices <- c("None", c("Condition", "Gender", "HighestDegree", "FirstGen", "FirstGen_US",
                           "X1st_immigrant", "X2nd_immigrant", "CurrentMajor"))
    selectInput("facetCol", "Group", choices, selected = "None")
  })
  
  
  output$plot <- renderPlotly({
    req(input$selectedVar1, input$selectedVar2)
    
    # Check if variables are numeric
    if (is.numeric(dataInput()[[input$selectedVar1]]) && is.numeric(dataInput()[[input$selectedVar2]])) {
      # Create scatterplot
      p <- ggplot(data = dataInput(), aes(x = !!as.name(input$selectedVar1), y = !!as.name(input$selectedVar2))) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "lm", se = FALSE, color = "blue") +
        theme_minimal()
    } else { 
      # Check if either selectedVar1 or selectedVar2 is a factor
      # Create boxplot
      p <- ggplot(data = dataInput(), aes(x = !!as.name(input$selectedVar1), y = !!as.name(input$selectedVar2), 
                                          fill = !!as.name(input$selectedVar1))) +
        geom_boxplot(alpha = 0.5) +
        # Display mean
        stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red")
      theme_minimal()
    }
    
    # If a faceting column is selected, facet the plot
    if (input$facetCol != "None") {
      p <- p + facet_wrap(as.formula(paste("~", input$facetCol))) + 
        ggtitle(paste(input$selectedVar1, "vs.", input$selectedVar2, 
                      "- Grouped by", input$facetCol))
    } else {
      p <- p + ggtitle(paste(input$selectedVar1, "vs.", 
                             input$selectedVar2, "Plot"))
    }
    
    ggplotly(p)
  })
  
  
  output$tablePreview <- DT::renderDataTable({
    DT::datatable(head(dataInput(), 5), 
                  options = list(scrollX = TRUE, pageLength = 5, searching = TRUE))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
