
###########################################
# title: "Analysis of the Salary Dataset" #
# subtitle: "STAT 824 Final Project"      #
# author: "Bill Fitzpatrick"              #
# date: "7/18/2025"                       #
# output: Shiny Interative Dashboard      #
###########################################

#*** DATASET IS FROM KAGGLE ***
#***https://www.kaggle.com/datasets/rkiattisak/salaly-prediction-for-beginer***

# *** Alot of this code is not needed for the shiny app. I included it to show 
#     the R code used to manipulate and display data. ***

# Install and load required packages

# Install if necessary and load package ggplot2.
if (!require("ggplot2")) {
  install.packages("ggplot2")
}
library(ggplot2)

# Install if necessary and load package dplyr.
if (!require("dplyr")) {
  install.packages("dplyr")
}
library(dplyr)

if (!require("shiny")) {
  install.packages("shiny")
}
library(shiny)

if (!require("psych")) {
  install.packages("psych")
}
library(psych)

# Set to classic theme
theme_set(theme_classic())

#-------------------------------------------------------------------------------

# Load and clean the dataset

# Load the salary dataset
 Salary <- read.csv("C:/Users/trist/Downloads/Salary Data.csv", header = TRUE)
#Salary <- read.csv("Salary Data.csv", header = TRUE)

# List first 6 rows of the dataset
head(Salary) 

# View the dataset
View(Salary)

# Determine how much missing data
sum(is.na(Salary))

# Locate missing data 
Salary[!complete.cases(Salary), ]

# Delete rows with missing data because every column is NA
# Take out rows 173 and 261
new_Salary <- Salary[-c(173, 261), ]

# Check to make sure NA rows were taken out 
sum(is.na(new_Salary))

# Change salary for observation 259 from 350 to 35000. Obvious data entry error. 
new_Salary[259, "Salary"] <- 35000

# Convert all character variables to factor. Uses package dplyr.
sl <- new_Salary %>% mutate_if(is.character, as.factor)

# Check the variable characteristics again to see of character variables changed
# to factor.
str(sl)

# Get summary statistics for all the variables
summary(sl)

# Get detailed statistics for all variables. Uses the package psych.
describe(sl)

unique(sl$Education.Level)
#-------------------------------------------------------------------------------

############################
#*** CODE FOR SHINY APP ***#
############################

#*** DEFINE UI ***

# Define first panel that has histograms, bar graphs, and pie charts 
first_panel <- tabPanel(
  "Summary & Visuals",
  titlePanel("Salary Data Summary & Visuals"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("Plot_Type", "Choose a plot:",
                  choices = c(
                    "Employee Age Histogram",
                    "Employee Education Level Bar Graph",
                    "Employee Years of Experience Histogram",
                    "Employee Salary Histogram",
                    "Employee Education Level Pie Chart & Bar Graph",
                    "Employee Gender Pie Chart & Bar Graph"
                  )),
      selectInput("Stat_Summary_Var", "Choose variable for the statistical
                  summary:", choices = names(sl)[sapply(sl, is.numeric)])
      ),
    
    mainPanel(
      plotOutput("Graphs"),
      verbatimTextOutput("SummaryStats")
    )
  )
)

# Define panel that has scatter plots
second_panel <- tabPanel(
  "Scatterplots",
  titlePanel("Employee Scatterplots"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("scatterType", "Choose a scatterplot:",
                  choices = c("Age vs Salary", "Experience vs Salary",
                              "Gender vs Salary", "Education vs Salary"))
    ),
    
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

# Combine the first and second panel
ui <- navbarPage(
  "Shiny Interactive Dashboard for Salary Data",
  first_panel,
  second_panel
)

#-----------------

#*** DEFINE THE SERVER ***

server <- function(input, output) {
  
  # First Panel of histograms, bar graphs, and pie charts. 
  
  # Employee Age Histogram
  output$Graphs <- renderPlot({
    if (input$Plot_Type == "Employee Age Histogram") {
      ggplot(sl, aes(x = Age)) +
        geom_histogram(bins = 30, color = "black", fill = "lightblue") +
        xlab("Age") + ylab("Frequency of Age") +
        scale_x_continuous(breaks = seq(0, 60, 5)) +
        scale_y_continuous(breaks = seq(0, 45, 5)) +
        ggtitle("Employee Age - Histogram") +
        theme_classic() +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 14),
              plot.title = element_text(size = 20, face = "bold"))
      
    # Employee Education Level Bar Graph
    } else if (input$Plot_Type == "Employee Education Level Bar Graph") {
      ggplot(sl, aes(x = Education.Level)) +
        geom_bar(color = "black", fill = "lightblue") +
        xlab("Education Level") + ylab("Frequency of Education Level") +
        ggtitle("Employee Education Level - Bar Graph") +
        theme_classic() +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 14),
              plot.title = element_text(size = 20, face = "bold"))
      
    # Employee Years of Experience Histogram
    } else if (input$Plot_Type == "Employee Years of Experience Histogram") {
      ggplot(sl, aes(x = Years.of.Experience)) +
        geom_histogram(bins = 15, color = "black", fill = "lightblue") +
        xlab("Employee Years of Experience") + ylab("Frequency Yrs of Experience") +
        scale_x_continuous(breaks = seq(0, 30, 5)) +
        scale_y_continuous(breaks = seq(0, 50, 10)) +
        ggtitle("Employee Years of Experience - Histogram") +
        theme_classic() +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 14),
              plot.title = element_text(size = 20, face = "bold"))
      
    # Employee Salary Histogram
    } else if (input$Plot_Type == "Employee Salary Histogram") {
      ggplot(sl, aes(x = Salary)) +
        geom_histogram(bins = 23, color = "black", fill = "lightblue") +
        xlab("Employee Salary") + ylab("Frequency of Employee Salary") +
        scale_x_continuous(breaks = seq(0, 250000, 50000)) +
        scale_y_continuous(breaks = seq(0, 50, 10)) +
        ggtitle("Employee Salary - Histogram") +
        theme_classic() +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 14),
              plot.title = element_text(size = 20, face = "bold"))
      
      
   # Employee Education Level Pie Chart & Bar Graph on the same page
   } else if (input$Plot_Type == "Employee Education Level Pie Chart & Bar Graph") {
      
    # Create EducationCount variable for the pie chart. 
    EducationCount <- sl %>% count(Education.Level)
      
      # Employee Education Level Bar Graph
      Bar <- ggplot(sl, aes(x = Education.Level)) +
        geom_bar(color = "black", fill = "lightblue") +
        xlab("Employee Education Level") + ylab("Frequency of Education Level") +
        scale_y_continuous(breaks = seq(0, 250, 50)) +
        ggtitle("Employee Education Level - Bar Graph and Pie Chart") +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 14),
              plot.title = element_text(size = 20, face = "bold"))
      
      # Employee Education Level Pie Chart
      Pie <- ggplot(EducationCount, aes(x = "", y = n, fill = Education.Level)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        geom_text(aes(label = paste0(round((n / sum(n)) * 100, 2), "%")),
                  position = position_stack(vjust = 0.5)) +
        labs(x = NULL, y = NULL, fill = NULL) +
        theme(axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank())
      
      # Combine Employee Education Bar Graph and Pie Chart
      cowplot::plot_grid(Bar, Pie, ncol = 2)
      
      
   # Employee Gender Pie Chart & Bar Graph on the same page
   } else if (input$Plot_Type == "Employee Gender Pie Chart & Bar Graph") {
     
    # Create GenderCount variable for the pie chart. 
    GenderCount <- sl %>% count(Gender)
      
      # Employee Gender Bar Graph
      Bar_G <- ggplot(sl, aes(x = Gender)) +
        geom_bar(color = "black", fill = "lightblue") +
        xlab("Employee Gender") + ylab("Frequency of Employee Gender") +
        ggtitle("Employee Gender - Bar Graph and Pie Chart") +
        theme_classic() +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 14),
              plot.title = element_text(size = 20, face = "bold"))
      
      # Employee Gender Pie Chart
      Pie_G <- ggplot(GenderCount, aes(x = "", y = n, fill = Gender)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        geom_text(aes(label = paste0(round((n / sum(n)) * 100, 2), "%")),
                  position = position_stack(vjust = 0.5)) +
        labs(x = NULL, y = NULL, fill = NULL) +
        theme(axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank())
      
      # Combine Employee Gender Bar Graph and Pie Chart
      cowplot::plot_grid(Bar_G, Pie_G, ncol = 2)
    }
  })
  
  # Statistical summary going under graphs and plots
  output$SummaryStats <- renderPrint({
    psych::describe(sl[[input$Stat_Summary_Var]])
  })
  
#-------------------

  # Second Panel of Scatter Plots 
  
  # Scatterplot of Employee Age vs Employee Salary
  output$scatterPlot <- renderPlot({
    if (input$scatterType == "Age vs Salary") {
      ggplot(sl, aes(x = Age, y = Salary)) +
        geom_point(position = "jitter", color = "black", alpha = 0.1, size = 1.5) +
        geom_smooth(method = "lm", se = TRUE, size = 1.5, color = "lightblue") +
        scale_x_continuous(breaks = seq(0, 60, 5)) +
        scale_y_continuous(breaks = seq(0, 250000, 50000)) +
        xlab("Employee Age") + ylab("Employee Salary") +
        ggtitle("Employee Age vs Salary - Scatterplot") +
        theme(axis.text = element_text(size = 8),
              axis.title = element_text(size = 8),
              plot.title = element_text(size = 10, face = "bold"),
              panel.border = element_rect(color = "black", fill = NA, size = 1.5))
      
    # Scatterplot of Employee Years of Experience vs Employee Salary  
    } else if (input$scatterType == "Experience vs Salary") {
      ggplot(sl, aes(x = Years.of.Experience, y = Salary)) +
        geom_point(position = "jitter", color = "black", alpha = 0.1, size = 1.5) +
        geom_smooth(method = "lm", se = TRUE, size = 1.5, color = "lightblue") +
        scale_x_continuous(breaks = seq(0, 30, 5)) +
        scale_y_continuous(breaks = seq(0, 250000, 50000)) +
        xlab("Years of Experience") + ylab("Salary") +
        ggtitle("Experience vs Salary - Scatterplot") +
        theme(axis.text = element_text(size = 8),
              axis.title = element_text(size = 8),
              plot.title = element_text(size = 10, face = "bold"),
              panel.border = element_rect(color = "black", fill = NA, size = 1.5))
      
      # Scatter Plot of Employee Gender vs Employee Salary with Linear 
      # Regression Smoothing Line
      } else if (input$scatterType == "Gender vs Salary") {
        
      # Create a new variable Male_Female to change gender factors to numeric in 
      # order to do scatter plot
      sl <- sl %>%
        mutate(
          Male_Female = factor(Gender, levels = c("1","2")), # Set desired levels
          Male_Female = as.numeric(Gender) # Convert to numeric
        )
      sl$Male_Female
      
      # Scatter Plot of Employee Gender vs Employee Salary with Linear 
      # Regression Smoothing Line
        ggplot(sl, aes(x = Male_Female, y = Salary)) +
        geom_point(position = "jitter",
                   color = "black",
                   alpha = 1/10,
                   size = 1.5) +
        scale_x_continuous(
          breaks = c(1, 2),  # Numeric values on the x-axis
          labels = c("Female","Male")  # Corresponding character labels
        ) +
        geom_smooth(method = "lm", se = TRUE, size = 1.5, color = "lightblue") +
        scale_y_continuous(breaks = seq(0,250000,50000)) +
        xlab("Employee Gender") +
        ylab("Employee Salary") +
        ggtitle("Employee Gender vs Employee Salary - Scatterplot") +
        theme(axis.text = element_text(size = 8),
              axis.title = element_text(size = 8),
              plot.title = element_text(size = 10, face = "bold"),
              panel.border = element_rect(color = "black", fill = NA, size = 1.5))
      
      # Scatter Plot of Employee Education vs Employee Salary with Linear 
      # Regression Smoothing Line
        } else if (input$scatterType == "Education vs Salary") {
          
          # Create a New Variable called Education assigning numbers to the
          # levels of education in order to do the Scatter Plot
          sl_edu <- sl %>%
            mutate(Education = case_when(
              Education.Level == "Bachelor's" ~ 1,
              Education.Level == "Master's" ~ 2,
              Education.Level == "PhD" ~ 3
            ))
          
            # Scatter Plot of Employee Education vs Employee Salary with Linear 
            # Regression Smoothing Line
            ggplot(sl_edu, aes(x = Education, y = Salary)) +
            geom_point(position = "jitter",
                       color = "black",
                       alpha = 1/10,
                       size = 1.5) +
            scale_x_continuous(
              breaks = c(1, 2, 3),  # Numeric values on the x-axis
              labels = c("Bachelor's", "Master's", "Phd") # X-axis labels
            ) +
            geom_smooth(method = "lm", se = TRUE, size = 1.5, color = "lightblue") +
            scale_y_continuous(breaks = seq(0,250000,50000)) +
            xlab("Employee Age") +
            ylab("Employee Salary") +
            ggtitle("Employee Education Level vs Employee Salary - Scatterplot") +
            theme(axis.text = element_text(size = 8),
                  axis.title = element_text(size = 8),
                  plot.title = element_text(size = 10, face = "bold"),
                  panel.border = element_rect(color = "black", fill = NA, size = 1.5))
    }
  })
}

#  Launch the app
shinyApp(ui = ui, server = server)


