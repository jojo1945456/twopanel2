
###########################
# Bill Fitzpatrick        #
# DATA 824 Final Project  #
###########################

# PRODUCT DEVELOPEMENT

# ASSIGNMENT

# In this final Capstone Project, you’ll apply the skills learned by building a 
# data product using real-world data. This assignment is due 7/22/2025. 

# EXERCISE

# •	Project during the course, the students will work on a problem of their
#   interest.
#
# •	The project should follow the steps outlined during the lectures, that is,
#   literature review, design, data collection, data analysis, and data
#   presentation.  

# •	The goal is to create a shiny app for a problem of your choice. Some
#   examples could be word prediction, data representations with different R
#   visualization, etc.  A set of visualizations that allows you to perform some
#   kind of interesting analysis with your data. In addition, you need to create
#   a ppt presentation (no video) explaining the project and showing some
#   interesting insights that can be gained from it. You can pretend you are
#   giving a presentation for a specific purpose if you like (for example a
#   business meeting, an academic conference, etc.). Your visualizations need to
#   demonstrate the various skills you have learned. They should adhere to the
#   principles we have discussed. 

# •	For this final part, you will be graded on the following areas:
#   a. Background material – did the presentation tell us what the data were and
#                            why they were interesting?
#   b. Data – was the dataset sufficiently complex and interesting enough to
#             justify its use?
#   c. Insight – did the visualizations or tools created to allow for an
#                interesting insight into the data?
#   d. Principles – did the visualizations adhere to the principles, and were
#                   violations of the principles warranted?
#   e. Story – did the presentation give us an interesting/compelling story
#              about the data? 
#  
# • Describe what your application does. What to expect from it?
#   Where does the data come from?
#  
# •	Finally, you must deploy your product using Git (or drop the files) and
#   GitHub. Add links to your PowerPoint presentation to show the GitHub
#   repository and the Product Development. 
#-------------------------------------------------------------------------------

# INSTALL AND LOAD PACKAGES
#--------------------------

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

# Install if necessary and load package hexbin.
if (!require("psych")) {
  install.packages("psych")
}
library(psych)

# Install if necessary and load package shiny.
if (!require("shiny")) {
  install.packages("shiny")
}
library(shiny)

# Install if necessary and load package GGally.
if (!require("GGally")) {
  install.packages("GGally")
}
library(GGally)

# Set to classic theme
theme_set(theme_classic())

#-------------------------------------------------------------------------------

# LOAD THE DATASET AND EXPLORE
# ----------------------------
# Load the dataset Salary Data
Salary <- read.csv("C:/Users/trist/Downloads/Salary Data.csv", header=TRUE)

# List first 6 rows of the dataset
head(Salary) 
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

# Check the variable characteristics
str(new_Salary)

# Convert all character variables to factor. Uses package dplyr.
sl <- new_Salary %>% mutate_if(is.character, as.factor)

# Check the variable characteristics again to see of character variables changed
# to factor.
str(sl)

# Get summary statistics for all the variables
summary(sl)

# Get detailed statistics for all variables. Uses the package psych.
describe(sl)

#-------------------------------------------------------------------------------

# SUMMARIZE AND SORT JOB TITLES
# -----------------------------

# Use the function attach so that the data frame is directly accessible by the 
# variable names without the need for $.
attach(sl)

# Calculate the most prevalent job titles
Jobcount <- sl %>%
  count(Job.Title, name = "count") %>%
  arrange(desc(count)) # Optional: Sort by count in descending order

View(Jobcount)

# Sum salary by job title and sort
Salarybyjob <- sl %>%
  group_by(Job.Title) %>%
  summarise(avg_Salary = mean(Salary)) %>%
  arrange(desc(avg_Salary)) # Sort in descending order of sum
View(Salarybyjob)

Salarybyjob <- sl %>%
  group_by(Job.Title) %>%
  summarise(avg_Salary = mean(Salary, na.rm = TRUE)) %>%
  arrange(desc(avg_Salary))  # Sort in descending order of average
View(Salarybyjob)

#-------------------------------------------------------------------------------

# HISTOGRAMS AND BAR GRAPH
# ------------------------

# Histogram of employee age
histage <- sl %>% 
  ggplot(aes(x = Age)) +
  geom_histogram(bins = 30, color = "black", fill = "lightblue") +
  theme_classic() +
  xlab("Age") +
  ylab("Frequency of Age") +
  scale_x_continuous(breaks = seq(0,60,5)) +
  scale_y_continuous(breaks = seq(0,45,5)) +
  ggtitle("Age of Employee - Histogram") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))
print(histage)

# Bar plot of employee education level
histeduc <- sl %>% 
  ggplot(aes(x = Education.Level)) +
  geom_bar(color = "black", fill = "lightblue") +
  theme_classic() +
  xlab("Education Level") +
  ylab("Frequency of Education Level") +
 # scale_y_continuous(breaks = seq(0,45,5)) +
  ggtitle("Employee Education Level - Bar Graph") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))
print(histeduc)

# Histogram of employee years of experience
histexp <- sl %>% 
  ggplot(aes(x = Years.of.Experience)) +
  geom_histogram(bins = 15, color = "black", fill = "lightblue") +
  theme_classic() +
  xlab("Employee Years of Experience") +
  ylab("Frequency Yrs of Experience") +
    scale_x_continuous(breaks = seq(0,30,5)) +
    scale_y_continuous(breaks = seq(0,50,10)) +
  ggtitle("Employee Years of Experience - Histogram") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))
print(histexp)

# Histogram of Employee Salary
histsal <- sl %>% 
  ggplot(aes(x = Salary)) +
  geom_histogram(bins = 23, color = "black", fill = "lightblue") +
  theme_classic() +
  xlab("Employee Salary") +
  ylab("Frequency of Employee Salary") +
  scale_x_continuous(breaks = seq(0,250000,50000)) +
#  scale_y_continuous(breaks = seq(0,50,10)) +
  ggtitle("Employee Salary - Histogram") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))
print(histsal)

cowplot::plot_grid(histage, histeduc, histexp, histsal, ncol = 2)

#-------------------------------------------------------------------------------

# Combination bar graph and pie chart showing employee education level using
# the cowplot function

# Change text sizes and name the bar graph for employee education level for use
# of the cowplot function
Bar <- sl %>% 
  ggplot(aes(x = Education.Level)) +
  geom_bar(color = "black", fill = "lightblue") +
  xlab("Education Level") +
  ylab("Frequency of Education Level") +
   scale_y_continuous(breaks = seq(0,250,50)) +
  ggtitle("Employee Education Level - Bar Graph and Pie Chart") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold"))

# Calculate total count of each education level 
EducationCount <- sl %>%
  count(Education.Level, name = "count")

# Change text sizes and name the pie chart for employee education level for use
# in the cowplot function
Pie <- ggplot(EducationCount, aes(x="", y=count, fill=Education.Level)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(round((count/373)*100,2), "%")),
            position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) 

# Use cowplot function to create subplots of the bar graph and pie chart of 
# education level
cowplot::plot_grid(Bar, Pie, ncol = 2)


#-------------------------------------------------------------------------------

# Combination bar graph and pie chart showing employee gender using the cowplot
# function

# Change text sizes and name the bar graph for gender for use in the cowplot
# function
Bar_G <- sl %>% 
  ggplot(aes(x = Gender)) +
  geom_bar(color = "black", fill = "lightblue") +
  theme_classic() +
  xlab("Education Level") +
  ylab("Frequency of Gender") +
  # scale_y_continuous(breaks = seq(0,45,5)) +
  ggtitle("Employee Gender - Bar Graph and Pie Chart") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold"))

# Calculate total count of each gender
GenderCount <- sl %>%
  count(Gender, name = "count")

# Change text sizes and name the pie chart for gender for use in the cowplot
# function
Pie_G <- ggplot(GenderCount, aes(x="", y=count, fill=Gender)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(round((count/373)*100,2), "%")),
            position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) 

# Use cowplot function to create bar graph and pie chart of gender
cowplot::plot_grid(Bar_G, Pie_G, ncol = 2)
str(sl)
gender_count
#-------------------------------------------------------------------------------

# SCATTER PLOTS
#--------------

# Scatter plot of employee age vs employee salary with linear regression 
# smoothing line
scatempage <- sl %>%
  ggplot(aes(x = Age, y = Salary)) +
  geom_point(position = "jitter",
             color = "black",
             alpha = 1/10,
             size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, size = 1.5, color = "lightblue") +
  scale_x_continuous(breaks = seq(0,60,5)) +
  scale_y_continuous(breaks = seq(0,250000,50000)) +
  xlab("Employee Age") +
  ylab("Employee Salary") +
  ggtitle("Employee Age vs Employee Salary - Scatterplot") +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))
print(scatempage)


# Scatter plot of education vs employee salary with linear regression 
# smoothing line

# Create new variable changing education level factors to numeric to do scatter
# plot
Education = as.numeric(Education.Level)

# Scatter plot
scatempeduc <- sl %>%
  ggplot(aes(x = Education, y = Salary)) +
  geom_point(position = "jitter",
             color = "black",
             alpha = 1/10,
             size = 1.5) +
  scale_x_continuous(
    breaks = c(1, 2, 3),  # Numeric values on the x-axis
    labels = c("Bachelor's", "Master's", "Phd") # X-axis labels
  ) +
  geom_smooth(method = "lm", se = TRUE, size = 1.5, color = "lightblue") +
  #  scale_x_continuous(breaks = seq(0,60,5)) +
  #  scale_y_continuous(breaks = seq(0,250000,50000)) +
  xlab("Employee Level of Education") +
  ylab("Employee Salary") +
  ggtitle("Employee Education vs Employee Salary - Scatterplot") +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))
print(scatempeduc)


# Scatter plot of employee years of experience vs employee salary with linear
# regression smoothing line
scatempexp <- sl %>%
  ggplot(aes(x = Years.of.Experience, y = Salary)) +
  geom_point(position = "jitter",
             color = "black",
             alpha = 1/10,
             size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, size = 1.5, color = "lightblue") +
   scale_x_continuous(breaks = seq(0,30,5)) +
   scale_y_continuous(breaks = seq(0,250000,50000)) +
  xlab("Employee Years of Experience") +
  ylab("Employee Salary") +
  ggtitle("Employee Experience vs Employee Salary - Scatterplot") +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 9, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))
print(scatempexp)


# Scatter plot of employee gender vs employee salary with linear regression 
# smoothing line

# Create new variable to change gender level factors to numeric to do scatter
# plot
sl <- sl %>%
  mutate(
    Male_Female = factor(Gender, levels = c("1","2")), # Set desired levels
    Male_Female = as.numeric(Gender) # Convert to numeric
  )
sl$Male_Female

# Scatter Plot
scatempgend <- sl %>%
  ggplot(aes(x = Male_Female, y = Salary)) +
  geom_point(position = "jitter",
             color = "black",
             alpha = 1/10,
             size = 1.5) +
  scale_x_continuous(
    breaks = c(1, 2),  # Numeric values on the x-axis
    labels = c("Female","Male")  # Corresponding character labels
  ) +
  geom_smooth(method = "lm", se = TRUE, size = 1.5, color = "lightblue") +
  #  scale_x_continuous(breaks = seq(0,60,5)) +
  #  scale_y_continuous(breaks = seq(0,250000,50000)) +
  xlab("Employee Gender") +
  ylab("Employee Salary") +
  ggtitle("Employee Gender vs Employee Salary - Scatterplot") +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))
print(scatempgend)


cowplot::plot_grid(scatempage, scatempeduc, scatempexp, scatempgend, ncol = 2)


#-------------------------------------------------------------------------------


# BOX PLOTS

# Create new variable changing Age from numeric to factor to do box plot
agef = as.factor(Age)

# Box plot comparing employee age with employee salary
sl %>%
  ggplot(aes(x = agef, y = Salary)) +
  geom_boxplot(size = 0.5,
               outlier.alpha = 1/3,
               outlier.size = 1,
               fill = "lightblue") +
  xlab("Employee Age") +
  ylab("Employee Salary") +
  ggtitle("Employee Age vs Employee Salary - Box Plot") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.0),
        legend.position = "none")


# Creating variable Education.Factor so the box plot of education level vs 
# employee salary has a factor variable for the x-axis. 
sl <- sl %>%
  mutate(Education.Factor = factor(Education,
                                   levels = c(1, 2, 3),
                                   labels = c("Bachelor's", "Master's", "PhD")))

# Box plot comparing employee education level with employee salary
sl %>%
  ggplot(aes(x = Education.Factor, y = Salary)) +
  geom_boxplot(size = 0.5,
               outlier.alpha = 1/3,
               outlier.size = 1,
               fill = "lightblue") +
  xlab("Employee Education Level") +
  ylab("Employee Salary") +
  ggtitle("Employee Education Level vs Employee Salary - Box Plot") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 19, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.position = "none")
  

# Create new variable changing variable years of experience from numeric to
# factor to do box plot
experf = as.factor(Years.of.Experience)

# Box plot comparing employee years of experience with employee salary
sl %>%
  ggplot(aes(x = experf, y = Salary)) +
  geom_boxplot(size = 0.5,
               outlier.alpha = 1/3,
               outlier.size = 1,
               fill = "lightblue") +
  #  scale_x_continuous(
  #    breaks = c(23,24,25),  # Numeric values on the x-axis
  #    labels = c("Female","Male")  # Corresponding character labels
  #  scale_x_continuous(labels = c("23","25",2)) +
  xlab("Employee Years of Experience") +
  ylab("Employee Salary") +
  ggtitle("Employee Experience vs Employee Salary - Box Plot") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 19, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.position = "none")

# Box plot comparing employee gender with employee salary
sl %>%
  ggplot(aes(x = Gender, y = Salary)) +
  geom_boxplot(size = 0.5,
               outlier.alpha = 1/3,
               outlier.size = 1,
               fill = "lightblue") +
  #  scale_x_continuous(
  #    breaks = c(23,24,25),  # Numeric values on the x-axis
  #    labels = c("Female","Male")  # Corresponding character labels
  #  scale_x_continuous(labels = c("23","25",2)) +
  xlab("Employee Gender") +
  ylab("Employee Salary") +
  ggtitle("Employee Gender vs Employee Salary - Box Plot") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.position = "none")


#-------------------------------------------------------------------------------


# SCATTER PLOTS AND BOX PLOTS ON THE SAME PAGE

# Create new variable for employee age, change size of text, and remove title to
# make more room for scatter plot
scatage <- sl %>%
  ggplot(aes(x = Age, y = Salary)) +
  geom_point(position = "jitter",
             color = "black",
             alpha = 1/10,
             size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, size = 1.5, color = "lightblue") +
  scale_x_continuous(breaks = seq(0,60,5)) +
  scale_y_continuous(breaks = seq(0,250000,50000)) +
  xlab("Employee Age") +
  ylab("Employee Salary") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

# Create new variable for employee age, change size of text, and remove title to
# make more room for box plot
boxage <- sl %>%
  ggplot(aes(x = agef, y = Salary, color = Age)) +
  geom_boxplot(size = 0.5,
               outlier.alpha = 1/3,
               outlier.size = 1,
               fill = "lightblue") +
  #  scale_x_continuous(
  #    breaks = c(23,24,25),  # Numeric values on the x-axis
  #    labels = c("Female","Male")  # Corresponding character labels
  #  scale_x_continuous(labels = c("23","25",2)) +
  xlab("Employee Age") +
  ylab("Employee Salary") +
#  ggtitle("Employee Age vs Employee Salary - Box Plot") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.position = "none")

# Use cowplot function to display scatter plot and box plot of age on the same
# page
cowplot::plot_grid(scatage, boxage, ncol = 1)


# Create new variable for employee eudcation level, change size of text, and
# remove title to make more room for scatter plot
scateduc <- sl %>%
  ggplot(aes(x = Education, y = Salary)) +
  geom_point(position = "jitter",
             color = "black",
             alpha = 1/10,
             size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, size = 1.5, color = "lightblue") +
    xlab("Employee Education Level") +
  ylab("Employee Salary") +
   ggtitle("Education Level vs Salary - Scatterplot") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

# Create new variable for employee education level, change size of text, and
# remove title to make more room for box plot
boxeduc <- sl %>%
  ggplot(aes(x = Education.Level, y = Salary, color = Age)) +
  geom_boxplot(size = 0.5,
               outlier.alpha = 1/3,
               outlier.size = 1,
               fill = "lightblue") +
  xlab("Employee Education Level") +
  ylab("Employee Salary") +
    ggtitle("Education Level vs Salary - Box Plot") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.position = "none")

# Use cowplot function to display scatter plot and box plot of age on the same
# page
cowplot::plot_grid(scateduc, boxeduc, ncol = 2)


# Create new variable for employee years of experience, change size of text, and
# remove title to make more room for scatter plot
scatexp <- sl %>%
  ggplot(aes(x = Years.of.Experience, y = Salary)) +
  geom_point(position = "jitter",
             color = "black",
             alpha = 1/10,
             size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, size = 1.5, color = "lightblue") +
  scale_x_continuous(breaks = seq(0,30,5)) +
  scale_y_continuous(breaks = seq(0,250000,50000)) +
  xlab("Employee Years of Experience") +
  ylab("Employee Salary") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

# Create new variable for employee education level, change size of text, and
# remove title to make more room for box plot
boxexp <- sl %>%
  ggplot(aes(x = experf, y = Salary)) +
  geom_boxplot(size = 0.5,
               outlier.alpha = 1/3,
               outlier.size = 1,
               fill = "lightblue") +
  xlab("Employee Years of Experience") +
  ylab("Employee Salary") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.position = "none")

# Use cowplot function to display scatter plot and box plot of age on the same
# page
cowplot::plot_grid(scatexp, boxexp, ncol = 1)


# Create new variable for employee gender, change size of text, and remove title
# to make more room for scatter plot
scatgend <- sl %>%
  ggplot(aes(x = Male_Female, y = Salary)) +
  geom_point(position = "jitter",
             color = "black",
             alpha = 1/10,
             size = 1.5) +
  scale_x_continuous(
    breaks = c(1, 2),  # Numeric values on the x-axis
    labels = c("Female","Male")  # Corresponding character labels
  ) +
  geom_smooth(method = "lm", se = TRUE, size = 1.5, color = "lightblue") +
  #  scale_x_continuous(breaks = seq(0,60,5)) +
  #  scale_y_continuous(breaks = seq(0,250000,50000)) +
  xlab("Employee Gender") +
  ylab("Employee Salary") +
  ggtitle("Gender vs Salary - Scatterplot") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

# Create new variable for employee gender, change size of text, and remove title to
# make more room for box plot
boxgend <- sl %>%
  ggplot(aes(x = Gender, y = Salary)) +
  geom_boxplot(size = 0.5,
               outlier.alpha = 1/3,
               outlier.size = 1,
               fill = "lightblue") +
  #  scale_x_continuous(
  #    breaks = c(23,24,25),  # Numeric values on the x-axis
  #    labels = c("Female","Male")  # Corresponding character labels
  #  scale_x_continuous(labels = c("23","25",2)) +
  xlab("Employee Gender") +
  ylab("Employee Salary") +
  ggtitle("Gender vs Salary - Box Plot") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.position = "none")

# Use cowplot function to display scatter plot and box plot of age on the same
# page
cowplot::plot_grid(scatgend, boxgend, ncol = 2)

#-------------------------------------------------------------------------------

# CORRELATION ANALYSIS

# Produce a correlation heatmap for the variables

# Remove the variables classified as factor
new_sl <- subset(sl, select = -c(Gender, Education.Level, Job.Title, 
                                 Education.Factor))

# Check variable characteristics
str(new_sl)

#calculate correlation between each pairwise combination of variables
cor_sl <- round(cor(new_sl), 2)

#melt the data frame
melted_cormat <- melt(cor_sl)

#create correlation heatmap
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 5) +
  scale_fill_gradient2(low = "blue", high = "red",
                       limit = c(-1,1), name="Correlation") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())

#---------

# Create a scatter plot matrix to display pairwise correlation between all the
# variables using package GGally with function ggpairs. 
ggpairs(new_sl)


ggpairs(iris, 
        columns = 1:4,  # Select numeric columns for the matrix
        aes(color = Species, alpha = 0.7)) +
  theme_minimal()  # Add a clean theme


#-------------------------------------------------------------------------------


# LISTS OF JOB TITLES BY NUMBER OF EMPLOYEES AND AVG SALARY

# Number of employees per job title in descending order
numjob <- Salary %>%
  group_by(Job.Title) %>%
  summarise(num_emp = n()) %>%
  arrange(desc(num_emp))

# Create a variable with the top 20 job titles by number of employees 
topnum <- numjob[1:20, ]

# Create bar chart showing the top 20 job titles by number of employess
ggplot(topnum, aes(x = reorder(Job.Title, num_emp), y = num_emp)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  theme_classic() +
  xlab("Job Title") +
  ylab("Number of Employees") +
  scale_y_continuous(breaks = seq(0,14,1)) +
  ggtitle("Top 20 Job Titles by Number of Employees") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold"))

  
# Job Titles sorted by average salary in DESCENDING order
avgsal <- Salary %>%
  group_by(Job.Title) %>%
  summarise(Avg_Salary = mean(Salary)) %>%
  arrange(desc(Avg_Salary))

# Create a variable with the top 20 job titles by average salary 
topsal <- avgsal[1:20, ]

# Create bar chart showing the top 20 job titles by average salary
ggplot(topsal, aes(x = reorder(Job.Title, Avg_Salary), y = Avg_Salary)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  theme_classic() +
  xlab("Job Title") +
  ylab("Average Salary") +
  scale_y_continuous(breaks = seq(0,250000,50000)) +
  ggtitle("Top 20 Job Titles based on Average Salary") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold"))


# Job Titles sorted by average salary in ASCENDING order
avgsal <- Salary %>%
  group_by(Job.Title) %>%
  summarise(Avg_Salary = mean(Salary)) %>%
  arrange(Avg_Salary)

# Create a variable with the lowest 20 job titles based on salary
lowsal <- avgsal[1:20, ]

# Set Job.Title as a factor with levels ordered from highest to lowest so in the
# bar graph the job with the lowest average salary is at the top
lowsal$Job.Title <- factor(lowsal$Job.Title, levels = rev(lowsal$Job.Title))

# Create bar chart showing the lowest 20 job titles by average salary
ggplot(lowsal, aes(x = Job.Title, y = Avg_Salary)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  theme_classic() +
  xlab("Job Title") +
  ylab("Average Salary") +
  scale_y_continuous(breaks = seq(0,50000,10000)) +
  ggtitle("Lowest 20 Job Titles based on Average Salary")
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold"))

  
  
  
  
  
  
  



