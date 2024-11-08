## LSE Data Analytics Online Career Accelerator 
# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment 5 scenario
## Turtle Games’s sales department has historically preferred to use R when performing 
## sales analyses due to existing workflow systems. As you’re able to perform data analysis 
## in R, you will perform exploratory data analysis and present your findings by utilising 
## basic statistics and plots. You'll explore and prepare the data set to analyse sales per 
## product. The sales department is hoping to use the findings of this exploratory analysis 
## to inform changes and improvements in the team. (Note that you will use basic summary 
## statistics in Module 5 and will continue to go into more detail with descriptive 
## statistics in Module 6.)

################################################################################

## Assignment 5 objective
## Load and wrangle the data. Use summary statistics and groupings if required to sense-check
## and gain insights into the data. Make sure to use different visualisations such as scatterplots, 
## histograms, and boxplots to learn more about the data set. Explore the data and comment on the 
## insights gained from your exploratory data analysis. For example, outliers, missing values, 
## and distribution of data. Also make sure to comment on initial patterns and distributions or 
## behaviour that may be of interest to the business.

################################################################################

# Module 5 assignment: Load, clean and wrangle data using R

## It is strongly advised that you use the cleaned version of the data set that you created and 
##  saved in the Python section of the course. Should you choose to redo the data cleaning in R, 
##  make sure to apply the same transformations as you will have to potentially compare the results.
##  (Note: Manual steps included dropping and renaming the columns as per the instructions in module 1.
##  Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 

## 1. Open your RStudio and start setting up your R environment. 
## 2. Open a new R script and import the turtle_review.csv data file, which you can download from 
##      Assignment: Predicting future outcomes. (Note: You can use the clean version of the data 
##      you saved as csv in module 1, or, can manually drop and rename the columns as per the instructions 
##      in module 1. Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 
## 3. Import all the required libraries for the analysis and view the data. 
## 4. Load and explore the data.
##    - View the head the data.
##    - Create a summary of the new data frame.
## 5. Perform exploratory data analysis by creating tables and visualisations to better understand 
##      groupings and different perspectives into customer behaviour and specifically how loyalty 
##      points are accumulated. Example questions could include:
##    - Can you comment on distributions, patterns or outliers based on the visual exploration of the data?
##    - Are there any insights based on the basic observations that may require further investigation?
##    - Are there any groupings that may be useful in gaining deeper insights into customer behaviour?
##    - Are there any specific patterns that you want to investigate
## 6. Create
##    - Create scatterplots, histograms, and boxplots to visually explore the loyalty_points data.
##    - Select appropriate visualisations to communicate relevant findings and insights to the business.
## 7. Note your observations and recommendations to the technical and business users.

###############################################################################



# Import required libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(DataExplorer)
library(skimr)
library(moments)
library(psych)
require(lattice)
library(magrittr)
library(plotly)
library(patchwork)
library(lubridate)
library(stats)


# Check current working directory
getwd()

# Import the data
# Imported through 'Environment'

# View the head of the data
head(turtle_new_r)

# Drop unnecessary columns
turtle_new_r <- select(turtle_new_r, -c(review, summary))

# Summary of the data frame
summary(turtle_new_r)

# Exploratory Data Analysis

# Print structure of the dataset
str(turtle_new_r)
View(turtle_new_r)
dim(turtle_new_r)
typeof(turtle_new_r)
class(turtle_new_r)

# View column names
colnames(turtle_new_r)

# Summary statistics using skimr
skim(turtle_new_r)

# EDA - scatterplot of Age vs Loyalty Points
ggplot(turtle_new_r, aes(x = age, y = loyalty_points)) +
  geom_point() +
  labs(x = "Age", y = "Loyalty Points") +
  ggtitle("Scatterplot of Age vs Loyalty Points")

# EDA - histogram of Age
ggplot(turtle_new_r, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(x = "Age", y = "Frequency") +
  ggtitle("Histogram of Age")

# EDA - scatterplot of Remuneration vs Loyalty Points
ggplot(turtle_new_r, aes(x = remuneration, y = loyalty_points)) +
  geom_point() +
  labs(x = "Remuneration", y = "Loyalty Points") +
  ggtitle("Scatterplot of Remuneration vs Loyalty Points")

# EDA - histogram of Remuneration
ggplot(turtle_new_r, aes(x = remuneration)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(x = "Remuneration", y = "Frequency") +
  ggtitle("Histogram of Remuneration")

# EDA - scatterplot of Spending Score vs Loyalty Points
ggplot(turtle_new_r, aes(x = spending_score, y = loyalty_points)) +
  geom_point() +
  labs(x = "Spending Score", y = "Loyalty Points") +
  ggtitle("Scatterplot of Spending Score vs Loyalty Points")

# EDA - histogram of Spending Score
ggplot(turtle_new_r, aes(x = spending_score)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  labs(x = "Spending Score", y = "Frequency") +
  ggtitle("Histogram of Spending Score")

# Grouped barplot: Education by Gender
# Specify the ggplot function
ggplot(turtle_new_r, aes(x = education, fill = gender)) + 
  # Specify the geom_bar function
  # Add position
  geom_bar(position = 'dodge') + 
  # Change fill colours
  scale_fill_manual(values=c('skyblue', 'orange')) +
  labs(title = "Education by Gender")


# EDA - boxplot of Spending Score by Gender
ggplot(turtle_new_r, aes(x = gender, y = spending_score, fill = gender)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Spending Score") +
  ggtitle("Boxplot of Spending Score by Gender")

# EDA - boxplot of Remuneration by Gender
ggplot(turtle_new_r, aes(x = gender, y = remuneration, fill = gender)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Remuneration") +
  ggtitle("Boxplot of Remuneration by Gender")

# EDA - boxplot of Loyalty Points by Gender
ggplot(turtle_new_r, aes(x = gender, y = loyalty_points, fill = gender)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Loyalty Points") +
  ggtitle("Boxplot of Loyalty Points by Gender")


# EDA - boxplot of Spending Score by Education
ggplot(turtle_new_r, aes(x = education, y = spending_score, fill = education)) +
  geom_boxplot() +
  labs(x = "Education", y = "Spending Score") +
  ggtitle("Boxplot of Spending Score by Education")

# EDA - boxplot of Remuneration by Education
ggplot(turtle_new_r, aes(x = education, y = remuneration, fill = education)) +
  geom_boxplot() +
  labs(x = "Education", y = "Remuneration") +
  ggtitle("Boxplot of Remuneration by Education")

# EDA - boxplot of Loyalty Points by Education
ggplot(turtle_new_r, aes(x = education, y = loyalty_points, fill = education)) +
  geom_boxplot() +
  labs(x = "Education", y = "Loyalty Points") +
  ggtitle("Boxplot of Loyalty Points by Education")


# Statistical tests - ANOVA for Gender
gender_anova <- aov(loyalty_points ~ gender, data = turtle_new_r)
summary(gender_anova)
# The ANOVA analysis reveals that the 'gender' variable, with a p-value of 0.363, 
# suggests no significant difference in 'loyalty_points' between genders, 
# supported by the F value of 0.827 indicating a larger variance within groups than 
# between groups.

# Statistical tests - ANOVA for Education
education_anova <- aov(loyalty_points ~ education, data = turtle_new_r)
summary(education_anova)
# The ANOVA analysis indicates a significant impact of the 'education' variable 
# on 'loyalty_points', with a p-value of 8.94e-06 much smaller than 0.05, and 
# an F value of 7.226 suggesting a notable difference in variance between education levels.

# Post-hoc tests for ANOVA results (if ANOVA is significant)
# ANOVA is significant for Education
TukeyHSD(education_anova)


# Observations and Recommendations
# Loyalty Points: There is a large range in loyalty points, 
# with some customers having significantly higher points than others.

# Age: Customer age is spread out, with a range of 17 to 72 years old.
# Education: There are five education levels represented: graduate, PhD, and three others.
# Gender: The data shows no significant difference in loyalty points based on gender.
# Education: There is a significant difference in loyalty points based on education level. 
# Customers with higher education levels tend to have higher loyalty points.

# Recommendations:
  
# Target High-Value Customers: Analyse which education levels have the highest loyalty points 
# and target marketing efforts towards those groups.

# Loyalty Program Analysis: Investigate how the loyalty program works and if adjustments 
# could be made to incentivise higher spending or engagement from all customer segments, 
# especially those with lower education levels.

# Further Analysis: Explore the relationship between other variables, 
# such as "spending_score", "product", and "loyalty_points". 
# This could reveal additional insights into customer behaviour

# Data Visualisation: Consider creating additional visualisations, such as scatter plots, 
# to explore relationships between variables in more detail.




###############################################################################
###############################################################################

# Assignment 6 scenario

## In Module 5, you were requested to redo components of the analysis using Turtle Games’s preferred 
## language, R, in order to make it easier for them to implement your analysis internally. As a final 
## task the team asked you to perform a statistical analysis and create a multiple linear regression 
## model using R to predict loyalty points using the available features in a multiple linear model. 
## They did not prescribe which features to use and you can therefore use insights from previous modules 
## as well as your statistical analysis to make recommendations regarding suitability of this model type,
## the specifics of the model you created and alternative solutions. As a final task they also requested 
## your observations and recommendations regarding the current loyalty programme and how this could be 
## improved. 

################################################################################

## Assignment 6 objective
## You need to investigate customer behaviour and the effectiveness of the current loyalty program based 
## on the work completed in modules 1-5 as well as the statistical analysis and modelling efforts of module 6.
##  - Can we predict loyalty points given the existing features using a relatively simple MLR model?
##  - Do you have confidence in the model results (Goodness of fit evaluation)
##  - Where should the business focus their marketing efforts?
##  - How could the loyalty programme be improved?
##  - How could the analysis be improved?

################################################################################

## Assignment 6 assignment: Making recommendations to the business.

## 1. Continue with your R script in RStudio from Assignment Activity 5: Cleaning, manipulating, and 
##     visualising the data.
## 2. Load and explore the data, and continue to use the data frame you prepared in Module 5.
## 3. Perform a statistical analysis and comment on the descriptive statistics in the context of the 
##     review of how customers accumulate loyalty points.
##  - Comment on distributions and patterns observed in the data.
##  - Determine and justify the features to be used in a multiple linear regression model and potential
##.    concerns and corrective actions.
## 4. Create a Multiple linear regression model using your selected (numeric) features.
##  - Evaluate the goodness of fit and interpret the model summary statistics.
##  - Create a visual demonstration of the model
##  - Comment on the usefulness of the model, potential improvements and alternate suggestions that could 
##     be considered.
##  - Demonstrate how the model could be used to predict given specific scenarios. (You can create your own 
##     scenarios).
## 5. Perform exploratory data analysis by using statistical analysis methods and comment on the descriptive 
##     statistics in the context of the review of how customers accumulate loyalty points.
## 6. Document your observations, interpretations, and suggestions based on each of the models created in 
##     your notebook. (This will serve as input to your summary and final submission at the end of the course.)

################################################################################

# Select desired features from 'turtle_new_r' (if necessary)
turtle_mlr <- turtle_new_r[, c("age", "remuneration",
                               "spending_score", "loyalty_points")]

# Build the multiple linear regression model
model <- lm(loyalty_points ~ age + remuneration + spending_score,
            data = turtle_mlr)

# Evaluate the model
summary(model)  # View model coefficients, p-values, R-squared, etc.

# Evaluate goodness of fit
# R-squared
r_squared <- summary(model)$r.squared
cat("R-squared:", r_squared, "\n")

# Adjusted R-squared
adjusted_r_squared <- summary(model)$adj.r.squared
cat("Adjusted R-squared:", adjusted_r_squared, "\n")

# F-test
f_test <- summary(model)$fstatistic
cat("F-statistic:", f_test[1], "\n")
cat("p-value:", pf(f_test[1], f_test[2], f_test[3],
                   lower.tail = FALSE), "\n")

# Residual analysis
par(mfrow=c(2,2))  # Set up the layout for residual plots
plot(model)         # Plot all four residual plots

# Visualise the findings
# Scatterplot of observed vs. predicted values
predicted <- predict(model)
observed <- turtle_mlr$loyalty_points

plot(observed, predicted, 
     xlab = "Observed Loyalty Points",
     ylab = "Predicted Loyalty Points",
     main = "Observed vs. Predicted Loyalty Points",
     col = "blue")
abline(0, 1, col = "red")  # Add a diagonal line for reference

# Hypothetical scenario 1: Predict loyalty points for a customer with the following characteristics
hypothetical_data <- data.frame(age = 28, remuneration = 100.73, spending_score = 70)
predicted_points <- predict(model, newdata = hypothetical_data)
cat("Predicted Loyalty Points for Hypothetical Scenario:", predicted_points, "\n")

# Hypothetical scenario 2: Predict loyalty points for a customer with the following characteristics
hypothetical_data <- data.frame(age = 56, remuneration = 78.23, spending_score = 23)
predicted_points <- predict(model, newdata = hypothetical_data)
cat("Predicted Loyalty Points for Hypothetical Scenario:", predicted_points, "\n")

# Shapiro-Wilk test for normality of residuals
shapiro.test(model$residuals)

# Compute skewness of residuals
skew <- skewness(model$residuals)
cat("Skewness of Residuals:", skew, "\n")

# Compute kurtosis of residuals
kurt <- kurtosis(model$residuals)
cat("Kurtosis of Residuals:", kurt, "\n")

# The Shapiro-Wilk test rejects the null hypothesis of normality, indicating non-normality of residuals.
# However, the skewness of residuals is close to zero, indicating approximate symmetry, 
# and the kurtosis is slightly higher than expected for a normal distribution.

# Ultimately, while the model's predictive accuracy may not be affected significantly 
# by the non-normality of residuals, it's essential to consider other factors such as the model's overall fit, 
# the presence of influential outliers, multicollinearity, 
# and the practical relevance of the predictors in making predictions. 
# Therefore, additional evaluation and validation measures beyond normality tests
# are crucial for determining the usefulness of the model for predictions.



###############################################################################
###############################################################################




