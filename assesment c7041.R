## Header ####
## C7041 Assignment ####
## last edited 01-02-2023 ####


## CONTENTS ####

# 01. Loading the excel library ##
# 02. Import the data from the Excel file ##
# 03.
# 04.


## 01. Loading the excel library ####

library(readxl)

asses_data <- read_excel("D:/HAU/Modules/C7041/asses.data.xlsx")

View(asses_data)

head(asses_data)

## hypothesis 1 ####

# Load required library
library(tidyverse)


plot(asses_data)

## hypothesis 1: correlation between the two pests ####

#Create a scatterplot of bactrocera.density versus prays.density
ggplot(asses_data, aes(x = prays.density, y = bactrocera.density)) +
  geom_point() +
  labs(x = "prays.density", y = "bactrocera.density")

#Calculate the Pearson's correlation coefficient between bactrocera.density and prays.density
correlation <- cor.test(asses_data$bactrocera.density, asses_data$prays.density)

#Interpret the results
cat("The Pearson's correlation coefficient between bactrocera.density and prays.density is", 
    round(correlation$estimate, 2), "with a p-value of", round(correlation$p.value, 2), 
    "\n")

##Based on the result, the Pearson's correlation coefficient between bactrocera.density and prays.density is 0.21 and the p-value is 0, 
#which suggests that there is a weak positive relationship between the two variables. 
#However, the relationship is not significant because the p-value is less than 0.05.

## 2. Hypothesis: Hypothesis: The density of Bactrocera is related to the number of insecticides used and the type of cultivar being grown.
#Method: Two-way ANOVA.####

#Create a model that includes BactroceraDensity as the response variable and BactroceraInsecticides and Cultivar as predictor variables.
#Conduct a two-way ANOVA to test the effect of BactroceraInsecticides and Cultivar on BactroceraDensity.
#Interpret the results. If both predictor variables are significant, it supports the hypothesis that the density of Bactrocera is related to both the number of insecticides used and the type of cultivar being grown.

colnames(asses_data)

library(tidyverse)

model_1 <- lm(asses_data$bactrocera.density ~ asses_data$bactrocera.insecticides + asses_data$cultivar, data = asses_data)

summary(model_1)


##The summary of the linear model shows that the coefficients of the variables bactrocera.insecticides and cultivar (picual, uk) are significant in explaining the variation in bactrocera.density. 
#The residual standard error is 2.525 and multiple R-squared is 0.1589, meaning that the model accounts for approximately 16% of the total variability in bactrocera.density.

##The p-value for the bactrocera.insecticides variable is very low (p-value < 2.2e-16), indicating strong evidence against the null hypothesis that the coefficient for this variable is 0, 
#i.e., there is a significant positive relationship between bactrocera.insecticides and bactrocera.density. 
##The p-value for cultivar (picual, uk) is 0.0999 and 0.1320, respectively, 
#indicating weak evidence against the null hypothesis of no relationship between cultivar and bactrocera.density.

model_2 <- aov(asses_data$bactrocera.density ~ asses_data$bactrocera.insecticides + asses_data$cultivar + asses_data$bactrocera.insecticides: asses_data$cultivar, data = asses_data)

summary(model_2)

# a significant effect on bactrocera.density (p-value = 5.03e-13, which is smaller than 0.05), as it has the highest F-value of 56.863. 
# The interaction term between bactrocera.insecticides and cultivar is not significant (p-value = 0.572). 
# The factor cultivar is not significant (p-value = 0.258).

## hypothesis 3:examine the relationship between field size and Prays density ####

model_3 <- lm(asses_data$prays.density ~ asses_data$field.size, data = asses_data)
summary(model_3)

#This is the output of a linear regression model where the dependent variable is "prays.density" and the independent variable is "field.size". 
#The model has an adjusted R-squared of -0.004317, indicating a weak relationship between the two variables. 
#The p-value for the field size coefficient is 0.9583, which is not significant and suggests that the field size does not have a significant effect on the prays density.

## hypothesis 4: Here is the code for running a linear regression model to investigate the relationship between altitude and bactrocera density:####

model_altitude <- lm(asses_data$bactrocera.density ~ asses_data$altitude, data = asses_data)
summary(model_altitude)

#The regression model between "bactrocera.density" and "altitude" has the following output:

#Coefficients:
  #Intercept: -1.8013408
#Altitude: 0.0082280

#The coefficient of altitude (0.0082280) is positive and significant (p-value < 2e-16), 
#which means as the altitude increases, the bactrocera.density is expected to increase. 
#The intercept (-1.8013408) represents the expected bactrocera.density when altitude is 0.

#The residual standard error of the model is 2.277 and the multiple R-squared value is 0.3119, 
#which means that about 31.19% of the variance in bactrocera.density can be explained by the altitude.
