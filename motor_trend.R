# motor_trends.R
# 2015-06-21

# Libraries and options

library(knitr)
library(dplyr)
library(ggplot2)
library(GGally)

# Load and transform data
mt <- tbl_df(mtcars)

# Exploratory analysis

## Take a looka at the variables
str(mt)

## Change some variables to factor for easier plotting
mt <- mt %>%
    mutate(cyl = as.factor(mt$cyl),
           vs = as.factor(mt$vs),
           gear = as.factor(mt$gear),
           carb = as.factor(mt$carb)
          )

# Change the am column (transmission type) to textual form
mt$transmission <- NA
mt <- mutate(mt, transmission = ifelse(mt$am == 0, 'automatic', mt$transmission))
mt <- mutate(mt, transmission = ifelse(mt$am == 1, 'manual', mt$transmission))
mt <- mutate(mt, transmission = as.factor(mt$transmission))

# Pairs plots to see correllations between all the variables
plot1 <- ggpairs(data = mt,
        colour = "transmission")

# Boxplot of transmission type versus mpg
plot2 <- ggplot(data = mt, aes(x = transmission, y = mpg, fill = transmission)) +
    geom_boxplot() +
    labs(title = 'Miles per Gallon with Automatic or Manual Transmission')
  
# Summarise main statistics on mpg against am
transmissionSummary <- mt %>%
            group_by(transmission)%>%
            summarise(count = n(),
                      meanMpg = round(mean(mpg), 1),
                      medianMpg = median(mpg),
                      sdMpg = round(sd(mpg), 1)
                      )

# Regression analysis with linear model

## Drop the textual transmission variable and use factor am instead
mt2 <- select(mt, -(transmission))
mt2 <- mutate(mt2, am = as.factor(mt2$am))

initialModel <- lm(mpg ~ ., data = mt2)
summary(initialModel)

bestModel <- step(initialModel, direction = 'both')
summary(bestModel)

amModel <- lm(mpg ~ am, data = mt2)
summary(amModel)

## anova
anova(amModel, bestModel)

## t-test
t.test(mpg ~ am, data = mt2)
