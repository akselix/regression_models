# motor_trends.R
# 2015-06-21

# Libraries and options

library(knitr)
library(dplyr)
library(ggplot2)
library(GGally)
library(gridExtra)

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

## Drop the textual transmission variable and use factor am instead
mt2 <- select(mt, -(transmission))
mt2 <- mutate(mt2, am = as.factor(mt2$am))

# Pairs plots to see correllations between all the variables
plot1 <- ggpairs(data = mt2,
        colour = 'am')

# Boxplot of transmission type versus mpg
plot2 <- ggplot(data = mt, aes(x = transmission, y = mpg, fill = transmission)) +
    geom_boxplot() +
    labs(title = 'Miles per Gallon with Automatic or Manual Transmission')
  
## Summarise main statistics on mpg against am
transmissionSummary <- mt %>%
            group_by(transmission)%>%
            summarise(count = n(),
                      meanMpg = round(mean(mpg), 1),
                      medianMpg = median(mpg),
                      sdMpg = round(sd(mpg), 1)
                      )

# Regression analysis with linear model

## t-test
t.test(mpg ~ am, data = mt2)

## Model building
initialModel <- lm(mpg ~ ., data = mt2)
summary(initialModel)
confint(bestModel, level = 0.95)

bestModel <- step(initialModel, direction = 'both')
summary(bestModel)

amModel <- lm(mpg ~ am, data = mt2)
summary(amModel)
confint(amModel, level = 0.95)



## anova
anova(amModel, bestModel)


## Residual analysis
bestRes <- resid(bestModel)
bestFit <- bestModel$fitted.values

amRes <- resid(amModel)
amFit <- amModel$fitted.values

mtRes <- data.frame(mt$mpg, bestFit,bestRes, amFit, amRes)

### Plot actual mpg against residuals from both models
plot3 <- ggplot(data = mtRes, aes(x = mt.mpg, y = bestRes)) +
    geom_point() +
    stat_smooth() +
    labs(title = 'Actual MPG and Best Model`s Residuals', x = 'Actual MPG', y = 'Residuals')

plot4 <- ggplot(data = mtRes, aes(x = mt.mpg, y = amRes)) +
    geom_point() +
    stat_smooth() +
    labs(title = 'Actual MPG and Transmission Type Model`s Residuals', x = 'Actual MPG', y = 'Residuals')

plot5 <- ggplot(data = mtRes, aes(x = mt.mpg, y = bestFit)) +
    geom_point() +
    stat_smooth() +
    labs(title = 'Actual MPG and Best Model`s Fitted Values', x = 'Actual MPG', y = 'Fitted Values')

plot6 <- ggplot(data = mtRes, aes(x = mt.mpg, y = amFit)) +
    geom_point() +
    stat_smooth() +
    labs(title = 'Actual MPG and Transmission Type Model`s Fitted Values', x = 'Actual MPG', y = 'Fitted Values')

plots3to6 <- grid.arrange(plot3, plot4, plot5, plot6, ncol = 2)