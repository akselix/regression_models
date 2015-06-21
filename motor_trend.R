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
mt <- mutate(mt, am = ifelse(mt$am == 0, 'automatic', mt$am))
mt <- mutate(mt, am = ifelse(mt$am == 1, 'manual', mt$am))
mt <- mutate(mt, am = as.factor(mt$am))

# Pairs plots to see correllations between all the variables
plot1 <- ggpairs(data = mt,
        colour = "am")

# Boxplot of transmission type versus mpg
plot2 <- ggplot(data = mt, aes(x = am, y = mpg, fill = am )) +
    geom_boxplot()
    scale_fill_manual(name = 'Transmission',
                      values = c('pink', 'green'),
                      breaks = c(0,1),
                      labels = c('Automatic','Manual'))

mtSummary <- mt %>%
            group_by(am)%>%
            summarise(count = n(),
                      meanMpg = mean(mpg),
                      medianMpg = median(mpg),
                      sdMpg = sd(mpg)
                      )

