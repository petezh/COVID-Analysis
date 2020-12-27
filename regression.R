install.packages("jtools")
library(jtools)

data <- read.csv("sources/covid.csv")
data$Income = as.numeric(data$Income)

data$deathIncrease = (data$Oct_2020_Deaths-data$June_2020_Deaths)/data$Total.Population*100000
data$Oct_2020_DeathRate = data$Oct_2020_Deaths/data$Total.Population*100000
cor(data$deathIncrease, data$Biden_Margin)
cor(data$deathIncrease, data$Income)
data$Boomers = data$Total.number..adults.age.65.and.older/data$Total.Population

linear.1 <- lm(Oct_2020_DeathRate ~ Biden_Margin + Income + Boomers,
               data=data)
summ(linear.1)
