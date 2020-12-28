library(jtools)

data <- read.csv("sources/covid.csv")
data$Income = as.numeric(data$Income)
data$Density = as.numeric(data$Density)
data$Testing = as.numeric(data$Testing)

data$deathIncrease = (data$Oct_2020_Deaths-data$June_2020_Deaths)/data$Total.Population*100000
data$caseIncrease = (data$Oct_2020_Cases-data$June_2020_Cases)/data$Total.Population*100000
data$Oct_2020_DeathRate = data$Oct_2020_Deaths/data$Total.Population*100000
data$Oct_2020_CaseRate = data$Oct_2020_Cases/data$Total.Population*100000
cor(data$deathIncrease, data$Biden_Margin)
cor(data$deathIncrease, data$Income)
data$Boomers = data$Total.number..adults.age.65.and.older/data$Total.Population


linear.1 <- lm(Oct_2020_CaseRate ~ Biden_Margin+ Density + Testing + Income + Boomers + Black + White + Asian + Hispanic,
               data=data)
linear.1 <- lm(Oct_2020_DeathRate ~ Biden_Margin+ Density + Income + Boomers + Black + White + Asian + Hispanic,
               data=data)
linear.1 <- lm(deathIncrease ~ Biden_Margin,
               data=data)
summ(linear.1)


countydata <- read.csv("sources/matched.csv")
countydata$SeptCases = (countydata$Cases.9.30.2020-countydata$Cases.8.31.2020 -(countydata$Cases.7.31.2020-countydata$Cases.6.30.2020))/countydata$Population_2019*100000
countydata$Population.Density
linear.2 = lm(Cases.9.30.2020 ~ diff + Population.Density,
              data = countydata)
summ(linear.2)
