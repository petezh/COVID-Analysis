
# case correlation
cor(state_data$Biden.Margin, state_data$change_caserate)

# death correlation
cor(state_data$Biden.Margin, state_data$change_deathrate)

# read county-level data
county_data <- read.csv("agglomerated/county_dataset.csv")

# create summary table
kable(summarize(county_data))


county_data$caserate = (county_data$X12.31.2020_cases-county_data$X11.30.2020_cases)/county_data$population*100000
county_data$deathrate = county_data$X12.31.2020_deaths/county_data$population*100000
county_data$elderly = county_data$X60older/county_data$population
county_data$icu_rate = county_data$icus/county_data$population


ggplot(data = county_data,
       aes(x = per_dem_2020 - per_gop_2020,
           y = caserate,
           color = per_dem_2020 - per_gop_2020)) +
  geom_point() +
  ggtitle("Change in Death Rate")+
  xlab("Biden Margin") +
  ylab("Deaths per 100K") +
  scale_color_gradient2(name="Biden Margin",low = "red", mid="grey", high = "blue",  midpoint = 0) +
  theme(legend.position = "none") +
  geom_smooth(method="lm", se=F, col="black")

linear.1 <- lm(dem_caserate~ per_dem_2020 + maskuse + X60older/population + density + total_deaths/population + lattitude + distance_to_work + per_capita_income + median_household + black + hispanic + bachelors + poverty + tests,
               data=county_data)
summ(linear.1)

linear.2 <- lm(dem_caserate ~ per_dem_2020 + distance_to_work,
               data=county_data)

linear.3 <- lm(deathrate/caserate ~ black + bachelors +  density + elderly + icu_rate,
               data=county_data)
summ(linear.3)

ggplot(county_data,
       aes(x=deathrate/caserate,
           y=icu_rate)) +
  geom_point() +
  geom_smooth(method="lm", se=F, col="black")




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
