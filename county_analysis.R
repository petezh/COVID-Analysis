# Partisanship and COVID plots and regression analysis
# Author: Peter Zhang

# imports
library("jtools")
library("ggplot2")
library("ggpubr")
library("knitr")
library("papeR")
library("memisc")
library("pander")
library("stargazer")

# read county-level data
county_data <- read.csv("agglomerated/county_dataset.csv")

# create summary table
kable(summarize(county_data), digits=5)

# get monthly case/death rates
county_data$apr_cases = (county_data$X4.30.2020_cases-county_data$X3.31.2020_cases)/county_data$population*100000
county_data$apr_deaths = (county_data$X4.30.2020_deaths-county_data$X4.30.2020_deaths)/county_data$population*100000
county_data$jun_cases = (county_data$X6.30.2020_cases-county_data$X5.31.2020_cases)/county_data$population*100000
county_data$jun_deaths = (county_data$X6.30.2020_deaths-county_data$X5.31.2020_deaths)/county_data$population*100000
county_data$oct_cases = (county_data$X10.31.2020_cases-county_data$X9.30.2020_cases)/county_data$population*100000
county_data$oct_deaths = (county_data$X10.31.2020_deaths-county_data$X9.30.2020_deaths)/county_data$population*100000
county_data$dec_cases = (county_data$X12.31.2020_cases-county_data$X11.30.2020_cases)/county_data$population*100000
county_data$dec_deaths = (county_data$X12.31.2020_deaths-county_data$X11.30.2020_deaths)/county_data$population*100000
county_data$county_margin = county_data$per_dem_2020 - county_data$per_gop_2020

# proportionize
county_data$trips = county_data$trips/county_data$population
county_data$at_home = county_data$at_home/county_data$population

# april plot
apr_case_plot <- ggplot(data = subset(county_data, apr_cases >= 0, select=c(county_margin, apr_cases)),
                        aes(x = county_margin,
                            y = apr_cases,
                            color = county_margin)) +
  geom_point() +
  ggtitle("April Cases")+
  xlab("Biden Margin") +
  ylab("Cases per 100K") +
  scale_color_gradient2(name="Biden Margin",low = "red", mid="grey", high = "blue",  midpoint = 0) +
  theme(legend.position = "none") +
  geom_smooth(method="lm", se=F, col="black")

# june plot
jun_case_plot <- ggplot(data = subset(county_data, jun_cases >= 0, select=c(county_margin, jun_cases)),
                        aes(x = county_margin,
                            y = jun_cases,
                            color = county_margin)) +
  geom_point() +
  ggtitle("June Cases")+
  xlab("Biden Margin") +
  ylab("Cases per 100K") +
  scale_color_gradient2(name="Biden Margin",low = "red", mid="grey", high = "blue",  midpoint = 0) +
  theme(legend.position = "none") +
  geom_smooth(method="lm", se=F, col="black")

# oct plot
oct_case_plot <- ggplot(data = subset(county_data, oct_cases >= 0, select=c(county_margin, oct_cases)),
                        aes(x = county_margin,
                            y = oct_cases,
                            color = county_margin)) +
  geom_point() +
  ggtitle("October Cases")+
  xlab("Biden Margin") +
  ylab("Cases per 100K") +
  scale_color_gradient2(name="Biden Margin",low = "red", mid="grey", high = "blue",  midpoint = 0) +
  theme(legend.position = "none") +
  geom_smooth(method="lm", se=F, col="black")

# dec plot
dec_case_plot <- ggplot(data = subset(county_data, dec_cases >= 0, select=c(county_margin, dec_cases)),
                        aes(x = county_margin,
                            y = dec_cases,
                            color = county_margin)) +
  geom_point() +
  ggtitle("December Cases")+
  xlab("Biden Margin") +
  ylab("Cases per 100K") +
  scale_color_gradient2(name="Biden Margin",low = "red", mid="grey", high = "blue",  midpoint = 0) +
  theme(legend.position = "none") +
  geom_smooth(method="lm", se=F, col="black")


# combine and write
ggarrange(apr_case_plot,
          jun_case_plot,
          oct_case_plot,
          dec_case_plot,
          nrow=2, ncol=2,
          common.legend = TRUE, legend="bottom")  %>%
  ggexport(filename = "resources/month_cases.png",
           width=600,
           height=650)

# correlations
cor(county_data$apr_cases, county_data$county_margin)
cor(county_data$jun_cases, county_data$county_margin)
cor(county_data$oct_cases, county_data$county_margin)
cor(county_data$dec_cases, county_data$county_margin)


# jun plot
jun_death_plot <- ggplot(data = subset(county_data, jun_deaths >= 0, select=c(county_margin, jun_deaths)),
                         aes(x = county_margin,
                             y = jun_deaths,
                             color = county_margin)) +
  geom_point() +
  ggtitle("June Deaths")+
  xlab("Biden Margin") +
  ylab("Deaths per 100K") +
  scale_color_gradient2(name="Biden Margin",low = "red", mid="grey", high = "blue",  midpoint = 0) +
  theme(legend.position = "none")+
  geom_smooth(method="lm", se=F, col="black")


# apr plot
oct_death_plot <- ggplot(data = subset(county_data, oct_deaths >= 0, select=c(county_margin, oct_deaths)),
                         aes(x = county_margin,
                             y = oct_deaths,
                             color = county_margin)) +
  geom_point() +
  ggtitle("October Deaths")+
  xlab("Biden Margin") +
  ylab("Deaths per 100K") +
  scale_color_gradient2(name="Biden Margin",low = "red", mid="grey", high = "blue",  midpoint = 0) +
  theme(legend.position = "none")+
  geom_smooth(method="lm", se=F, col="black")

# dec plot
dec_death_plot <- ggplot(data =  subset(county_data, dec_deaths >= 0, select=c(county_margin, dec_deaths)),
                         aes(x = county_margin,
                             y = dec_deaths,
                             color = county_margin)) +
  geom_point() +
  ggtitle("December Deaths")+
  xlab("Biden Margin") +
  ylab("Deaths per 100K") +
  scale_color_gradient2(name="Biden Margin",low = "red", mid="grey", high = "blue",  midpoint = 0) +
  theme(legend.position = "none")+
  geom_smooth(method="lm", se=F, col="black")

# combine and write
ggarrange(jun_death_plot,
          oct_death_plot,
          dec_death_plot,
          nrow=2, ncol=2,
          common.legend = TRUE, legend="bottom")  %>%
  ggexport(filename = "resources/month_deaths.png",
           width=600,
           height=650)

# correlations
cor(county_data$jun_deaths, county_data$county_margin)
cor(county_data$oct_deaths, county_data$county_margin)
cor(county_data$dec_deaths, county_data$county_margin)

# compare with 2016
cor(county_data$dec_cases, county_data$per_dem_2016-county_data$per_gop_2016)
cor(county_data$dec_deaths, county_data$per_dem_2016-county_data$per_gop_2016)

# compare with states
cor(county_data$dec_cases, county_data$state_margin)
cor(county_data$dec_deaths, county_data$state_margin)

# mobility vs cases
mobility_plot <- ggplot(data = county_data,
                         aes(x = trips,
                             y = dec_cases,
                             color = county_margin)) +
  geom_point() +
  ggtitle("Dec. Cases and Mobility")+
  xlab("Trips Per Person") +
  ylab("Cases per 100K") +
  scale_color_gradient2(name="Biden Margin",low = "red", mid="grey", high = "blue",  midpoint = 0) +
  theme(legend.position = "none")+
  geom_smooth(method="lm", se=F, col="black")

# mask use
maskuse_plot <- ggplot(data = subset(county_data),
       aes(x = mask_use,
           y = dec_cases,
           color = county_margin)) +
  geom_point() +
  ggtitle("Dec. Cases and Masking")+
  xlab("Mask Use Proportion") +
  ylab("Cases per 100K") +
  scale_color_gradient2(name="Biden Margin",low = "red", mid="grey", high = "blue",  midpoint = 0) +
  theme(legend.position = "none")+
  geom_smooth(method="lm", se=F, col="black")


# combine and write
ggarrange(mobility_plot,
          maskuse_plot,
          nrow=1,
          common.legend = TRUE, legend="bottom")  %>%
  ggexport(filename = "resources/mobility_maskuse.png",
           width=600,
           height=350)


# measures
cor(county_data$trips/county_data$population, county_data$dec_cases)
cor(county_data$mask_use, county_data$dec_cases)

# features
county_data$elderly = county_data$X60older/county_data$population*100
county_data$icu_rate = county_data$icus/county_data$population*100000
county_data$income = county_data$median_household/1000
county_data$density = county_data$density/1000
county_data$work_distance = county_data$distance_to_work

# cases linear models
lm_cases_1 <- lm(dec_cases ~ tests + density +
               income + elderly + black + hispanic + lattitude + work_distance + household_size,
               data = county_data)
summ(lm_cases_1)

lm_cases_2 <- lm(dec_cases ~ county_margin + tests + density +
                   income + elderly + black + hispanic + lattitude + work_distance + household_size,
                 data = county_data)
summ(lm_cases_2)


lm_cases_3 <- lm(dec_cases ~ county_margin + state_margin + county_margin * state_margin+
                   tests + density +
                   income + elderly + black +  hispanic + lattitude + work_distance + household_size,
               data=county_data)
summ(lm_cases_3)

lm_cases_4 <- lm(dec_cases ~ county_margin +  mask_use + trips + at_home +
                   tests + density +
                   income + elderly + black + hispanic + lattitude + work_distance + household_size,
                 data=county_data)
summ(lm_cases_4)

# write to html
cases_table <- mtable('Model 1' = lm_cases_1,
             'Model 2' = lm_cases_2,
             'Model 3' = lm_cases_3,
             'Model 4' = lm_cases_4,
             summary.stats = c('R-squared', 'adj. R-squared', 'F','p'))
write_html(cases_table, "resources/case_reg.html")


# deaths linear models
lm_deaths_1 <- lm(dec_deaths ~ density +
                    income + elderly + black + hispanic + lattitude + work_distance + household_size,
                  data = county_data)
summ(lm_deaths_1)


lm_deaths_2 <- lm(dec_deaths ~ county_margin + density +
                 income + elderly + black  + hispanic + lattitude + work_distance + household_size,
               data = county_data)
summ(lm_deaths_2)

lm_deaths_3 <- lm(dec_deaths ~ county_margin + state_margin + county_margin * state_margin+ density +
                    income + elderly + black + hispanic + lattitude + work_distance + household_size,
                 data=county_data)
summ(lm_deaths_3)

lm_deaths_4 <- lm(dec_deaths ~ county_margin + mask_use + trips + at_home + icu_rate+ density +
                    income + elderly + black + hispanic + lattitude + work_distance + household_size,
                 data=county_data)
summ(lm_deaths_4)

# write to html
deaths_table <- mtable('Model 5' = lm_deaths_1,
                      'Model 6' = lm_deaths_2,
                      'Model 7' = lm_deaths_3,
                      'Model 8' = lm_deaths_4,
                      summary.stats = c('R-squared', 'adj. R-squared', 'F','p'))
write_html(deaths_table, "resources/death_reg.html")




# cases linear models
lm_cases_1 <- lm(dec_cases ~ density + income + elderly + black + hispanic,
                 data = county_data)
summ(lm_cases_1)

lm_cases_2 <- lm(dec_cases ~ county_margin + density + income + elderly + black + hispanic,
                 data = county_data)
summ(lm_cases_2)


lm_cases_3 <- lm(dec_cases ~ county_margin + mask_use + trips + density + income + elderly + black + hispanic,
                 data = county_data)
summ(lm_cases_3)



# deaths linear models
lm_deaths_1 <- lm(dec_deaths ~ density +
                    income + elderly + black + hispanic,
                  data = county_data)
summ(lm_deaths_1)


lm_deaths_2 <- lm(dec_deaths ~ county_margin + density +
                    income + elderly + black  + hispanic,
                  data = county_data)
summ(lm_deaths_2)

lm_deaths_3 <- lm(dec_deaths ~ county_margin + density + mask_use + trips +
                    income + elderly + black + hispanic,
                  data=county_data)
summ(lm_deaths_3)

stargazer(lm_cases_1, lm_cases_2, lm_cases_3, lm_deaths_1, lm_deaths_2, lm_deaths_3, digits=2)
