# Partisanship and COVID analysis
# Author: Peter Zhang

# imports
library("jtools")
library("ggplot2")
library("ggpubr")
library("knitr")
library("papeR")

# EDA

# read state-level data
state_data <- read.csv("agglomerated/state_dataset.csv")

# calculate case rates
state_data$june_caserate <- state_data$June_2020_Cases / state_data$Total.Population * 100000
state_data$june_deathrate <- state_data$June_2020_Deaths / state_data$Total.Population * 100000
state_data$oct_caserate <- state_data$Oct_2020_Cases / state_data$Total.Population * 100000
state_data$oct_deathrate <- state_data$Oct_2020_Deaths / state_data$Total.Population * 100000

# calculate changes
state_data$change_caserate <- state_data$oct_caserate - state_data$june_caserate
state_data$change_deathrate <- state_data$oct_deathrate - state_data$june_deathrate

# june plot
june_plot <- ggplot(data = state_data,
       aes(x = june_caserate,
           y = june_deathrate,
           color = Biden.Margin)) +
  geom_point() +
  ggtitle("June Case/Death Rate")+
  xlab("Cases per 100K") +
  ylab("Deaths per 100K") +
  scale_color_gradient2(name="Biden Margin",low = "red", mid="grey", high = "blue",  midpoint = 0) +
  theme(legend.position = "none")

# october plot
oct_plot <- ggplot(data = state_data,
                    aes(x = oct_caserate,
                        y = oct_deathrate,
                        color = Biden.Margin)) +
  geom_point() +
  ggtitle("October Case/Death Rate")+
  xlab("Cases per 100K") +
  ylab("Deaths per 100K") +
  scale_color_gradient2(low = "red", mid="grey", high = "blue",  midpoint = 0)+
  theme(legend.position = "bottom")

# combine
ggarrange(june_plot, oct_plot, nrow=1, common.legend = TRUE, legend="bottom")

# case increase plot
case_plot <- ggplot(data = state_data,
                    aes(x = Biden.Margin,
                        y = change_caserate,
                        color = Biden.Margin)) +
  geom_point() +
  ggtitle("Change in Case Rate")+
  xlab("Biden Margin") +
  ylab("Cases per 100K") +
  scale_color_gradient2(name="Biden Margin",low = "red", mid="grey", high = "blue",  midpoint = 0) +
  theme(legend.position = "none") +
  geom_smooth(method="lm", se=F, col="black")

# death increase plot
death_plot <- ggplot(data = state_data,
                    aes(x = Biden.Margin,
                        y = change_deathrate,
                        color = Biden.Margin)) +
  geom_point() +
  ggtitle("Change in Death Rate")+
  xlab("Biden Margin") +
  ylab("Deaths per 100K") +
  scale_color_gradient2(name="Biden Margin",low = "red", mid="grey", high = "blue",  midpoint = 0) +
  theme(legend.position = "none") +
  geom_smooth(method="lm", se=F, col="black")


# combine
ggarrange(case_plot, death_plot, nrow=1, common.legend = TRUE, legend="bottom")

# case correlation
cor(state_data$Biden.Margin, state_data$change_caserate)

# death correlation
cor(state_data$Biden.Margin, state_data$change_deathrate)