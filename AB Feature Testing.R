#-------------------------------------------------------------------------
#Author: Akanksha Mishra
#Topic: A/B Testing for Product Feature
#-------------------------------------------------------------------------

library(dplyr)
library(tidyverse)
library(ggplot2)
library(broom)
library(readr)

# -------------- 1. Load Data --------------------------------------------
setwd("C:/Users/mishr/Desktop/PlayQ")
data <- read.csv("PlayQdata.csv")
data <- data %>% rename(user_id = ï..user_id)
head(data)

# -------------- 2. Exploratory Data Analysis -----------------------------

# 2a) Overall Summary
summary(data)

# 2b) Player Joining 
joining_date<- data %>%
  group_by(experiment_selected_date) %>%
  summarise(players = n_distinct(user_id))
ggplot(data=joining_date, aes(x=experiment_selected_date, y=players))+
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=players), vjust=-0.3, size=3.5)+
  theme_minimal()+
  labs(x = "Day", y = "# Joining Players")

# 2c) Activity Distribution 
activity_dist<- data %>%
  filter(experiment_selected_date == '1/17/2018')%>%
  group_by(event_date,feature_enabled) %>%
  summarise(players = n_distinct(user_id))
activity_dist$feature_enabled <- factor(activity_dist$feature_enabled)
ggplot(data=activity_dist, aes(x=event_date, y=players, fill=feature_enabled)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=players), vjust=1.6, color="black",
            position = position_dodge(0.9), size=3.5) +
  theme_minimal()+
  labs(x = "Day", y = "# Active Players")

# 2d. Overall Summary
overall_summary<- data %>%
  filter(experiment_selected_date == '1/17/2018')%>%
  group_by(feature_enabled) %>%
  summarise(players = n_distinct(user_id),
            player_perc = n_distinct(user_id)/1040,
            avg_activity = mean(games_played),
            tot_activity = sum(games_played),
            avg_revenue = mean(revenue),
            tot_revenue = sum(revenue),
            avg_activedayes = n_distinct(event_date))

# 2e. Compute daily summary by joining date
daily_summary<- data %>%
  filter(experiment_selected_date == '1/17/2018')%>%
  group_by(event_date,feature_enabled) %>%
  summarise(players = n_distinct(user_id),
            avg_activity = mean(games_played), 
            avg_revenue = mean(revenue),
            player_perc = n_distinct(user_id)/1040 )
daily_summary$feature_enabled <- factor(daily_summary$feature_enabled)

ggplot(daily_summary,
       aes(x = event_date,
           y = avg_activity,
           color = feature_enabled,
           group = feature_enabled)) +
  geom_point(size = 2) +
  geom_line(lwd =1 )+
  geom_text(aes(label=round(avg_activity,0)),vjust=1.6)+
  labs(x = "Day",y = "Avg. Games Played")
 
ggplot(daily_summary,
       aes(x = event_date,
           y = avg_revenue,
           color = feature_enabled,
           group = feature_enabled)) +
  geom_point(size = 2) +
  geom_line(lwd =1 )+
  geom_text(aes(label=round(avg_revenue,2)),vjust=1.6)+
  labs(x = "Day", y = "Avg. Revenue")

# --------------------- 3. Power Analysis -----------------------------------

install.packages("pwr")
library(pwr)
pwr.t.test(d = 0.1 ,sig.level = 0.05, power = 0.9)
pwr.t.test(d = 0.2 ,sig.level = 0.05, power = 0.9)
pwr.t.test(d = 0.1 ,sig.level = 0.05, power = 0.8)
pwr.t.test(d = 0.2 ,sig.level = 0.05, power = 0.8)

# --------------------- 4. Analysis = T-test ---------------------------------

# Player Summary Data Set
experiment_days<- c('1/17/2018', '1/18/2018','1/19/2018', '1/20/2018', '1/21/2018','1/22/2018','1/23/2018','1/24/2018')

player_summary<- data %>%
  filter( experiment_selected_date %in% c('1/17/2018' ,'1/16/2018'),
          event_date %in% experiment_days) %>%
  group_by(user_id,feature_enabled) %>%
  summarise(days = n_distinct(event_date),
            games = sum(games_played), 
            revenue = sum(revenue))


ab_test_days<- t.test(days ~ feature_enabled, data = player_summary)
tidy()
ab_test_days

ab_test_games<- t.test(games ~ feature_enabled, data = player_summary)
tidy()
ab_test_games

ab_test_revenue<- t.test(revenue ~ feature_enabled, data = player_summary)
tidy()
ab_test_revenue


# Testing again on whole player Data Set 

whole_player_summary<- data %>%
  filter(event_date %in% experiment_days) %>%
  group_by(user_id,feature_enabled) %>%
  summarise(days = n_distinct(event_date),
            games = sum(games_played), 
            revenue = sum(revenue))

ab_test_days_2<- t.test(days ~ feature_enabled, data = whole_player_summary)
tidy()
ab_test_days_2

ab_test_games_2<- t.test(games ~ feature_enabled, data = whole_player_summary)
tidy()
ab_test_games_2

ab_test_revenue_2<- t.test(revenue ~ feature_enabled, data = whole_player_summary)
tidy()
ab_test_revenue_2









