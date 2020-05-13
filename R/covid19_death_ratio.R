# install the package and load the df
install.packages("devtools")
install.packages("dplyr")

#to be updated daily
library(devtools)
devtools::install_github("covid19r/coronavirus", force = TRUE)
library(coronavirus)
data("coronavirus")
head(coronavirus)

# some summary statistics
library(dplyr)

summary_df <- coronavirus %>% group_by(Country.Region, type) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases)

summary_df %>% head(20)

# where is bulgaria? how many have been confirmed, died or recovered
bul <- grep('Bulg.*', summary_df$Country.Region)
summary_df[bul,]

bul_df <- coronavirus %>% filter(Country.Region == "Bulgaria")

# graph for all cases
# make it one integer
total_df <- summary_df %>% group_by(type) %>% filter(type == "confirmed") %>% summarise(sum(total_cases))
total <- total_df$`sum(total_cases)`

library(ggplot2)
library(ggthemes)
all_running_total <- coronavirus %>%
  select(date, cases, type) %>%
  group_by(type, date) %>%
  summarise(total_cases = sum(cases)) %>%
  ungroup() %>%
  group_by(type) %>%
  mutate(running_total = cumsum(total_cases)) %>%
  ungroup()

all_running_total %>% ggplot(aes(date, running_total, colour = type)) +
  geom_line(size = 1.5) +
  geom_point(size = 2) +
  scale_x_date(date_breaks = "1 week") +
  scale_y_continuous(breaks = seq(0,max(all_running_total$running_total), 100000), 
                     expand = c(0,0),
                     limits = c(0,max(all_running_total$running_total)+50000)) +
  scale_color_manual(values = c("dark blue", "dark red", "dark green")) + 
  theme_bw() +
  labs(title = "The cumulative total confirmed cases of COVID-19",
       subtitle = paste("There've been", total, "confirmed cases in the world"),
       x = "",
       y = "Running Total Cases",
       caption = "(data based on Remi Krispin package \"coronavirus\")") +
  theme(plot.title = element_text(18)) +
  theme(plot.subtitle = element_text(16)) +
  theme(legend.text = element_text(14)) +
  theme(legend.title = element_blank()) + 
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# let's animate the coronavirus death rate per country
# first part of preparation
corona_rates <- coronavirus %>%
  select(Country.Region, date, cases, type) %>%
  group_by(Country.Region, date) %>%
  summarise(total_cases = sum(cases),
            deaths = sum(cases[type == "death"]),
            recovories = sum(cases[type == "recovered"])) %>% 
  ungroup() %>%
  group_by(Country.Region) %>%
  mutate(running_total = cumsum(total_cases),
         running_total_deaths = cumsum(deaths),
         running_total_recovories = cumsum(recovories),
         death_rate = round((running_total_deaths/running_total)*100, 2),
         recovory_rate = round((running_total_recovories/running_total)*100, 2)) %>%
  ungroup() %>% 
  filter(running_total >= 50)

# second part of preparation
corona_formatted <- corona_rates %>%
  group_by(date) %>%
  # The na.last = "keep" makes it, that if a death_rate is NA to leave it as such in the rank, so the lenght is the same
  # The * 1 makes it possible to have non-integer ranks while sliding
  # we rank it by -death_rate so the first rank is with the highest death rate, correspondingly when it is 
  # devided it would be some number (I mean that if we left it for top 10 countries with lowest mortality it will devide by 0 for some)
  mutate(rank = dense_rank(-death_rate), 
         Value_rel = death_rate/death_rate[rank==1],
         Value_lbl = paste0(" ",death_rate)) %>%
  group_by(Country.Region) %>% 
  filter(rank <=10) %>%
  ungroup()

library(tidyverse)
# install.packages("gganimate")
library(gganimate)
library(scales)

staticplot <- ggplot(corona_formatted[corona_formatted$death_rate > 0, ], aes(rank, group = Country.Region, 
                                       fill = as.factor(Country.Region), color = as.factor(Country.Region))) +
  geom_tile(aes(y = death_rate/2,
                height = death_rate,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Country.Region, " ")), vjust = 0.2, hjust = 1, size = 10) +
  geom_text(aes(y=death_rate, label = Value_lbl, hjust=0), size = 10) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title=element_text(size=30, hjust=0.5, face="bold", colour="black", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=14, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,4, 2, 12, "cm"))

anim <- staticplot + transition_states(date, transition_length = 100, state_length = 100, wrap = TRUE) +
  view_follow(fixed_x = TRUE)  +
  enter_fade() +
  exit_fade() +
  labs(title = 'Covid-10 death rates per day : {closest_state}',  
       subtitle  =  "Highest 10 Countries with more than 50 cases",
       caption  = "Death Rate as Percentage | Data Source: Remi Krispin package \"coronavirus\"")

# install.packages("gifski")
library(gifski)
# some of the below arguments are setting the speed - needs more chechking
animate(anim, 1000, fps = 100,  width = 1200, height = 1000,
        renderer = gifski_renderer("gganim1000_100 - 100 100 1.9.gif"))
