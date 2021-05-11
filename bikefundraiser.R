# Program:    bikefundraiser01.R
# Programmer: Eric Simms
# Version:    1.0
# Date:       11May2021
#
# Purpose:    Create an animated gif showing km per day and cumulative km.
#
# Input:      data/data_cumulativekm.csv
# Output:     Animated gif

# Load packages
library(tidyverse)
library(gganimate)
library(gifski)
library(png)

# Import data
date_cumulativekm <- read_csv("data/date_cumulativekm.csv")

# Derive km per day
date_cumulativekm_dailykm <- date_cumulativekm %>%
  mutate(DAILY_KM = ifelse(is.na(lag(CUMULATIVE_KM)), 0, CUMULATIVE_KM - lag(CUMULATIVE_KM)))

# Create series of graphs to be animated
theme_set(theme_classic())
animate <- ggplot(data = date_cumulativekm_dailykm, aes(x = DAY, y = CUMULATIVE_KM)) +
  geom_col(aes(group=seq_along(DAY), y = DAILY_KM, color = "red"), width = 0.3, show.legend = FALSE) +
  geom_line(aes(color = "green"), size = 2, show.legend = FALSE) +
  geom_vline(xintercept = 31, linetype = "dashed") +
  geom_hline(yintercept = 1000, linetype = "dashed") +
  xlim(0, 31) +
  ylim(0, 1000) +
  expand_limits(x = 0, y = 0) +
  transition_reveal(DAY) +
  labs(title = "Daily and Cumulative Km by Challenge Day",
       subtitle = "Day: {floor(frame_along)}",
       x = "Challenge Day", y = "Km")

# Create animated gif
animate(animate, renderer=gifski_renderer(), nframes = 20, duration = 20, fps = 10, end_pause = 60, rewind = FALSE)
