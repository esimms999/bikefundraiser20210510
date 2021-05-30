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

# Derive km per day and a character value for cumulative km
date_cumulativekm_dailykm <- date_cumulativekm %>%
  mutate(DAILY_KM = ifelse(is.na(lag(CUMULATIVE_KM)), 0, CUMULATIVE_KM - lag(CUMULATIVE_KM))) %>%
  mutate(CUMULATIVE_KM_CHAR = ifelse(CUMULATIVE_KM < 30, "", as.character(CUMULATIVE_KM)))

# Create series of graphs to be animated
theme_set(theme_classic())
animate <- ggplot(data = date_cumulativekm_dailykm, aes(x = DAY, y = CUMULATIVE_KM)) +
  geom_col(aes(group=seq_along(DAY), y = DAILY_KM, color = "red"), width = 0.3, show.legend = FALSE) +
  geom_line(aes(color = "green"), size = 2, show.legend = FALSE) +
  geom_point() +
  geom_text(aes(label=CUMULATIVE_KM_CHAR, vjust= -2), show.legend = FALSE) +
  geom_vline(xintercept = 31, linetype = "dashed") +
  geom_hline(yintercept = 1000, linetype = "dashed") +
  annotate("text", x = 5, y = 1000, label = "Challenge: 1000 Km", vjust = -0.5) +
  geom_abline(slope = 1000/31, linetype = "dotted") +
  coord_cartesian(xlim = c(0,31), ylim = c(0, 1400), expand = FALSE) +
  transition_reveal(DAY) +
  labs(title = "Daily and Cumulative Km by Challenge Day",
       subtitle = "Day: {floor(frame_along)}",
       x = "Challenge Day", y = "Km")

# Create animated gif
animate(animate, renderer=gifski_renderer(), nframes = 20, duration = 20, fps = 10, end_pause = 60, rewind = FALSE)
anim_save("fileec4527ed1d.gif", path = "output")

