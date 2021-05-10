# Eric

library(tidyverse)
library(gganimate)

date_cumulativekm <- read_csv("data/date_cumulativekm.csv")

date_cumulativekm_dailykm <- date_cumulativekm %>%
  mutate(DAILY_KM = ifelse(is.na(lag(CUMULATIVE_KM)), 0, CUMULATIVE_KM - lag(CUMULATIVE_KM)))

animate <- ggplot(data = date_cumulativekm_dailykm, aes(x = DAY, y = CUMULATIVE_KM)) +
  geom_line() +
  geom_vline(xintercept = 31, linetype = "dashed") +
  geom_hline(yintercept = 1000, linetype = "dashed") +
  xlim(0, 31) +
  ylim(0, 1000) +
  labs(title = "Daily and Cumulative Km by Challenge Day", x = "Challenge Day", y = "KM") +
  transition_reveal(DAY) +
  geom_col(width = 0.2, aes(group=seq_along(DAY), y = DAILY_KM))

animate(animate, renderer=gifski_renderer(), nframes = 20, duration = 20, fps = 10, end_pause = 60, rewind = FALSE)
