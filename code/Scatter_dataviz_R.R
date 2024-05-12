library(tidyverse)
data()
?mpg

mpg %>% glimpse()

# Group by linear smoothing
mpg %>%
  filter(hwy < 35) %>%
  ggplot(
    aes(
      x = displ,
      y = hwy,
      color =drv
    )
  )+
  geom_point()+
  geom_smooth(
    method = lm,
    se = FALSE)+
  labs(
    x = "Engine size",
    y = "MPG on the Highway",
    title = "Fuel efficiency"
  )+
  theme_minimal()

# Global loess
mpg %>%
  filter(hwy < 35) %>%
  ggplot(
    aes(
      x = displ,
      y = hwy
    )
  )+
  geom_point(
    aes(
      color = drv
    )
  )+
  geom_smooth(
    method = loess,
    se = TRUE)+
  labs(
    x = "Engine size",
    y = "MPG on the Highway",
    title = "Fuel efficiency"
  )+
  theme_minimal()


msleep |>
  filter(bodywt < 2) |>
  ggplot(
    aes(
      bodywt,
      brainwt
    )
  )+
  geom_point(
    aes(
      color = sleep_total,
      size = awake
    )
  )+
  geom_smooth()+
  labs(
    x = "Body Weight",
    y = "Brain Weight",
    title = "Brain and body weight"
  )+
  theme_minimal()
  
























