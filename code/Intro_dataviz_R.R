library(tidyverse)
data()
?BOD

BOD %>%
  ggplot(
    aes(
      x =Time,
      y = demand
    )
  )+
  geom_point(size = 3)+
  geom_line(color = "red")

CO2 %>% glimpse()
CO2 %>%
  ggplot(
    aes(
      x = conc,
      y = uptake,
      color = Treatment,
    )
  )+
  geom_point(size = 3,
             alpha = 0.5)+
  geom_smooth(
    method = lm,
    se = FALSE
    )+
  facet_wrap(.~Type)+
  labs(
    title = "Concentration of CO2"
  )+
  theme_bw()

CO2 %>%
  ggplot(
    aes(
      x = Treatment,
      y = uptake
    )
  )+
  geom_boxplot()+
  geom_point(
    alpha = 0.5,
    aes(
      size = conc,
      color = Plant
    )
  )+
  facet_wrap(~Type)+
  coord_flip()+
  theme_bw()+
  labs(
    title = "Cilled vs Non chilled"
  )
  
mpg %>%
  filter(
    cty <25
  ) %>%
  ggplot(
    aes(
      displ,
      cty
    )
  )+
  geom_point(
    aes(
      color = drv,
      size = trans
    ),
    alpha = 0.5
  )+
  geom_smooth(
    method = lm
    )+
  facet_wrap(
    ~year,
    nrow = 1)+
  labs(
    x = "Engine size",
    y = "MPG in the city",
    title = "Fuel efficiency"
  )+
  theme_minimal()
