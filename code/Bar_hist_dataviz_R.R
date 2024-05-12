library(tidyverse)
data()
?msleep

msleep |> glimpse()

msleep |>
  drop_na(vore) |>
  ggplot(
    aes(
      fct_infreq(vore)
      )
    )+
  geom_bar(
    fill = "#97b3c6"
  )+
  #coord_flip()+
  theme_minimal()+
  labs(
    x = "Who eats what?",
    y = NULL,
    title = "Number of observations per order"
  )


msleep |>
  ggplot(
    aes(
      x = awake
    )
  )+
  geom_histogram(
    binwidth = 2,
    fill = "darkgreen",
    color = "red"
  )+
  theme_classic()+
  labs(
    x = "Total Sleep",
    y = NULL,
    title = "Histogram of total sleep"
  )
  
