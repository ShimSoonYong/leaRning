#### Load packages ####

library(tidymodels)
tidymodels_prefer()
library(nycflights13)
library(skimr)


### Data preparation ###
set.seed(123)

flights |> skim()
flights |> glimpse()

flight.data <-
  flights |>
  mutate(
    arr.delay = ifelse(arr_delay >= 30,
                       "late",
                       "on.time"),
    arr.delay = factor(arr.delay),
    date = as_date(time_hour)
  ) |>
  inner_join(weather,
             by = c('origin',
                    "time_hour")) |>
  select(dep_time, flight, origin, dest,
         air_time, distance, carrier,
         date, arr.delay, time_hour) |>
  drop_na() |>
  mutate_if(is.character, as.factor)
  
### Descriptive statistic ###

flight.data |> 
  count(arr.delay) |>
  mutate(prop = n/sum(n))

flight.data |>
  skim(dest, carrier)
flight.data |> skim()


### Inital split ###
set.seed(2022)
data.split <- flight.data |>
  initial_split(prop = 3/4)

train.data <- data.split |>
  training()
test.data <- data.split |>
  testing()

train.data |> glimpse()
test.data |> dim()


#### recipe! ####
library(lubridate)
library(timeDate)
library(tidymodels)

flights.recipe <-
  recipe(arr.delay ~ .,
         data = train.data) |>
  update_role(
    flight,
    time_hour,
    new_role = "ID"
    ) |>
  step_date(date,
            features = c("dow", "month")) |>
  step_holiday(date,
               holidays = listHolidays("US"),
               keep_original_cols = FALSE) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors())


#### Modeling ####
linear.model <-
  logistic_reg() |>
  set_engine("glm")

flights.workflow <- 
  workflow() |>
  add_model(linear.model) |>
  add_recipe(flights.recipe) 

# train.data2 <- flights.recipe |>
#   juice()
# test.data2 <- flights.recipe |>
#   bake(new_data = test.data)
# 
# train.data2 |>
#   glimpse()
# test.data2 |>
#   glimpse()
# train.data2 |> dim()

flights.fit <-
  flights.workflow |>
  fit(data = train.data)

flights.fit |>
  extract_fit_parsnip() |>
  tidy()

result <- flights.fit |>
  predict(test.data)
result

flights.aug <-
  flights.fit |>
  augment(test.data)

flights.aug |>
  select(arr.delay,
         time_hour,
         flight,
         .pred_class,
         .pred_on.time)

flights.aug |>
  roc_curve(
    truth = arr.delay,
    .pred_late) |>
  autoplot()
