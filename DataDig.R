
library(tidyverse)
library(janitor)
library(tidymodels)
library(GGally)
bike_data <- read_csv("https://www4.stat.ncsu.edu/online/datasets/SeoulBikeData.csv",locale = locale(encoding = "ISO-8859-1"))
bike_data <- bike_data %>% janitor::clean_names()
#format the date variable to be an actual date variable
bike_data$date_new<-as.Date(bike_data$date,format="%m/%d/%y")
#convert character variables to factor
bike_data <- bike_data %>% mutate(across(where(is.character), as.factor))
bike_data_numVars <- 
  bike_data %>%
  filter(functioning_day=="Yes") %>%
  select(-date,-seasons,-holiday,-functioning_day,-date_new) 

class(bike_data)
typeof(bike_data)
attributes(bike_data)
names(bike_data)

sum_na <- function(column){
  sum(is.na(column))
}

na_counts <- bike_data |>
  summarize(across(everything(), sum_na))
summary(na_counts)

summary(bike_data)
summary(bike_data$rented_bike_count)
table(bike_data$seasons)
table(bike_data$holiday)
table(bike_data$functioning_day)
cor(bike_data_numVars)

bike_data_for_model <- bike_data %>%
  group_by(date_new,seasons,holiday) %>%
  filter(functioning_day=="Yes") %>%
  summarize(bike_count = sum(rented_bike_count),
            temp = mean(temperature_c),
            humidity = mean(humidity_percent),
            wind_speed = mean(wind_speed_m_s),
            vis = mean(visibility_10m),
            dew_point_temp = mean(dew_point_temperature_c),
            solar_radiation = mean(solar_radiation_mj_m2),
            rainfall = sum(rainfall_mm),
            snowfall = sum(snowfall_cm)) %>%
  ungroup()

bike_data_for_model_numVars <- bike_data_for_model %>%
  select(-seasons,-holiday,-date_new) 

set.seed(123)
bike_split <- initial_split(bike_data, prop = 0.75, strata = seasons)
bike_train <- training(bike_split)
bike_test <- testing(bike_split)
bike_10_fold <- vfold_cv(bike_train, 10)

MLR_rec1 <- recipe(bike_count ~ ., data = bike_train) |>
  step_date(date, features = "dow") |>
  step_mutate(day_type = factor(if_else(date_dow %in% c("Sat", "Sun"), "Weekend", "Weekday"))) |>
  step_rm(date, date_dow) |>
  step_dummy(seasons, holiday, day_type) |>
  step_normalize(all_numeric(), -bike_count)


