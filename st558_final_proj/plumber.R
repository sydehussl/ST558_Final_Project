library(plumber)
library(tidyverse)
library(tidymodels)

#* @apiTitle ST 558 Final Project API
#* @apiDescription API for predicting water potability using a tuned random forest model. Written by Chris Grace for the ST 558 final project.

# read in water data
water_raw <- read.csv("water_potability.csv")

names(water_raw) <- tolower(names(water_raw))

# preds we identified in EDA step
useful_preds <- c("solids",
                  "chloramines",
                  "conductivity",
                  "hardness",
                  "ph",
                  "turbidity")

# prep data set
water <-
  water_raw |>
  rename("potable" = "potability",
         "trihalo" = "trihalomethanes") |>
  mutate(potable = factor(potable,
                          labels = c("non_potable", "potable"))) |>
  select(potable, all_of(useful_preds)) |>
  drop_na()

# create recipe
tree_rec <-
  recipe(potable ~ ., data = water)

# specify model w/ tuned mtry (2)
rf_model <-
  rand_forest(mtry = 2) |>
  set_engine("ranger") |>
  set_mode("classification")

# define workflow
rf_wkf <-
  workflow() |>
  add_recipe(tree_rec) |>
  add_model(rf_model)

# fit model to data
rf_mod <-
  rf_wkf |>
  fit(water)

mean_parms <-
  water |>
  select(!potable) |>
  summarize(across(everything(), mean))



#* Predict potability given a set of predictor values
#* @param water_parms The 
#* @get /echo
function(msg = "") {
    list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
    rand <- rnorm(100)
    hist(rand)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
    as.numeric(a) + as.numeric(b)
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
