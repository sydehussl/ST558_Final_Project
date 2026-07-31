library(plumber)
library(tidyverse)
library(tidymodels)

#* @apiTitle ST 558 Final Project API - Chris Grace
#* @apiDescription API for predicting water potability using a tuned random forest model. Written by Chris Grace for the ST 558 final project.

# read in water data
water_raw <- read.csv("../water_potability.csv")

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

# find mean predictor values
mean_parms <-
  water |>
  select(!potable) |>
  summarize(across(everything(), mean))

#* Predict potability given a set of predictor values 
#* @param solids Total dissolved solids in PPM
#* @param chloramines Amount of chloramines in PPM
#* @param conductivity Electrical conductivity in microsiemens/cm
#* @param hardness Hardness of the water in mg/L
#* @param ph pH of the water
#* @param turbidity Haziness in NTU
#* @get /pred
function(solids = mean_parms$solids,
         chloramines = mean_parms$chloramines,
         conductivity = mean_parms$conductivity,
         hardness = mean_parms$hardness,
         ph = mean_parms$ph,
         turbidity = mean_parms$turbidity) {
    
    parm_df <-
      data.frame(solids = solids,
                 chloramines = chloramines,
                 conductivity = conductivity,
                 hardness = hardness,
                 ph = ph,
                 turbidity = turbidity)
  
    predict(rf_mod, new_data = mean_parms) |>
    pull() |> 
    as.character()
}

#* Plot a confusion matrix
#* @serializer png
#* @get /confusion
function() {
  conf_matrix <-  
    water |>
    cbind(predict(rf_mod, water)) |>
    rename("pred" = ".pred_class") |>
    conf_mat(potable, pred) |>
    autoplot(type = "heatmap")
  
  print(conf_matrix)
}

#* Return my name and github link
#* @get /info
function() {
    c("Chris Grace! EDA/Modeling is at https://sydehussl.github.io/ST558_Final_Project/eda.html")
}