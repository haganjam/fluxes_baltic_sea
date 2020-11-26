
# Project: Fluxes in the Baltic Sea

# Title: Data preparation: Tidy the data

library(dplyr)
library(readr)
library(tidyr)
library(tibble)
library(here)

# load in the Baltic Sea data
baltic_raw <- read_csv(file = here("data/BalticData_v2.csv"))


# reorganise the data to make it tidy

# replace NAs in unit and deployment with 1s
baltic_ana <- 
  baltic_raw %>%
  mutate(unit = if_else(is.na(unit), "A", unit),
         deployment = if_else(is.na(deployment), 1, deployment))


# make the data tidy
baltic_ana <- 
  baltic_ana %>%
  pivot_wider(id_cols = c("basin", "BT", "code", "year", "unit", "deployment"),
              names_from = c("var"),
              values_from = c("value")) %>% 
  mutate(CN = OC.inv/TN.inv)


# summarise variables by unit because this is a procedural replicate
var_names <- 
  baltic_ana %>%
  select(-basin, -BT, -code, -year, -unit, -deployment) %>%
  names()

var_names

baltic_ana %>%
  group_by(basin, BT, code, year, deployment) %>%
  summarise(units = length(unique(unit)))

baltic_ana <- 
  baltic_ana %>%
  group_by(basin, BT, code, year, deployment) %>%
  summarise(across(.cols = all_of(var_names), ~mean(., na.rm = TRUE)), .groups = "drop")


# summarise variables by deployment because multiple deployments made on same day at same station
baltic_ana <- 
  baltic_ana %>%
  group_by(basin, BT, code, year) %>%
  summarise(across(.cols = all_of(var_names), ~mean(., na.rm = TRUE)), .groups = "drop")

# write this into a .csv for the analysis data
write_csv(x = baltic_ana, file = here("data/Baltic_data_analysis.csv"))


