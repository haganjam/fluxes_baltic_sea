
# Project: Fluxes in the Baltic Sea

# Title: DIP flux models

library(dplyr)
library(readr)
library(here)
library(ggplot2)

# install libraries where functions are called from
if( (("broom" %in% installed.packages()[,1]) & ("viridis" %in% installed.packages()[,1]))  == FALSE) {
  print("WARNING! this script requires broom and viridis to be installed")
}

# load in the Baltic Sea data
dip.mod <- read_csv(file = here("data/baltic_flux_model_data.csv"))


# set up a function to run different models that can then be compared
lm.comp <- function(data, resp, e.vars) {
  
  # set an output list for the model coefficients
  est.lm <- vector("list", length(e.vars))
  names(est.lm) <- seq(1:length(e.vars))
  
  # set an output list for the model fit statistics
  fit.lm <- vector("list", length(e.vars))
  names(fit.lm) <- seq_along(1:length(e.vars))
  
  for (i in 1:length(e.vars) ) {
    
    df <- data
    
    # fit model using chosen predictors
    lm.e.vars <- lm(reformulate(e.vars[[i]], resp), data = df)
    
    # write coefficients to the est.lm list
    est.lm[[i]] <- broom::tidy(lm.e.vars)
    
    # write fit statistics to the fit.lm list
    fit.lm[[i]] <- broom::glance(lm.e.vars)
  }
  
  # convert lists to data.frames and join
  full_join(bind_rows(est.lm, .id = "model"), 
            bind_rows(fit.lm, .id = "model"),
            by = "model")
}

# define models to test
exp.vars <- list(c("1"),
                 c("1", "BT"),
                 c("1", "BT", "log_BW_O2"),
                 c("1", "BT", "log_BW_O2", "CN"),
                 c("1", "BT", "log_BW_O2*CN"))

# run these five different models
lm.out <- lm.comp(data = dip.mod, resp = "DIP", e.vars = exp.vars)

# arrange models by AIC and output into a viewing pane
lm.out %>%
  arrange(AIC) %>%
  View()




