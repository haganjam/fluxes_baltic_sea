
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

# define models to test (explanations on the side)
exp.vars <- list(c("1"), # null model with no predictor variables
                 c("1", "BT"), # null model: DIP is only affected by the type of basin
                 c("1", "BT", "log_BW_O2"), # H: DIP flux is only driven by BW02 irrespective of the origin or organic matter
                 c("1", "BT", "log_BW_O2", "CN"), # DIP flux is driven independently by BW02 and origin of organic matter
                 c("1", "BT", "log_BW_O2*CN")) # H: DIP flux decreases with BW02 when marine organic matter dominates

# run these five different models
lm.out <- lm.comp(data = dip.mod, resp = "DIP", e.vars = exp.vars)

# arrange models by AIC and output into a viewing pane
lm.out %>%
  arrange(AIC) %>%
  View()

# low AIC is the best
# as you can see, the fifth model above is the best model according to AIC
# it also has quite a high R2

# but, just because it fits the data best, does not mean that it matches your predictions

# for this, we have to dig deeper into the model


# examine the best model to see if the predictions match
lm.x <- lm(DIP ~ BT + log_BW_O2*CN, data = dip.mod)
lapply(c(1:4), function(x) {plot(lm.x, x)} )
hist(residuals(lm.x))

# get coefficients from this model
lm.x.c <- lm.x$coefficients

# this is the equation of the linear model so you can see how it works

# we predict DIP based on the covariates for the first two rows of data (mod.test[1:2, ])
# input this where the data should go into the equation

# first, we need to convert basin type into 0 (A) and 1 (ET) because that's what lm() does automatically
mod.test <- 
  dip.mod %>%
  mutate(BT = if_else(BT == "A", 0, 1))

# main effects (m)
m <- lm.x.c[1] + lm.x.c[2]*mod.test[1:2,]$BT + lm.x.c[3]*mod.test[1:2,]$log_BW_O2 + lm.x.c[4]*mod.test[1:2,]$CN 

# interaction (i)
i <- lm.x.c[5]*mod.test[1:2,]$log_BW_O2*mod.test[1:2,]$CN 

# add the main effects (m) and the interaction term (i)
# and we get the predicted value for the first two rows of data
m + i

# we get the same result if we use predict on the lm() object
predict(lm.x)[1:2]


# we can check if our model predicts DIP in line with your predictions
# H1: DIP should decrease with BWO2 where the material is of marine origin (i.e. low CN)

# check the range of these predictor variables
range(dip.mod$CN)
range(dip.mod$log_BW_O2)

# use the model to predict how log_BW_O2 affects DIP when we 
# hold CN at low values i.e. marine origin in accumulation basins
low.cn <- expand.grid(log_BW_O2 = seq(0, 2.5, 0.25),
                      CN = c(7.5),
                      BT = c("A") ) # can also change to ET and see what happens

# examine relationship between log_BW_O2 and the model predictoin when holding CN at low values and in accumulation basins
plot(low.cn$log_BW_O2, predict(lm.x, newdata = low.cn))

# this model shows:
# dip decreases with log_BW_O2 when CN is low, in accumulation basins


# we can test your other prediction
# H1: DIP should not change with BWO2 where the material is of terrestrial origin (i.e. high CN)

# use the model to predict how log_BW_O2 affects DIP when we 
# hold CN at high values i.e. terrestrial origin in accumulation basins
high.cn <- expand.grid(log_BW_O2 = seq(0, 2.5, 0.25),
                      CN = c(12.5),
                      BT = c("A") ) # can also change to ET and see what happens

# examine relationship between log_BW_O2 and the model predictoin when holding CN at low values and in accumulation basins
plot(high.cn$log_BW_O2, predict(lm.x, newdata = high.cn))

# this model shows:
# dip increases with log_BW_O2 when CN is low, in accumulation basins
# if you look at the y-axis scale, the effect is very weak







# the model predicts that DIP decreases with bottom water oxygen when the material is of marine origin (i.e. low CN)


# H1a: DIP should not change with bottom water oxygen when the material is of terrestrial origin (i.e. high CN)

# hold cn at high values i.e. non-marine
range(dip.mod$CN)
eg <- expand.grid(log_BW_O2 = seq(0, 2.5, 0.5),
                  CN = c(12) )

m <- lm.x.c[1] + lm.x.c[2]*(0) + lm.x.c[3]*eg$log_BW_O2 + lm.x.c[4]*(eg$CN) 
i <- lm.x.c[5]*eg$log_BW_O2*(eg$CN) 

# dip increases with log_BW_O2 when CN is high (i.e. non-marine), in accumulation basins
m+i

# the model predicts that DIP increases slightly with bottom water oxygen when the material is of terrestrial origin (i.e. high CN)

