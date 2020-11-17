
# Project: Fluxes in the Baltic Sea

# Title: DIP flux data exploration

library(dplyr)
library(readr)
library(tidyr)
library(tibble)
library(here)
library(ggplot2)

# load in the Baltic Sea data
dip_raw <- read_csv(file = here("data/Baltic_data_analysis.csv"))

# view the data
View(dip_raw)
names(dip_raw)

# subset out the relevant columns
dip_ana <- 
  dip_raw %>%
  select(basin, BT, code, year, deployment,
         S, BW_O2, ChlA.inv, OC.inv, CN, DIP)

# subset out the rows where there are no DIP values available
dip_ana <- 
  dip_ana %>%
  filter(!is.na(DIP))

nrow(dip_ana)

# remove rows where all the explanatory have NAs i.e. where there are no values for the explanatory variables
dip_ana <- 
  dip_ana %>%
  filter_at(vars(S, BW_O2, ChlA.inv, OC.inv, CN), any_vars(!is.na(.)) ) # i.e. are any of these variables not NA?

# check how many data points have complete data
dip_ana %>%
  filter_at(vars(S, BW_O2, ChlA.inv, OC.inv, CN), all_vars(!is.na(.))) %>%
  View()

dip_ana %>%
  filter_at(vars(S, BW_O2, ChlA.inv, OC.inv, CN), any_vars(is.na(.))) %>%
  View()

# are there many sites with multiple deployments? no
dip_ana %>%
  group_by(basin, BT, code, year) %>%
  summarise(n = n()) %>%
  filter(n > 1)

# how many data points are there in the different basins?
dip_ana %>%
  group_by(basin) %>%
  summarise(n = n())

dip_ana %>%
  filter(basin == "GOB") %>%
  View()

# what is the spread of accumulation and other basin types
dip_ana %>%
  group_by(basin, BT) %>%
  summarise(n = n())

# subset out the rows where there is complete data
dip_comp <- 
  dip_ana %>%
  filter_at(vars(S, BW_O2, ChlA.inv, OC.inv, CN), all_vars(!is.na(.)))


# check the variable distributions
dip_comp %>%
  pivot_longer(cols = c("S", "BW_O2", "ChlA.inv", "OC.inv", "CN"),
               names_to = "var",
               values_to = "val") %>%
  ggplot(data = .,
         mapping = aes(x = val)) +
  geom_histogram() +
  facet_wrap(~var, scales = "free")

# very strange value for ChlA.inv
range(dip_comp$ChlA.inv)

# log-transform bw02 and chla.inv
dip_comp %>%
  mutate(BW_O2 = log10(1 + BW_O2),
         ChlA.inv = log10(1 + ChlA.inv)) %>%
  pivot_longer(cols = c("S", "BW_O2", "ChlA.inv", "OC.inv", "CN"),
               names_to = "var",
               values_to = "val") %>%
  ggplot(data = .,
         mapping = aes(x = val)) +
  geom_histogram() +
  facet_wrap(~var, scales = "free")

dip_comp <- 
  dip_comp %>%
  mutate(log_BW_O2 = log10(1 + BW_O2),
         log_ChlA.inv = log10(1 + ChlA.inv))


# BW_O2 is bimodal (need to be aware of this)
ggplot(data = dip_ana,
         mapping = aes(x = BT, y = BW_O2, colour = basin)) +
  geom_jitter()


# create a hypoxic vs. non-hypoxic variable
dip_comp <- 
  dip_comp %>%
  mutate(hypoxic = if_else(BW_O2 < 63, 1, 0))


# investigate marine vs. terrestrial origins using PCA
names(dip_comp)

pca_1 <- prcomp(~ CN + log_ChlA.inv, data = dip_comp, center = TRUE, scale = TRUE)
summary(pca_1)

plot(dip_comp$CN, dip_comp$log_ChlA.inv)

eigs <- (pca_1$sdev)^2
plot(eigs/sum(eigs))

ggplot(data = data.frame(pca_1$x),
       mapping = aes(x = PC1, y = PC2)) +
  geom_point()

cbind(pca_1$x, select(dip_comp, CN, log_ChlA.inv)) %>%
  cor() %>%
  corrplot::corrplot(method = "number")


# fit a preliminary model
ggplot(data = dip_comp,
       mapping = aes(x = log10(1+DIP), colour = as.character(hypoxic) )) +
  geom_histogram()





