
# Project: Fluxes in the Baltic Sea

# Title: DIP flux data exploration

library(dplyr)
library(readr)
library(tidyr)
library(tibble)
library(here)
library(ggplot2)

# load in the Baltic Sea data
dip_raw <- read_csv(file = here("data/baltic_flux_analysis_data.csv"))

# view the data
View(dip_raw)
names(dip_raw)

# subset out the relevant columns
dip_ana <- 
  dip_raw %>%
  select(basin, BT, code, year,
         BW_O2, ChlA.inv, CN, DIP)

# subset out the rows where there are no DIP values available
dip_ana <- 
  dip_ana %>%
  filter(!is.na(DIP))

nrow(dip_ana)

# remove rows where all the explanatory have NAs i.e. where there are no values for the explanatory variables
dip_ana <- 
  dip_ana %>%
  filter_at(vars(BW_O2, ChlA.inv, CN), any_vars(!is.na(.)) ) # i.e. are any of these variables not NA?

# check how many data points have complete data
dip_ana %>%
  filter_at(vars(BW_O2, ChlA.inv, CN), all_vars(!is.na(.))) %>%
  View()

dip_ana %>%
  filter_at(vars(BW_O2, ChlA.inv, CN), any_vars(is.na(.))) %>%
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


### investigate marine vs. terrestrial origins using PCA

# does ChlA.inv add extra information? 
plot(log10(dip_ana$ChlA.inv), log10(dip_ana$CN) )
cor(log10(dip_ana$ChlA.inv), log10(dip_ana$CN), use = "complete.obs")

# subset complete observations (i.e. ChlA.inv and CN)
d.pca <- 
  dip_ana %>%
  filter_at(vars(ChlA.inv, CN), all_vars(!is.na(.)))

# run a PCA with these two variables
pca_1 <- prcomp(~ log10(ChlA.inv) + log10(CN), d.pca, 
                center = TRUE, scale = TRUE)
summary(pca_1)

# try predict PC1 axis with just CN
summary(lm(pca_1$x[,1] ~ log10(d.pca$CN)))

# therefore, we probably wouldn't gain much by using ChlA.inv anyway


### create a data subset for the analysis

# subset out the rows where there is complete data
dip_comp <- 
  dip_ana %>%
  filter_at(vars(BW_O2, CN), all_vars(!is.na(.)))

# check the variable distributions
dip_comp %>%
  pivot_longer(cols = c("BW_O2", "CN"),
               names_to = "var",
               values_to = "val") %>%
  ggplot(data = .,
         mapping = aes(x = val)) +
  geom_histogram() +
  facet_wrap(~var, scales = "free")


# create a log10-transformed version of BW_O2
dip_comp <- 
  dip_comp %>%
  mutate(log_BW_O2 = log10(1 + BW_O2))

# create a hypoxic vs. non-hypoxic variable
dip_comp <- 
  dip_comp %>%
  mutate(hypoxic = if_else(BW_O2 < 63, 1, 0))

View(dip_comp)

# examine the distribution of DIP
hist(dip_comp$DIP) # decent distribution


# check variable distributions one more time
dip_comp %>%
  pivot_longer(cols = c("DIP", "CN"),
               names_to = "var",
               values_to = "val") %>%
  ggplot(data = .,
         mapping = aes(x = val, fill = as.character(BT) )) +
  geom_histogram() +
  facet_wrap(~var, scales = "free")

ggplot(data = dip_comp,
       mapping = aes(x = basin, y = DIP, colour = BT)) +
  geom_jitter()

# check BT distribution in the different basins
dip_comp %>%
  group_by(basin, BT) %>%
  summarise(n = n())

# write these data into a file to run the models on
write_csv(x = dip_comp, file = here("data/baltic_flux_model_data.csv"))



