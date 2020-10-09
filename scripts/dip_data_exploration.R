
# Project: Fluxes in the Baltic Sea

# Title: DIP flux data exploration

library(dplyr)
library(readr)
library(tidyr)
library(tibble)
library(here)
library(mice)
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

# remove rows where all the explanatory have NAs
dip_ana <- 
  dip_ana %>%
  filter_at(vars(S, BW_O2, ChlA.inv, OC.inv, CN), any_vars(!is.na(.)) )

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


# we don't want to lose those nine data points...
# let's see how prevalent they are...

# check the variable distributions
dip_ana %>%
  pivot_longer(cols = c("S", "BW_O2", "ChlA.inv", "OC.inv", "CN"),
               names_to = "var",
               values_to = "val") %>%
  ggplot(data = .,
         mapping = aes(x = val)) +
  geom_histogram() +
  facet_wrap(~var, scales = "free")

# very strange value for ChlA.inv
range(dip_ana$ChlA.inv[!is.na(dip_ana$ChlA.inv) ])

# log-transform bw02 and chla.inv
dip_ana %>%
  mutate(BW_O2 = log10(1 + BW_O2),
         ChlA.inv = log10(1 + ChlA.inv)) %>%
  pivot_longer(cols = c("S", "BW_O2", "ChlA.inv", "OC.inv", "CN"),
               names_to = "var",
               values_to = "val") %>%
  ggplot(data = .,
         mapping = aes(x = val)) +
  geom_histogram() +
  facet_wrap(~var, scales = "free")

# BW_O2 is bimodal (need to be aware of this)
ggplot(data = dip_ana,
         mapping = aes(x = BT, y = BW_O2, colour = basin)) +
  geom_jitter()

# check the correlations among the variables
dip_ana %>%
  select(S, BW_O2, ChlA.inv, OC.inv, CN) %>%
  mutate(BW_O2 = log10(1 + BW_O2),
         ChlA.inv = log10(1 + ChlA.inv)) %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot::corrplot(method = "number")

dip_ana %>%
  select(S, BW_O2, ChlA.inv, OC.inv, CN) %>%
  mutate(BW_O2 = log10(1 + BW_O2),
         ChlA.inv = log10(1 + ChlA.inv)) %>%
  pairs()

lm(log10(1 + ChlA.inv) ~ basin,
   data = dip_ana) %>%
  summary()

ggplot(data = dip_ana,
       mapping = aes(x = basin, y = log(1+ChlA.inv) )) +
  geom_jitter()


# the phosphate flux increases under low oxygen conditions
# but only when marine organic matter dominates and the material is fresh

hist(log(1 + dip_ana$DIP))
hist(dip_ana$DIP)


# simulate what these continuous interaction terms mean
df <- as.data.frame(replicate(n = 3, expr = rnorm(n = 10, mean = 10, sd = 2)))
df



ggplot(data = dip_ana %>%
         mutate(OC.inv = if_else(OC.inv <= mean(OC.inv, na.rm = TRUE), "A", "B")),
       mapping = aes(x = log(1 + BW_O2), y = DIP, colour = OC.inv)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = dip_ana %>%
         mutate(S = if_else(S <= mean(S, na.rm = TRUE), "A", "B")),
       mapping = aes(x = log(1 + BW_O2), y = DIP, colour = S)) +
  geom_point() +
  geom_smooth(method = "lm")


lm1 <- lm(DIP ~ log(1 + BW_O2) + log(1 + BW_O2):OC.inv:S, data = dip_ana)
summary(lm1)

lm2 <- lm(DIP ~ log(1 + BW_O2), data = dip_ana)

lm3 <- lm(DIP ~ 1, data = dip_ana)

AIC(lm1)
AIC(lm2)
AIC(lm3)





