# 2026-03-05
# JEL

# Multiple Linear Regression
# x's are not colinear
# how does each x contribute to the y
# when using linear for predicting, typically do not care about how correlated or colinear x values are

# Kinds of X's (independent variables)
# 1 - continuous variable, number (body mass)
# 2 - categorical variable or distinct variable (factor), fixed number of distinct groups (species)

library(tidyverse)
library(palmerpenguins)
library(ggiraph)
library(ggiraphExtra)
head(penguins)

# see ALL the different groups, base R
unique(penguins$year)

# tidyverse
penguins %>%
  distinct(year)

# year is technically a numerical value but is a categorical variable

# create data set that drops NA's
penguins_lm3 = penguins %>%
  filter(!is.na(bill_depth_mm))

# build my first
lm_3 = lm(bill_depth_mm ~ bill_length_mm + species, data = penguins_lm3)
summary(lm_3)
# multiple r-squared = 0.769, 77% of variation in bill depth is explained by this model

# ANOVA = analysis of variation
# ANOVA's most frequently used when all x's are categorical
anova(lm_3)

# broom package is a part of tidy that is not typically called upon loading
# tidy makes a tibble output of p-values
# conf.int = T, adds confidence intervals to tibble, gives high and low estimates within your specified confidence level
lm_3_table = broom::tidy(lm_3, conf.int = T, conf.level = 0.95)
write_csv(lm_3_table, file = "figures/lm_3_table.csv")

# using ggiraph and ggiraphExtra
# makes plot output
# se = T, adds standard error ribbon to the linear models
# interactive = T, allows you to hover over a point and will give the specifics for that point
# interactive = T also gives equation of the lines
ggPredict(lm_3, se = T, interactive = T)


###############################################################
# 3 different methods for how to generate your own predictions
###############################################################

# 1 - predict function in base R
lm_3_predictions = predict(lm_3, interval = "confidence", level = 0.95)
head(lm_3_predictions)
dim(lm_3_predictions)
dim(penguins_lm3)
# spits out vector of numbers
# took all the underlying bill_length and species data in lm_3 and is generating y predictions for each row

# add columns together from predictions and original data
penguins_lm3_predict = cbind(penguins_lm3, lm_3_predictions)
head(penguins_lm3_predict)
dim(penguins_lm3_predict)

# build my own figure with my generated predictions
# color = NULL gets rid of outlines on the confidence intervals
ggplot(data = penguins_lm3_predict, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() + 
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = species, color = NULL), alpha = 0.5) +
  theme_bw()

# generate new data so we can extrapolate beyond the data feeding the model
# use data with NA removed or it will not like it
newdata_bill_length_mm = seq(from = min(penguins_lm3$bill_length_mm), to = max(penguins_lm3$bill_length_mm), by = 0.1)
newdata = expand.grid(bill_length_mm = newdata_bill_length_mm, species = unique(penguins_lm3$species))
head(newdata)
tail(newdata)
# expand.grid makes a table containing every possible combination of bill depth and species

newdata_predict_lm3 = cbind(newdata, predict(lm_3, newdata = newdata, interval = "confidence"))
head(newdata_predict_lm3)

# plot with linear models plotted across whole data range for all species
ggplot() +
  geom_point(data = penguins_lm3, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_line(data = newdata_predict_lm3, aes(x = bill_length_mm, y = fit, color = species)) +
  geom_ribbon(data = newdata_predict_lm3, aes(x = bill_length_mm, ymin = lwr, ymax = upr, fill = species), alpha = 0.5)

# 2 - augment function with tidyverse
lm_3_predict = lm_3 %>%
  broom::augment(data = penguins_lm3, se_fit = T, interval = "confidence", conf.level = 0.95)
head(lm_3_predict)
glimpse(lm_3_predict)

# 3 - tidyR function has expand function
newdata = penguins_lm3 %>%
  tidyr::expand(bill_length_mm, species)
head(newdata)

lm_3_predict = lm_3 %>%
  broom::augment(newdata = newdata, se_fit = T, interval = "confidence", conf.level = 0.95)
head(lm_3_predict)
