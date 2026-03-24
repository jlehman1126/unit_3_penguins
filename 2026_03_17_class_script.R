# 2026-03-17
# JEL

library(tidyverse)
library(palmerpenguins)

###### pick up from 2026-03-05 script ########
summary(lm_3)

# (+ species) adds it as an x consideration
# (* species) adds it as an interaction
lm_4 = lm(bill_depth_mm ~ bill_length_mm + species + bill_length_mm * species, data = penguins_lm_3)
lm_4 = lm(bill_depth_mm ~ bill_length_mm * species, data = penguins_lm3)
summary(lm_4)

AIC(lm_3, lm_4)
# if the difference between 2 models is greater than a magnitude of 2, then the smaller AIC is the significantly better fit
# penalized for model complexities

# step function, use for hw 5 !!!!!
# use step function on most complex lm
# takes complex model and removes terms one at a time until it finds the most parsimonious model
# the model that has the best fit while being the most simple it can be
best_model = step(lm_4)
best_model

lm_4_predict = lm_4 %>%
  broom::augment(se_fit = T, interval = "confidence", conf.level = 0.95)
head(lm_4_predict)

ggplot(data = lm_4_predict) +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) + 
  geom_line(aes(x = bill_length_mm, y = .fitted, color = species)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, x = bill_length_mm, fill = species), alpha = 0.5) +
  theme_classic()

# 2 continuous variable predictors 
library(car) # vif()

gentoo = penguins %>%
  filter(species == "Gentoo")
summary(gentoo)

lm_gentoo_1 = lm(bill_depth_mm ~ bill_length_mm, data = gentoo)
lm_gentoo_2 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm, data = gentoo)
lm_gentoo_3 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm + body_mass_g, data = gentoo)
lm_gentoo_4 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm + body_mass_g - 1, data = gentoo) # removes intercept since intercept was insignificant
# This is not typically done unless a claim can be made that 0 is a realistic value
# lm_4 is technically more simple but we cannot make such a claim so proceed with lm_gentoo_3

AIC(lm_gentoo_1, lm_gentoo_2, lm_gentoo_3)
step(lm_gentoo_3)
# lm_gentoo_3 is the best model despite being the most complex

summary(lm_gentoo_3)
summary(lm_gentoo_4)

# variance inflaction factor
vif(lm_gentoo_3)
# spits out a value for all 3 variables we are using to predict bill_depth_mm
# vif > 10, those things are colinear
# vif 3-5 squishy
# vif < 3, you are good to go with multiple regression with no colinearity

# create new data with variable body mass but constant flipper and bill length
newdata = gentoo %>%
  select(body_mass_g) %>%
  mutate(flipper_length_mm = median(gentoo$flipper_length_mm, na.rm = T)) %>%
  mutate(bill_length_mm = median(gentoo$bill_length_mm, na.rm = T))
head(newdata)
head(gentoo)

lm_gentoo_3_predict = lm_gentoo_3 %>%
  broom::augment(newdata = newdata, se_fit = T, interval = "confidence")
head(lm_gentoo_3_predict)

ggplot(data = lm_gentoo_3_predict) +
  geom_point(aes(x = body_mass_g, y = bill_depth_mm), data = gentoo) +
  geom_line(aes(x = body_mass_g, y = .fitted)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, x = body_mass_g), alpha = 0.3) +
  annotate("text", x = 4250, y = 17, label = paste0("flipper length = ", median(gentoo$flipper_length_mm, na.rm = T))) +
  annotate("text", x = 4250, y = 16.8, label = paste0("bill length =", median(gentoo$bill_length_mm, na.rm = T))) +
  theme_bw()
# in figure or figure caption make sure to clarify that flipper and bill length are set at median
# this model is not fully capturing the full story that flipper and bill length may be creating

# ANOVA
# mathematically equivalent to linear models
# x's are usually categorical variables
penguin_lm = lm(body_mass_g ~ species + sex, data = penguins)
summary(penguin_lm)
anova(penguin_lm)

penguins %>%
  group_by(sex) %>%
  summarize(mean_body_mass_g = mean(body_mass_g, na.rm = T))

penguins %>%
  group_by(species) %>%
  summarize(mean_body_mass_g = mean(body_mass_g, na.rm = T))
penguin_anova = aov(body_mass_g ~ sex + species, data = penguins)
summary(penguin_anova)
TukeyHSD(penguin_anova) # breaks down species interaction into each set of two species comparisons to tell you which ones are significant
