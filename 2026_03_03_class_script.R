# 2026-03-03
# JEL

# Linear Models

library(tidyverse)
library(palmerpenguins)
library(GGally)

head(penguins)

# Give me columns only that contain numeric data
# ggpairs gives the big fancy plot
ggpairs(data = penguins %>% select(bill_depth_mm, bill_length_mm)) 

# Build a bad model
lm_1 = lm(bill_depth_mm ~ bill_length_mm, data = penguins)
summary(lm_1)
# Intercept = 20.88547
# bill_length_mm = slope = -0.08502 (this doesn't feel good bc states as beak gets longer, it gets skinnier)

str(lm_1)
lm_1$coefficients
class(lm_1)

# putting aes in ggplot call copies it into all proceeding geom calls
ggplot(data = penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm")

# this gives four different plots
# various plots showing stat analysis
plot(lm_1)

# try just modeling one species
gentoo = penguins %>%
  filter(species == "Gentoo",
          !is.na(bill_length_mm))
head(gentoo)
summary(gentoo)

ggpairs(data = gentoo  %>% select(bill_length_mm, bill_depth_mm))

lm_2 = lm(bill_depth_mm ~ bill_length_mm, data = gentoo)
summary(lm_2)

ggplot(data = gentoo, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm")
plot(lm_2)

# quickly slickly plot lm for all three species

ggplot(data = penguins) +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_smooth(aes(x = bill_length_mm, y = bill_depth_mm, color = species), method = lm) +
  geom_smooth(aes(x = bill_length_mm, y = bill_depth_mm), color = "black", method = lm)
# Simpsons Paradox

# Class exercise - 5.1
lm_3 = lm(bill_depth_mm ~ flipper_length_mm, data = gentoo)
summary(lm_3)

plot(lm_3)
ggplot(data = gentoo) +
  geom_point(aes(x = flipper_length_mm, y = bill_depth_mm)) +
  geom_smooth(aes(x = flipper_length_mm, y = bill_depth_mm), method = lm) 
