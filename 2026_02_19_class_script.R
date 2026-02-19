# 2026-02-19
# JEL

library(tidyverse)
library(palmerpenguins)
# install.packages("rstatix")
library(rstatix)

head(penguins)

# t test on gentoo body mass
# are the body masses derived from this data set similar to what is found in the literature

gentoo = penguins %>%
  filter(species == "Gentoo") %>%
  droplevels() # drops Adelie and Chinstrap and only keeps the identified variable
gentoo
dim(gentoo)
summary(gentoo)
mean(gentoo$body_mass_g, na.rm = T)
sd(gentoo$body_mass_g, na.rm = T)

ggplot() +
  geom_histogram(data = gentoo, aes(x = body_mass_g)) +
  theme_bw()

# QQ plot, quantile quantile plot, if the data is normally distributed this should look linear
ggplot() + 
  stat_qq(aes(sample = body_mass_g), data = gentoo)

gentoo_body_mass_g_symonds = 5500 # from Symonds and Tattersal 2010, accessed via EOL

# one sample t test
my_t_test = t.test(gentoo$body_mass_g, mu = gentoo_body_mass_g_symonds)
my_t_test
class(my_t_test) # output 'htest'
str(my_t_test) # structure
my_t_test$p.value # this is how you programmatically access numbers from your t test ($)

# try a pipe friendly t test from rstatix package
my_t_test_2 = gentoo %>%
  t_test(body_mass_g ~ 1, mu = gentoo_body_mass_g_symonds)
# output is a tibble so easy to access in a similar fashion

my_t_test_2$p


# independent sample t test (2 samples boys heights in 2 different classrooms 5th grade v 6th grade)
# observations need to be INDEPENDENT

my_data_for_t_test = penguins %>%
  filter(species %in% c("Gentoo", "Adelie"), 
          !is.na(body_mass_g)) %>%
  select(species, body_mass_g) %>%
  droplevels()

ggplot(my_data_for_t_test) +
  geom_histogram(aes(x = body_mass_g)) +
  facet_wrap(~species, scales = "free")

ggplot(data = my_data_for_t_test) +
  stat_qq(aes(sample = body_mass_g)) +
  facet_wrap(~species, scales = "free")

# check the quality of variance, if true can use student's t test, if false use welch's t test
my_data_for_t_test %>%
  levene_test(body_mass_g ~ species)
# p value output = 0.159 
# null is that variance is equal, greater than 0.05 (Student's)
# alternative is that they are not equal, less than 0.05 (Welch's)


# this automatically runs the Welch's t test bc var.equal = FAULT is default
t.test(my_data_for_t_test$body_mass_g ~ my_data_for_t_test$species) # significantly different from each other
# t = -23.386
# p = < 2.2e-16

# run Student's t test
t.test(my_data_for_t_test$body_mass_g ~ my_data_for_t_test$species, var.equal = TRUE)
# t = -23.614, further from 0
# p = < 2.2e-16

# pipe friendly version (rstatix)
my_data_for_t_test %>%
  t_test(body_mass_g ~ species, var.equal = TRUE)

# paired sample t test, replicates with the exact same individuals
# add paired = TRUE to t_test parameters



######################################################################
#                Correlations babyyyyyy
######################################################################

head(gentoo)
ggplot(data = gentoo) +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm))

ggplot(data = gentoo) +
  stat_qq(aes(sample = bill_length_mm))

cor(x = gentoo$bill_depth_mm, y = gentoo$bill_length_mm, use = "complete.obs") # gives pearson method (parametric test)
# other methods are kendall or spearman
# pearson r = 0.6433839 

cor.test(x = gentoo$bill_length_mm, y = gentoo$bill_depth_mm, use = "complete.obs") # significantly correlated

gentoo %>%
  cor_test(bill_length_mm, bill_depth_mm)
# pearson r, 0 = not at all correlated, most correlated = -/+ 1


# use GGally to get flahsy graph things
library(GGally)

# correlation matrices
gentoo %>%
  select(body_mass_g, bill_length_mm, bill_depth_mm, flipper_length_mm) %>%
  GGally::ggpairs()

cor(gentoo[, seq(3,6)] )

penguins %>%
  select(species, where(is.numeric)) %>%
  ggpairs(aes(color = species))
