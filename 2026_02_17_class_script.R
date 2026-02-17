# 2026-02-17
# JEL

#ggplot intro

library(tidyverse)
library(palmerpenguins)

head(penguins)

# scatter plot: flipper length v. body mass
ggplot(data = penguins) + 
  geom_point(aes(x = flipper_length_mm, y = body_mass_g), color = "red") +
  xlab("Flipper Length (mm)") +
  ylab("Body mass (g)") +
  ggtitle("penguins are awesome") +
  theme_bw()

# geom_point = scatter plot 
# aes = aesthetic, anything to do with the data itself and how you want the data mapped
# color = "red", does not go in aes() bc has nothing to do with data itself and how it is mapped
# theme_bw, black and white
# theme_classic, no grid black axes
# ggtitle gives plot a main title

# same plot but color points by penguin species
ggplot(data = penguins) + 
  geom_point(aes(x = flipper_length_mm, y = body_mass_g, color = species, shape = sex)) +
  xlab("Flipper Length (mm)") +
  ylab("Body mass (g)") +
  ggtitle("penguins are awesome") +
  theme_bw()
# color is ran in aes bc you are wanting to manipulate how the actual data is mapped
# shape = sex gives each species a different shape, also in aes 

ggplot(data = penguins) + 
  geom_point(aes(x = flipper_length_mm, y = body_mass_g, color = species, shape = sex)) +
  geom_smooth(aes(x = flipper_length_mm, y = body_mass_g), method = "lm") + 
  xlab("Flipper Length (mm)") +
  ylab("Body mass (g)") +
  ggtitle("penguins are awesome") +
  theme_bw()
# geom_smooth introduces line that shows smoothed data on top of scatter plot
# output = `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
# without setting a method it will do what it finds to be best
# method = lm, linear model

penguins_ts = penguins %>%
  group_by(species, year) %>%
  summarize(n_penguins = n())
penguins_ts

ggplot(data = penguins_ts) +
  geom_line(aes(x = year, y = n_penguins, color = species)) + 
  theme_bw()

# histogram - univariable plotting
ggplot(data = penguins) +
  geom_histogram(aes(x = flipper_length_mm, fill = species), 
                    position = "identity",
                    alpha = 0.75,
                    binwidth = 5) +
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4")) +
  theme_bw()
# color = species will outline the bar
# fill = species will fill in the whole bar
# fill will stack bars on top of each other rather than behind each other, WILL NOT SHOW TRUE VALUES
# position = identity, bars will stack behind each other
# alpha = 0.75 will change the transparency of the bars so you can see each color 
# scale_fill_manual = choose the colors manually and its fed as values and a vector of colors
# binwidth = 5, every flipper length in 5 mm range is binned together

# boxplots
ggplot(data = penguins) +
  geom_boxplot(aes(y = flipper_length_mm, x = species)) +
  geom_point(aes(y = flipper_length_mm, x = species)) +
  xlab("Species") +
  ylab("Flipper length (mm)") +
  theme_bw()
# geom_point adds a scatter plot of the data over the box plot to show the true data

# using jitter to move points off of the actual box plot
ggplot(data = penguins) +
  geom_boxplot(aes(y = flipper_length_mm, x = species)) +
  geom_jitter(aes(y = flipper_length_mm, x = species, color = species), width = 0.2) +
  xlab("") +
  ylab("Flipper length (mm)") +
  theme_bw()


# bar plots
my_sex_plot = ggplot(data = penguins %>% filter(!is.na(sex))) +
  geom_bar(aes(x = sex, fill = species)) +
  facet_wrap(~species, nrow = 1) +
  theme_bw()
# facet_wrap, create a different panel for each different quantity in the variable I fed you
# coord_flip will flip the axes

ggsave(filename = "figures/my_sex_plot.png", 
        plot = my_sex_plot,
        width = 5,
        height = 4,
        units = "in",
        dpi = 300)


# Exercise 2.2
ggplot(data = penguins) +
  geom_point(aes(x = , y = ), color = species)
# scales = fixed v. scales = free