# 2026-02-12
# JEL
# Intro to dplyr

# install.packages("tidyverse"), done on 26-02-12 dont have to do again
# install.packages("palmerpenguins"), done on 26-02-12 dont have to do again

# call forward the packages that you have installed, do this every positron session
library(tidyverse)
library(palmerpenguins)

tidyverse_packages() # tells you what packages you can use through tidyverse, typically in console

head(penguins) # palmerpenguins loaded a dataset into R but does not display it
summary(penguins)
glimpse(penguins) # shows all columns and the first few inputs for each column
dim(penguins)


# filter
# filter for Gentoo penguins only
gentoo = filter(penguins, species == "Gentoo") # create new data set that you filtered for only gentoo penguins
summary(gentoo)

gentoo_ladies = filter(penguins, species == "Gentoo", sex == "female")
summary(gentoo_ladies)

# introduce the pipe 
gentoo_ladies = penguins %>%
  filter(species == "Gentoo") %>%
  filter(sex == "female")


# summarize function
gentoo_ladies_body_mass_g = penguins %>%
  filter(species == "Gentoo") %>%
  filter(sex == "female") %>%
  summarize(gentoo_lady_body_mass_g = mean(body_mass_g)) # using subset of data to summarize only the subset you are interested in

gentoo_gents_body_mass_g = penguins %>%
  filter(species == "Gentoo") %>%
  filter(sex == "male") %>%
  summarize(gentoo_gents_body_mass_g = mean(body_mass_g))

# write this code in base R
gentoo_ladies_body_mass_g = mean(penguins$body_mass_g[penguins$sex == "female" & penguins$species == "Gentoo"], na.rm = T)


# Exercise 1.1
chinstrap = penguins %>%
  filter(species == "Chinstrap")
head(chinstrap)
summary(chinstrap)

chinstrap_large = penguins %>%
  filter(species == "Chinstrap") %>%
  filter(flipper_length_mm >= 200)
head(chinstrap_large)
summary(chinstrap_large)
# examine sex ratio differences between these two subsets
# male chinstraps have larger flipper length

# use group_by()
# average mass of each of the 3 species of penguins
# broken down into species and sex
species_mean_mass = penguins %>%
  filter(!is.na(sex)) %>%
  group_by(species, sex) %>%
  summarize(mean_body_mass_g = mean(body_mass_g, na.rm = TRUE),
            sd_body_mass_g = sd(body_mass_g, na.rm = TRUE),
            n_penguins = n())

write_csv(species_mean_mass, file = "data/processed/my_sweet_sweet_body.csv")
temp = read_csv(file = "data/processed/my_sweet_sweet_body.csv")


# mutate
penguins_for_merica = penguins %>%
  mutate(body_mass_lb = body_mass_g * 0.0022) # 0.0022 lb / g
head(penguins_for_merica)

distinct(species)

temp = penguins %>%
  select(species, sex, body_mass_g) # pulls only the columns denoted
head(temp)

temp = penguins %>%
  select(-body_mass_g) # - removes that column
head(temp)

temp = penguins %>%
  arrange(body_mass_g) # arranges from low to high body mass
head(temp)
tail(temp)

temp = penguins %>%
  arrange(desc(body_mass_g)) # arranges from high to low body mass
head(temp)
tail(temp)

# Exercise 1.3

mean_bill_length = penguins %>%
  filter(species == "Adelie") %>%
  filter(island %in% c("Dream", "Biscoe")) %>%
  summarize(mean_bill_length_mm = mean(bill_length_mm, na.rm = TRUE),
            sd_bill_length = sd(bill_length_mm, na.rm = TRUE))

mean_bill_length_torg = penguins %>%
  filter(species == "Adelie") %>%
  filter(island == "Torgersen") %>%
  summarize(mean_bill_length_mm = mean(bill_length_mm, na.rm = TRUE),
            sd_bill_length = sd(bill_length_mm, na.rm = TRUE))
