# EPI590R Final Project 
# Daniel Herrera 

# Package loading 
library(pacman)
p_load(ggplot2, gtsummary, here, janitor, medicaldata, naniar, tidyverse)

# Reading-in the dataset. It comes from the package medicaldata
# https://github.com/higgi13425/medicaldata/tree/master/data
fapraw <- medicaldata::polyps

# Saving the data as an original raw file 
write_rds(fapraw, here::here("data", "fapraw.rds"))

# Calling the stored data for analysis 

