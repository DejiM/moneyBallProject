# preamble

# 16-07-2016

# Contain essentail operations needed for most scripts


setwd("~/Documents/PhD/NLMEM/Courses/Udemy/data_science_ml/")

package_vec <- c("readxl", "dplyr", "ggplot2")

lapply(package_vec, require, character.only = TRUE)
