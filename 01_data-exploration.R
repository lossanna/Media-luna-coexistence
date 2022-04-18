library(tidyverse)

# Load data ---------------------------------------------------------------

raw <- read.csv("LUNA_COEXIST_ALL_CLEAN_FINAL.csv")


# Data wrangling ----------------------------------------------------------

dat.use <- raw %>% 
  filter(USE == "Y")

unique(dat.use$FOCAL_ID)
