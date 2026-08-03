# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##               ---- Macrofauna Dutch Coast 2025  ----
#
###             ---- Author: Zita Maria Voskuilen S6043496 ---- 
###                         ---- Start date: 22-7-2026 ---- 
###              ---- Contact: z.m.voskuilen@student.rug.nl---- 
#
#                       ---- SET UP & DATA TIDYING ----
#
###############################################
###############################################
# PART 1: LOAD PACKAGES & R VERSION CONTROL ----

# This code ensures that R is using renv, a version control package manager.   
# Renv ensures that I can use the same package versions continuously. 
# This way, analyses are reproducible in the future regardless the existence 
# of new packages versions. 

# first we set the working directory 
setwd("~/Msc Ecology & Conservation/Master project 2/Master Project 2") 

# Restore packages and versions using renv: 
# - First, check if renv is installed - from the repository (website). 
# - If not, install it
# - Use the function restore from the renv package- 
if("renv" %in% installed.packages()[,"Package"]){
  renv::restore(repos = "https://cloud.r-project.org")
} else{
  install.packages("renv")
  library(renv)
  renv::restore(repos = "https://cloud.r-project.org")
}

# Here, we list and install all needed packages 

## - One of the used packages is "here". "Here" makes sure that all file paths work 
##   regardless of where the project is located

list_of_packages_used <- c("here", "ggplot2", "car", 
                           "tidyverse", "dplyr", "cowplot", 
                           "vegan", "renv", "lme4", 
                            "DHARMa", "emmeans", 
                           "brglm2", "glmmTMB", "MuMIn", 
                           "gridExtra", "multcompView", "survival", 
                           "survminer", "readr", "lme4", "MASS",
                           "minqa", "ggnewscale", "qqplotr", "tidyr", "stringr"
)

# Now we check which packages from the list are not installed yet. 

new_packages <- list_of_packages_used[!(list_of_packages_used 
                                        %in% installed.packages()[,"Package"])]

#If not installed, install the missing ones and also keep them in renv 

if(length(new_packages)) renv::install(new_packages)

# Load packages (library) & update lock file
lapply(list_of_packages_used, library, character.only = TRUE)


renv::snapshot(type = "implicit")  # to save a snapshot of my project package 
# versions into a file called renv.lock


###############################################
# PART 2: LOAD DATA Macrofauna Fact 2026 ----

# load in the data from the google drive 
data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQU4-VqDxuaSTUqruH83CrjeW6sTD95GdQlZnfCFKQLsLkcKOdmxxXD4G7mSRLf2AJeC3agHe8p_cOo/pub?gid=834923619&single=true&output=csv")

str(data)

# only take rows from the Dutch coast, heartbreak and kaap hoorn location 
# these are the pot_ID's tht start with "KH", "KWA", "IJM", "SDL", "HBD", "Heartbreak"

data <- data %>%
  filter(str_detect(pot_ID, "^(KH|KWA|IJM|SDL|HBD|Heartbreak)"))


view(data)

str(data)

# now filter only the rows that have "done" in the first column the other ones can be ignored

data <- data %>%
  filter(Done. == "done")

# view(data)

str(data)

# replace all NA's with zero's

data[is.na(data)] <- 0


