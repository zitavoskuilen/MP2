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
data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQU4-VqDxuaSTUqruH83CrjeW6sTD95GdQlZnfCFKQLsLkcKOdmxxXD4G7mSRLf2AJeC3agHe8p_cOo/pub?gid=1054653854&single=true&output=csv")

str(data)


# now filter only the rows that have "done" in the first column
data <- data %>%
  filter(Done. == "done")


# replace all NA's with zero's

data[is.na(data)] <- 0

# Replace the non numerical values in the data set 
# First find them
letters_per_column <- lapply(data, function(x) {
  x <- trimws(as.character(x))

  unique(x[
    !is.na(x) &
    x != "" &
    is.na(suppressWarnings(as.numeric(x)))
  ])
})

# Alleen kolommen tonen waarin iets gevonden is
letters_per_column <- letters_per_column[
  lengths(letters_per_column) > 0
]

letters_per_column

# replace the h with 10 and the x with 1 in the data set
data[data == "h"] <- 10
data[data == "x"] <- 1


# remove for now the unknown and the last column from the dataset 
data <- data %>%
  dplyr::select(-unknown)


# look how many rows there are of each pot id before i put them together
pot_ID_count <- data %>%
  count(pot_ID)



# FOR NOW TAKE OUT the first two HARVEST OF KAAP HOORN 
# select the rows that have poskey numbers that start with 2026_57 to 2026_71 and remove them from the dataset
# and the poskeys 2026_126 till 2026_140

data <- data %>%
 filter(!Poskey %in% c("2026_57", "2026_58", "2026_59", "2026_60", "2026_61", 
                        "2026_62", "2026_63", "2026_64", "2026_65", "2026_66",
                        "2026_67", "2026_68", "2026_69", "2026_70", "2026_71", 
                       "2026_126", "2026_127", "2026_128", "2026_129", "2026_130",
                       "2026_131", "2026_132", "2026_133", "2026_134", "2026_135", 
                       "2026_136", "2026_137", "2026_138", "2026_139", "2026_140"))

str(data)

