# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##               ---- Macrofauna Dutch Coast 2025  ----
#
###             ---- Author: Zita Maria Voskuilen S6043496 ---- 
###                         ---- Start date: 22-7-2026 ---- 
###              ---- Contact: z.m.voskuilen@student.rug.nl---- 
#
#                       ---- SET UP & DATA TIDYING ----
#
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ----::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::----
# ---- LOAD PACKAGES & R VERSION CONTROL ----

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
                           "minqa", "ggnewscale", "qqplotr", "tidyr"
)

# Now we check which packages from the list are not installed yet. 

new_packages <- list_of_packages_used[!(list_of_packages_used 
                                        %in% installed.packages()[,"Package"])]

#If not installed, install the missing ones and also keep them in renv 

if(length(new_packages)) renv::install(new_packages)

# Load packages (library) & update lock file
lapply(list_of_packages_used, library, character.only = TRUE)


theme_set(theme_cowplot()) # to change the default ggplot2 theme to cowplot


renv::snapshot(type = "implicit")  # to save a snapshot of my project package 
# versions into a file called renv.lock

# Keeps R environment clean by removing temporary variables from there. 
rm(list_of_packages_used, new_packages)

# ---- CREATE THEME ----
# Here I create a standard plotting theme:
st_theme <- theme_light(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

??theme_light

theme_set(st_theme)

my_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "grey50"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 14)
  )
#
# ----::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::----
## ----  Zostera noltii sqaures Fact 2025 ----
# ----::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::----
# ---- LOAD RAW DATASET ----

# First, load raw data from Google Drive
# In google drive: file -> share with others -> choose sheet -> publish to web
# FactMacrofauna link:
Macrofauna <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQU4-VqDxuaSTUqruH83CrjeW6sTD95GdQlZnfCFKQLsLkcKOdmxxXD4G7mSRLf2AJeC3agHe8p_cOo/pub?gid=834923619&single=true&output=csv")

Macrofauna
view(Macrofauna)
data <- Macrofauna

str(data)


### script van Janne 


###############################################
# PART 3: PREPARE SPECIES MATRIX
###############################################
# Your dataset structure:
# columns 1–5 = metadata (Done?, Harvest, pot_ID, poll_no_poll, Poskey)
# columns 6+ = species abundances

species_data <- data[,6:ncol(data)]

# Replace NAs by 0 (important for community data)
species_data <- species_data %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

# Remove columns with all zeros (rare in sparse data)
species_data <- species_data[, colSums(species_data) > 0]

# Optional: Hellinger transform (recommended for PCA)
species_hel <- decostand(species_data, method = "hellinger")

###############################################
# PART 4: DETERMINE BEST ORDINATION METHOD
###############################################
# Key idea from Lefcheck:
# Use DCA gradient length (axis 1) to decide:
# < 3  -> linear methods (PCA, RDA)
# 3–4  -> intermediate
# > 4  -> unimodal methods (CA, CCA, NMDS)


species0 <- species_hel %>%
  select(where(~ sum(.x, na.rm = TRUE) > 0)) %>%   # remove empty species
  filter(rowSums(.) > 0)                          # ✅ remove empty sites

dca_res <- decorana(species0)
print(dca_res) 

# Extract gradient length (first axis)
gradient_length <- dca_res$evals[1]
gradient_length

cat("Gradient length (DCA axis 1):", gradient_length, "\n") # = 0.5 dus PCA RDA

###############################################
# INTERPRETATION RULE
###############################################
if (gradient_length < 3) {
  cat("→ Linear methods recommended: PCA or RDA\n")
} else if (gradient_length >= 3 & gradient_length <= 4) {
  cat("→ Mixed methods: PCA or NMDS (compare both)\n")
} else {
  cat("→ Unimodal methods recommended: CA, CCA or NMDS\n")
}

###############################################
# PART 5: RUN MULTIPLE ORDINATION METHODS
###############################################

### 1. PCA (via RDA)
pca_res <- rda(species_hel)

### 2. CA
ca_res <- cca(species0)

### 3. NMDS (Bray-Curtis distance)
set.seed(123)
nmds_res <- metaMDS(species0,
                    distance = "bray",
                    k = 2,
                    trymax = 100)

### 4. (Optional) CCA if environmental variables exist
# Example placeholder (adapt if you have env variables)
# env_data <- data[,c("poll_no_poll","Harvest")]
# cca_res <- cca(species_data ~ poll_no_poll + Harvest, data = env_data)

###############################################
# PART 6: COMPARE MODEL QUALITY
###############################################

# NMDS stress (lower is better)
nmds_res$stress

# PCA explained variance
summary(pca_res)

# CA eigenvalues
summary(ca_res)

###############################################
# PART 7: PLOTS
###############################################

### PCA plot
plot(pca_res, main = "PCA")

### CA plot
plot(ca_res, main = "CA")

### NMDS plot
plot(nmds_res, type = "t", main = "NMDS")

#####################################################################################
library(stringr)

data_env <- data %>%
  mutate(
    physiotope = case_when(
      str_detect(pot_ID, "WS") ~ "wetstrip",
      str_detect(pot_ID, "FD") ~ "foredune",
      str_detect(pot_ID, "HD") ~ "highdensity",
      str_detect(pot_ID, "LD") ~ "lowdensity",
      str_detect(pot_ID, "_B_") ~ "bare",
      TRUE ~ NA_character_
    )
  )
