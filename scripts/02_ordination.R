
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
