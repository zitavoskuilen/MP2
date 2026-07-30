

###############################################
# PART 3: PREPARE SPECIES MATRIX
###############################################
# Your dataset structure:
# columns 1–5 = metadata (Done?, Harvest, pot_ID, poll_no_poll, Poskey)
# columns 6+ = species abundances

species_data <- data[,7:ncol(data)]

# Replace NAs by 0 (important for community data)
species_data <- species_data %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

# remove for now the columns that have non numeric values in the species columns (columns 6+)
# Identify columns containing non-empty, non-numeric values
bad_columns <- names(species_data)[
  sapply(species_data, function(x) {
    values <- trimws(as.character(x))

    any(
      !is.na(values) &
      values != "" &
      is.na(suppressWarnings(as.numeric(values)))
    )
  })
]


# remove some columns for now 

columns_to_remove <- c(
  "Anurida.maritima",
  "Clubiona.sp.",
 "Entomobryomorpha.sp.", 
 "Entomobryomorpha.sp..2", 
 "unknown", 
 "Collembola.sp."
)

species_data <- species_data %>%
  dplyr::select(-dplyr::all_of(columns_to_remove))

str(species_data)

# Remove columns and rows with all zeros (rare in sparse data)
species_data <- species_data[, colSums(species_data) > 0]
species_data[is.na(species_data)] <- 0
species_data <- species_data[
  rowSums(species_data, na.rm = TRUE) > 0,
  ,
  drop = FALSE
]


str(species_data)

sum(rowSums(species_data, na.rm = TRUE) == 0)
sum(colSums(species_data, na.rm = TRUE) == 0)


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
  dplyr::select(
    dplyr::where(~ sum(.x, na.rm = TRUE) > 0)
  ) %>%
  dplyr::filter(rowSums(.) > 0)                 # ✅ remove empty sites

dca_res <- decorana(species0)
print(dca_res) 


# Extract gradient length (first axis)
gradient_length <- dca_res$evals[1]
gradient_length

cat("Gradient length (DCA axis 1):", gradient_length, "\n") 


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
pca_res <- rda(species0)

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
 #env_data <- data[,c("poll_no_poll","Harvest")]
 #cca_res <- cca(species_data ~ poll_no_poll + Harvest, data = env_data)

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

ordiplot(pca_res,type="n")
orditorp(pca_res,display="species",col="red",air=0.01)
orditorp(pca_res,display="sites",cex=1.25,air=0.01)



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
      str_detect(pot_ID, "_B2_") ~ "bare2",
      str_detect(pot_ID, "FD2") ~ "foredune2",
      str_detect(pot_ID, "DS") ~ "drystrip",
      
      TRUE ~ NA_character_
    )
  )
###############################################
# PART 8: ADD GROUPING (OPTIONAL)
###############################################
# Example: color by pollination
group <- factor(data_env$physiotope)

ordiplot(nmds_res, type = "n")
points(nmds_res, display = "sites", col = as.numeric(group), pch = 19)
legend("topright", legend = levels(group), col = 1:length(levels(group)), pch = 19)


###############################################
# PART 8: ADD GROUPING TO PCA
###############################################


# Define grouping variable
group <- factor(data_env$physiotope)

# Define colours (adjust order if needed)
col_vec <- c("#1F7579","#BD7C0D","#D7B116","#561D25","#2F8011", "#6F4FA3", "#4FA3C7", "#D85C41")  


sp_scores <- scores(pca_res, display = "species", scaling = "symmetric")

# Calculate distance from origin
dist_sp <- sqrt(sp_scores[,1]^2 + sp_scores[,2]^2)

# Select 3 most extreme species
top3 <- names(sort(dist_sp, decreasing = TRUE))[1:3]
top3
# Base PCA plot
plot(pca_res, display = "sites", type = "n", scaling = "symmetric")

# Add sites (samples)
points(pca_res,
       display = "sites",
       scaling = "symmetric",
       pch = 19,
       col = col_vec[group])

# Add species (optional)
points(pca_res,
       display = "species",
       scaling = "symmetric",
       pch = 3,
       col = "black")

# Add species labels
# set.seed(10)
# ordipointlabel(pca_res,
#                display = "species",
#                scaling = "symmetric",
#                add = TRUE)

# Add ellipses per physiotope
ordiellipse(pca_res,
            groups = group,
            draw = "polygon",
            col = col_vec,
            scaling = "symmetric",
            kind = "sd",
            conf = 0.4)

# Add legend
legend("topright",
       legend = levels(group),
       col = col_vec,
       pch = 19,
       bty = "n")

species_out <- c("Anurida maritima", "Anthicus bimaculatus", "Linyphiidae")

# Add ONLY top 3 species labels
orditorp(pca_res,
         display = "species",
         scaling = "symmetric",
         select = species_out,
         col = "red")

