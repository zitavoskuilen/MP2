

###############################################
# PART 3: PREPARE SPECIES MATRIX
###############################################
# Your dataset structure:
# columns 1–5 = metadata (Done?, Harvest, pot_ID, poll_no_poll, Poskey)
# columns 6+ = species abundances

species_data <- data[,7:ncol(data)]
str(species_data)

# Replace NAs by 0 (important for community data)
species_data <- species_data %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

str(species_data)

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

view(species_data)

###############################################
# PART 4: DETERMINE BEST ORDINATION METHOD
###############################################
# Key idea from Lefcheck:
# Use DCA gradient length (axis 1) to decide:
# < 3  -> linear methods (PCA, RDA)
# 3–4  -> intermediate
# > 4  -> unimodal methods (CA, CCA, NMDS)

species0 <- species_data %>%
  dplyr::select(
    dplyr::where(~ sum(.x, na.rm = TRUE) > 0)
  ) %>%
  dplyr::filter(rowSums(.) > 0)                 # ✅ remove empty sites


# see how my data is connected, becasue the first dca axis is exaclty 1, there is ecological disconnectedness

connectivity <- distconnected(
  no.shared(species0),
  trace = TRUE
)

table(connectivity)

# which rows differ? > 43? 

split(
  rownames(species0),
  connectivity
)

# remove the group that doesn't have any species in common with the other group, so we can run DCA on the first group only
species_group1 <- species0[connectivity == 1, , drop = FALSE]

# Verwijder soorten die binnen deze groep niet voorkomen
species_group1 <- species_group1[
  ,
  colSums(species_group1, na.rm = TRUE) > 0,
  drop = FALSE
]

dca_group1 <- decorana(species_group1)

print(dca_group1)

axis_lengths <- apply(
  dca_group1$rproj,
  2,
  function(x) diff(range(x))
)

gradient_length <- unname(axis_lengths[1])

cat(
  "Gradient length DCA axis 1:",
  round(gradient_length, 3),
  "\n"
)


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

### 3. NMDS (Bray-Curtis distance)
set.seed(123)
nmds_res <- metaMDS(species_group1,
                    distance = "bray",
                    k = 2,
                    trymax = 100)

nmds_res


###############################################
# PART 6: COMPARE MODEL QUALITY
###############################################

# NMDS stress (lower is better)
nmds_res$stress
stressplot(nmds_res)
nrow(species_group1)
nrow(unique(species_group1))
site_scores <- as.data.frame(
  scores(nmds_res, display = "sites")
)

site_scores$sample_id <- rownames(site_scores)

head(site_scores)

###############################################
# PART 7: PLOTS
###############################################


#####
# zita zelf NMDS plot maken 
#####

# nu moeten we de metadata weer koppelen aan de site scores, zodat we kunnen zien welke samples bij elkaar horen

# Voeg oorspronkelijke rijnummers toe als ID
metadata_with_id <- data %>%
  mutate(sample_id = as.character(row_number()))

# De rownames van site_scores zijn normaal de behouden oorspronkelijke rijen
metadata_group1 <- metadata_with_id %>%
  filter(sample_id %in% rownames(site_scores)) %>%
  arrange(match(sample_id, rownames(site_scores)))



nrow(metadata_group1)
nrow(site_scores)

# checken of rij 43 erin zit? 
"43" %in% metadata_group1$sample_id
head(rownames(site_scores), 50)

# maar het is nu sample_id 44, dus die moet er nog uit 
metadata_group1 <- metadata_group1 %>%
  dplyr::filter(Poskey != "2026NLSurv_23")


metadata_group1 <- data %>%
  mutate(sample_id = as.character(row_number())) %>%
  filter(sample_id %in% rownames(site_scores)) %>%
  arrange(match(sample_id, rownames(site_scores)))

nrow(site_scores)
nrow(metadata_group1)



site_scores$sample_id <- rownames(site_scores)

plot_data <- site_scores %>%
  left_join(
    metadata_group1,
    by = "sample_id"
  )


