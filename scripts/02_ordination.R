#
#
##############################################
##############################################
# PART 1: MAKE TWO DATASETS

# DATA_TWO =  TWO HARVESTS OF EVERY LOCATION 
# DATA_THREE = TWO HARVESTS OF TS AND THREE OF THE DUTCH COAST 
###########
# DATA_TWO 
###################
# select only the harvest HK2 en HK3 and TS3 and T4

data_two <- data %>%
  dplyr::filter(harvest %in% c("HK2", "HK3", "TS3", "TS4")) %>%
  dplyr::select(-total_individuals)


pot_ID_count <- data_two %>%
  count(pot_ID)

# now i have to add the rows toegteher that have the same pot_ID 
# first i'll do ot for the harvests seperately 

metadata_cols <- c(
  "Done.",
  "Harvest_total",
  "harvest",
  "days",
  "pot_ID",
  "physiotope",
  "poll_no_poll",
  "Poskey"
)

# Alle overige kolommen zijn soortkolommen
species_cols <- names(data_two)[9:ncol(data_two)]

data_summed <- data_two %>%
  mutate(
    across(
      all_of(species_cols),
      ~ {
        x <- trimws(as.character(.x))
        x[x == ""] <- NA
        suppressWarnings(as.numeric(x))
      }
    )
  ) %>%
  mutate(
    pot_group = sub("_[^_]+$", "", pot_ID)
  ) %>%
  group_by(harvest, pot_group) %>%
  summarise(
    Harvest_total = first(Harvest_total),
    days = first(days),
    physiotope = first(physiotope),
    n_rows = n(),
    across(
      all_of(species_cols),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  rename(pot_ID = pot_group)


# now add all the pot_ID's together that have the same pot_ID but different harvests
data_two_summed_final <- data_summed %>%
  group_by(pot_ID) %>%
  summarise(
    Harvest_total = first(Harvest_total),
    days = first(days),
    physiotope = first(physiotope),
    n_rows = sum(n_rows),
    across(
      all_of(species_cols),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )





















# DATA THREE 

###############################################
# PART 3: PREPARE SPECIES MATRIX
###############################################
# Your dataset structure:
# columns 1–5 = metadata (Done?, Harvest, pot_ID, poll_no_poll, Poskey)
# columns 6+ = species abundances

species_data <- data[,8:ncol(data)]
str(species_data)

# Replace NAs by 0 (important for community data)
species_data <- species_data %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

str(species_data)

# remove columns with all zero's
species_data <- species_data[, colSums(species_data) > 0]


view(species_data)

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

#Janne doet hell transformatie voor de dca? 
species0 <- species_hel %>%
  dplyr::select(
    dplyr::where(~ sum(.x, na.rm = TRUE) > 0)
  ) %>%
  dplyr::filter(
    rowSums(.) > 0
  )

# Zita > dan krijg je eerste dca as van precies 1 
species0 <- species_data %>%
  dplyr::select(where(~ sum(.x, na.rm = TRUE) > 0)) %>%   # remove empty species
  filter(rowSums(.) > 0)                          # ✅ remove empty sites

dca_res <- decorana(species0)
print(dca_res) 

# Extract gradient length (first axis)
gradient_length <- dca_res$evals[1]
gradient_length

dca_res$

cat("Gradient length (DCA axis 1):", gradient_length, "\n") # = 0.5 dus PCA RDA

###########
# DATA IS DISCONNETED (DCA axis 1 is precies 1), so we need to check which rows are disconnected and remove them before running DCA again
##########
# see how my data is connected, becasue the first dca axis is exaclty 1, there is ecological disconnectedness

connectivity <- distconnected(
  no.shared(species0),
  trace = TRUE
)


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


