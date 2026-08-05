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
# select only the harvest HK2 en HK3 and TS3 and TS4

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
  ) %>%
  dplyr::select(-n_rows, -days, -Harvest_total)

str(data_two_summed_final)

###############################################
# PART 2: PREPARE SPECIES MATRIX
###############################################
# The dataset structure:
# columns 1 and 2 are pot_ID and physiotope, rest are species data.

species_data_two <- data_two_summed_final[,3:ncol(data_two_summed_final)]
str(species_data_two)

# Replace NAs by 0 (important for community data)
species_data_two <- species_data_two %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

str(species_data_two)

# remove columns with all zero's
species_data_two <- species_data_two[, colSums(species_data_two) > 0]


view(species_data_two)


###############################################
# PART 3: DETERMINE BEST ORDINATION METHOD
###############################################
# Key idea from Lefcheck:
# Use DCA gradient length (axis 1) to decide:
# < 3  -> linear methods (PCA, RDA)
# 3–4  -> intermediate
# > 4  -> unimodal methods (CA, CCA, NMDS)

species_dca <- species_data_two %>%
  dplyr::select(where(~ sum(.x, na.rm = TRUE) > 0)) %>%   # remove empty species
  filter(rowSums(.) > 0)                          # ✅ remove empty sites

dca_res <- decorana(species_dca)
print(dca_res) 

# Extract gradient length (first axis)

axis_lengths <- apply(
  dca_res$rproj,
  2,
  function(x) diff(range(x, na.rm = TRUE))
)

axis_lengths

gradient_length <- axis_lengths[1]


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
# PART 4: RUN MULTIPLE ORDINATION METHODS
###############################################

### 1. PCA
# first do the hellinger transformation 
# Hellinger transform (recommended for PCA) 

species_hel <- decostand(species_data_two, method = "hellinger")
pca_res <- rda(species_hel)

### 2. CA
ca_res <- cca(species_dca)

### 3. NMDS (Bray-Curtis distance)
set.seed(123)
nmds_res <- metaMDS(species_dca,
                    distance = "bray",
                    k = 2,
                    trymax = 100)

nmds_res


###############################################
# PART 5: COMPARE MODEL QUALITY
###############################################

# NMDS stress (lower is better)
nmds_res$stress

# PCA explained variance
summary(pca_res)

# CA eigenvalues
summary(ca_res)

###############################################
# PART 6: PLOTS
###############################################

### PCA plot
plot(pca_res, main = "PCA")

### CA plot
plot(ca_res, main = "CA")

### NMDS plot
plot(nmds_res, type = "t", main = "NMDS")

#####################################################################################
library(stringr)

data_env <- data_two %>%
  mutate(
    physiotope = case_when(
      str_detect(pot_ID, "_DS_") ~ "duneslack",
      str_detect(pot_ID, "_FD_") ~ "foredune",
      str_detect(pot_ID, "_HD_") ~ "highdensity",
      str_detect(pot_ID, "_LD_") ~ "lowdensity",
      str_detect(pot_ID, "_B_") ~ "bare",
      str_detect(pot_ID, "_B2_") ~ "bare2",
      str_detect(pot_ID, "_FD2_") ~ "foredune2",
      TRUE ~ NA_character_
    )
  )


###############################################
# PART 7: ADD GROUPING TO NMDS (OPTIONAL)
###############################################
# Example: color by pollination
group <- factor(data_env$physiotope)

ordiplot(nmds_res, type = "n")
points(nmds_res, display = "sites", col = as.numeric(group), pch = 19)
legend("topright", legend = levels(group), col = 1:length(levels(group)), pch = 19)

ordiellipse(
  nmds_res,
  groups = group,
  display = "sites",
  kind = "sd",
  draw = "polygon",
  col = col_vec,
  alpha = 50,
  border = col_vec
)


###############################################
# PART 8: ADD GROUPING TO PCA
###############################################

# Define grouping variable
group <- factor(data_env$physiotope)

# Define colours (adjust order if needed)
col_vec <- c("#1F7579","#BD7C0D","#D7B116","#561D25","#2F8011", "#6F4FA3", "#4FA3C7")


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
set.seed(10)
ordipointlabel(pca_res,
                display = "species",
                scaling = "symmetric",
                add = TRUE)

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

species_names <- rownames(scores(pca_res, display = "species"))
select_top3 <- species_names %in% top3

orditorp(
  pca_res,
  display = "species",
  scaling = "symmetric",
  select = select_top3,
  col = "red",
  cex = 0.8
)



# top 7 soorten toe te voegen 
##########
site_scores <- scores(
  pca_res,
  display = "sites",
  scaling = "symmetric"
)

species_scores <- scores(
  pca_res,
  display = "species",
  scaling = "symmetric"
)

# Top 7 soorten
top7 <- names(sort(dist_sp, decreasing = TRUE))[1:7]

top7

x_limits <- extendrange(
  c(site_scores[, 1], species_scores[top7, 1]),
  f = 0.15
)

y_limits <- extendrange(
  c(site_scores[, 2], species_scores[top7, 2]),
  f = 0.15
)

# Lege PCA-plot
plot(
  pca_res,
  display = "sites",
  type = "n",
  scaling = "symmetric",
  xlim = x_limits,
  ylim = y_limits
)

# Samples toevoegen
points(
  site_scores,
  pch = 19,
  col = col_vec[group]
)

# Alleen de top7 soorten toevoegen
text(
  species_scores[top7, 1],
  species_scores[top7, 2],
  labels = top7,
  col = "black",
  cex = 0.8,
  pos = 3
)

# Legenda
legend(
  "topright",
  legend = levels(group),
  col = col_vec,
  pch = 19,
  bty = "n"
)

# save the figure 
ggsave("plots/PCA_data_two.png", width = 8, height = 6, dpi = 300)

# NOW DO THE SAME BUT WITH DATAT FROM THREE DUTCH COAST 
# DATA THREE
##########

data_three <- data %>%
  dplyr::filter(harvest %in% c("HK2", "HK3", "HK1", "TS3", "TS4")) %>%
  dplyr::select(-total_individuals)


pot_ID_count <- data_three %>%
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
species_cols <- names(data_three)[9:ncol(data_three)]

data_summed <- data_three %>%
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
data_three_summed_final <- data_summed %>%
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
  ) %>%
  dplyr::select(-n_rows, -days, -Harvest_total)

str(data_three_summed_final)

###############################################
# PART 2: PREPARE SPECIES MATRIX
###############################################
# The dataset structure:
# columns 1 and 2 are pot_ID and physiotope, rest are species data.

species_data_three <- data_three_summed_final[,3:ncol(data_three_summed_final)]
str(species_data_three)

# Replace NAs by 0 (important for community data)
species_data_three <- species_data_three %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

str(species_data_three)

# remove columns with all zero's
species_data_three <- species_data_three[, colSums(species_data_three) > 0]


view(species_data_three)



###############################################
# PART 3: DETERMINE BEST ORDINATION METHOD
###############################################
# Key idea from Lefcheck:
# Use DCA gradient length (axis 1) to decide:
# < 3  -> linear methods (PCA, RDA)
# 3–4  -> intermediate
# > 4  -> unimodal methods (CA, CCA, NMDS)

species_dca_3 <- species_data_three %>%
  dplyr::select(where(~ sum(.x, na.rm = TRUE) > 0)) %>%   # remove empty species
  filter(rowSums(.) > 0)                          # ✅ remove empty sites

dca_res <- decorana(species_dca_3)
print(dca_res) 

# Extract gradient length (first axis)

axis_lengths <- apply(
  dca_res$rproj,
  2,
  function(x) diff(range(x, na.rm = TRUE))
)

axis_lengths

gradient_length <- axis_lengths[1]
###############################################
# PART 4: RUN MULTIPLE ORDINATION METHODS
###############################################

### 1. PCA
# first do the hellinger transformation 
# Hellinger transform (recommended for PCA) 

species_hel <- decostand(species_data_three, method = "hellinger")
pca_res <- rda(species_hel)

### 2. CA
ca_res <- cca(species_dca_3)

### 3. NMDS (Bray-Curtis distance)
set.seed(123)
nmds_res <- metaMDS(species_dca_3,
                    distance = "bray",
                    k = 2,
                    trymax = 100)

nmds_res
###############################################
# PART 5: COMPARE MODEL QUALITY
###############################################

# NMDS stress (lower is better)
nmds_res$stress

# PCA explained variance
summary(pca_res)

# CA eigenvalues
summary(ca_res)
###############################################
# PART 6: PLOTS
###############################################

### PCA plot
plot(pca_res, main = "PCA")

### CA plot
plot(ca_res, main = "CA")

### NMDS plot
plot(nmds_res, type = "t", main = "NMDS")

###############################################
# PART 7: ADD GROUPING TO NMDS (OPTIONAL)
###############################################
# Example: color by pollination
group <- factor(data_env$physiotope)

ordiplot(nmds_res, type = "n")
points(nmds_res, display = "sites", col = as.numeric(group), pch = 19)
legend("topright", legend = levels(group), col = 1:length(levels(group)), pch = 19)

ordiellipse(
  nmds_res,
  groups = group,
  display = "sites",
  kind = "sd",
  draw = "polygon",
  col = col_vec,
  alpha = 50,
  border = col_vec
)

# save this plot
ggsave("plots/NMDS_data_three.png", width = 8, height = 6, dpi = 300)


###############################################
# PART 8: ADD GROUPING TO PCA
###############################################

# Define grouping variable
group <- factor(data_env$physiotope)

# Define colours (adjust order if needed)
col_vec <- c("#1F7579","#BD7C0D","#D7B116","#561D25","#2F8011", "#6F4FA3", "#4FA3C7")


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
set.seed(10)
ordipointlabel(pca_res,
                display = "species",
                scaling = "symmetric",
                add = TRUE)

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

species_names <- rownames(scores(pca_res, display = "species"))
select_top3 <- species_names %in% top3

orditorp(
  pca_res,
  display = "species",
  scaling = "symmetric",
  select = select_top3,
  col = "red",
  cex = 0.8
)

ggsave("plots/PCA_data_three.png", width = 8, height = 6, dpi = 300)



# top 7 soorten toe te voegedn 