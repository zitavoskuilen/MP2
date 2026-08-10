####################

# ORDINATION WITH DATA_TWO 

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


#view(species_data_two)


###############################################
# PART 3: DETERMINE BEST ORDINATION METHOD
###############################################
# Key idea from Lefcheck:
# Use DCA gradient length (axis 1) to decide:
# < 3  -> linear methods (PCA, RDA)
# 3–4  -> intermediate
# > 4  -> unimodal methods (CA, CCA, NMDS)

species_dca_2 <- species_data_two %>%
  dplyr::select(where(~ sum(.x, na.rm = TRUE) > 0)) %>%   # remove empty species
  filter(rowSums(.) > 0)                          # ✅ remove empty sites

dca_res_2 <- decorana(species_dca_2)
print(dca_res_2) 

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

species_hel_2 <- decostand(species_data_two, method = "hellinger")
pca_res_2 <- rda(species_hel_2)

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

# stress = 0.2487336 which is quite high 

# PCA explained variance
summary(pca_res_2)

# CA eigenvalues
summary(ca_res)

###############################################
# PART 6: PLOTS
###############################################

### PCA plot
plot(pca_res_2, main = "PCA")

### CA plot
plot(ca_res, main = "CA")

### NMDS plot
plot(nmds_res, type = "t", main = "NMDS")

###########
# CHANGING LABELS 
###################

library(stringr)

data_env <- data_two_summed_final %>%
  mutate(
    physiotope = case_when(
      str_detect(pot_ID, "_DS")  ~ "duneslack",
      str_detect(pot_ID, "_FD2") ~ "foredune2",
      str_detect(pot_ID, "_FD")  ~ "foredune",
      str_detect(pot_ID, "_HD")  ~ "highdensity",
      str_detect(pot_ID, "_LD")  ~ "lowdensity",
      str_detect(pot_ID, "_B2")  ~ "bare2",
      str_detect(pot_ID, "_B")   ~ "bare",
      TRUE ~ NA_character_
    ),
    
    location = case_when(
      str_detect(pot_ID, "^KH_")  ~ "Kaap Hoorn",
      str_detect(pot_ID, "^SDL_") ~ "Schouwen-Duiveland",
      str_detect(pot_ID, "^IJM_") ~ "IJmuiden",
      str_detect(pot_ID, "^HBD_") ~ "Hondsbossche",
      str_detect(pot_ID, "^KWA_") ~ "Kwade Hoek",
      TRUE ~ NA_character_
    )
  )



###############################################
# PART 7: ADD GROUPING AND LABLES TO NMDS (OPTIONAL)
###############################################
# Example: color by LOCATION
group <- factor(data_env$location)

ordiplot(nmds_res, type = "n")
points(nmds_res, display = "sites", col = as.numeric(group), pch = 19)
legend("topright", legend = levels(group), col = 1:length(levels(group)), pch = 19)

# ADDING LABELS TO POINTS 
point_labels <- data_env$pot_ID

# NMDS-scores 
site_scores <- vegan::scores(
  nmds_res,
  display = "sites"
)

#PLOT
ordiplot(nmds_res, type = "n")

points(
  site_scores,
  col = as.numeric(group),
  pch = 19
)

#LABELS
text(
  site_scores[, 1],
  site_scores[, 2],
  labels = point_labels,
  pos = 4,
  cex = 0.7
)

#legend(
  "topright",
  legend = levels(group),
  col = as.numeric(group),
  pch = 19,
  bty = "n"
)

# Eén kleur per locatie
location_cols <- setNames(
  seq_along(levels(group)),
  levels(group)
)

# Kleuren in dezelfde volgorde als de factorlevels
ellipse_cols <- location_cols[levels(group)]


ordiellipse(
  nmds_res,
  groups = group,
  display = "sites",
  kind = "sd",
  draw = "polygon",
  col = as.numeric(group),
  alpha = 10,
  border = ellipse_cols
)


# save the plot 
ggsave("plots/NMDS_data_two_with_eliipse_location.png", width = 8, height = 6, dpi = 300)

# the same but then elipse for physiotope 


physio_group <- factor(data_env$physiotope)

physio_cols <- c(
  "bare"        = "#D9C28F",  # licht zand
  "bare2"       = "#BFA36A",  # donkerder zand
  "duneslack"   = "#4FA3A5",  # blauwgroen, natte duinvallei
  "foredune"    = "#E5B84B",  # geel/goud, helm en open duin
  "foredune2"   = "#C98F2E",  # donkerder goud
  "lowdensity"  = "#8FBF68",  # lichtgroen, lage vegetatiedichtheid
  "highdensity" = "#356B3A"   # donkergroen, dichte vegetatie
)

levels(physio_group)

physio_cols[levels(physio_group)]

sum(is.na(physio_cols[as.character(physio_group)]))

site_scores <- scores(nmds_res, display = "sites")

ordiplot(nmds_res, type = "n", main = "NMDS with Physiotope Grouping")

points(
  site_scores,
  col = physio_cols[as.character(physio_group)],
  pch = 19,
  cex = 1.2
)

ordiellipse(
  nmds_res,
  groups = physio_group,
  display = "sites",
  kind = "sd",
  draw = "polygon",
  col = adjustcolor(
    physio_cols[levels(physio_group)],
    alpha.f = 0.2
  ),
  border = physio_cols[levels(physio_group)]
)

legend(
  "topright",
  legend = levels(physio_group),
  col = physio_cols[levels(physio_group)],
  pch = 19,
  bty = "n"
)



###############################################
# PART 8: ADD GROUPING TO PCA BASED ON PHYSIOTOPE
###############################################

# Define grouping variable
group_phys <- factor(data_env$physiotope)
group_loc <- factor(data_env$location)

# Define colours (adjust order if needed)
phys_cols <- c(
  "bare"        = "#D9C28F",  # licht zand
  "bare2"       = "#BFA36A",  # donkerder zand
  "duneslack"   = "#4FA3A5",  # blauwgroen, beginnende duinvallei
  "foredune"    = "#E5B84B",  # geel/goud, helm en open duin
  "foredune2"   = "#C98F2E",  # donkerder goud
  "lowdensity"  = "#8FBF68",  # lichtgroen, lage vegetatiedichtheid
  "highdensity" = "#356B3A"   # donkergroen, dichte vegetatie
)

loc_shapes <- c(16, 17, 15, 18, 8)


site_scores <- scores(pca_res_2, display = "sites", scaling = "symmetric")

# Calculate distance from origin
dist_sp <- sqrt(sp_scores[,1]^2 + sp_scores[,2]^2)

# Select 3 most extreme species
top3 <- names(sort(dist_sp, decreasing = TRUE))[1:3]
top3

# Base PCA plot
plot(pca_res_2, display = "sites", type = "n", scaling = "symmetric", 
     main = "PCA  with Physiotope and Location Grouping (DATA_TWO)", 
     xlab = "PC1 (15.8%)", 
    ylab = "PC2 (13.1%)")

# Add sites (samples)
points(
  site_scores[,1],
  site_scores[,2],
  col = phys_cols[as.numeric(group_phys)],
  pch = loc_shapes[as.numeric(group_loc)],
  cex = 1.2
)

# Add species (optional)
#points(pca_res,
       display = "species",
       scaling = "symmetric",
       pch = 3,
       col = "black")

# Add species labels (optinal)
#set.seed(10)
#ordipointlabel(pca_res,
                display = "species",
                scaling = "symmetric",
                add = TRUE)

# Add ellipses per physiotope
ordiellipse(pca_res,
            groups = group_phys,
            draw = "polygon",
            col = phys_cols,
            scaling = "symmetric",
            kind = "sd",
            conf = 0.4)

# Add legend physiotpes 
legend("topright",
       legend = levels(group_phys),
       col = phys_cols,
       pch = 19,
       bty = "n")

# add legend locations 
legend("bottomright",
       legend = levels(group_loc),
       pch = loc_shapes,
       col = "black",
       bty = "n")

species_names <- rownames(scores(pca_res, display = "species"))
select_top3 <- species_names %in% top3

# dit toeveogen werkt nog niet
orditorp(
  pca_res,
  display = "species",
  scaling = "symmetric",
  select = select_top3,
  col = "black",
  cex = 0.8
)

# plot opslaan 
ggsave("plots/PCA_data_two_with_eliipse_physio_10/8.png", width = 8, height = 6, dpi = 300)


###############
# MAKE THE SAME ORDINATION PLOT BUT MAKE ELLIPS BASED ON LOCATION 
################

loc_col <- c(
  "Kaap Hoorn"        = "#1F7579",  # blauwgroen
  "Schouwen-Duiveland"= "#BD7C0D",  # oranje
  "IJmuiden"          = "#D7B116",  # geel
  "Hondsbossche"      = "#561D25",  # donkerrood
  "Kwade Hoek"        = "#2F8011"   # donkergroen
)

phys_shapes <- c(
  "bare"        = 16,  
  "bare2"       = 17,  
  "duneslack"   = 15,  
  "foredune"    = 18,  
  "foredune2"   = 8,   
  "lowdensity"  = 3,   
  "highdensity" = 4    
)


site_scores <- scores(pca_res_2, display = "sites", scaling = "symmetric")

# Calculate distance from origin
dist_sp <- sqrt(sp_scores[,1]^2 + sp_scores[,2]^2)


# Base PCA plot
plot(pca_res_2, display = "sites", type = "n", scaling = "symmetric", 
     main = "PCA  with Physiotope and Location Grouping (DATA_TWO)", 
     xlab = "PC1 (15.8%)", 
    ylab = "PC2 (13.1%)")

# Add sites (samples)
points(
  site_scores[,1],
  site_scores[,2],
  col = loc_col[as.numeric(group_loc)],
  pch = phys_shapes[as.numeric(group_phys)],
  cex = 1.2
)

# Add ellipses per location
ordiellipse(pca_res_2,
            groups = group_loc,
            draw = "polygon",
            col = loc_col,
            scaling = "symmetric",
            kind = "sd",
            conf = 0.4)

# Add legend location 
legend("topright",
       legend = levels(group_loc),
       col = loc_col,
       pch = 19,
       bty = "n")

# add legend physiotpes 
legend("bottomright",
       legend = levels(group_phys),
       pch = phys_shapes,
       col = "black",
       bty = "n")

# save the plot
ggsave("plots/PCA_data_two_with_eliipse_location_10_8.png", width = 8, height = 6, dpi = 300)


###########
# ADDING ENVIRONMENTAL DATA TO THE ORDINATION PLOT 
###########

# Loading in the data and ordining it on the same order as the species data 
data_two_summed_final

environmental_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQU4-VqDxuaSTUqruH83CrjeW6sTD95GdQlZnfCFKQLsLkcKOdmxxXD4G7mSRLf2AJeC3agHe8p_cOo/pub?gid=1256365017&single=true&output=csv")

view(environmental_data)

# make a new column of pot id which is site _ physiotope 
environmental_data <- environmental_data %>%
  mutate(pot_ID = paste(site,physiotope, sep = "_"))
environmental_data$pot_ID <- gsub(" ", "", environmental_data$pot_ID)

# orden the environmental data rows on the same order as the species data
environmental_data <- environmental_data[match(data_two_summed_final$pot_ID, environmental_data$pot_ID), ]



########

# ORDINATION PLOT WITH DATA THREE 

# NOW DO THE SAME BUT WITH DATA FROM THREE DUTCH COAST HARVESTS  
# DATA THREE
##########

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


# view(species_data_three)



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

dca_res_3 <- decorana(species_dca_3)
print(dca_res_3) 

# Extract gradient length (first axis)

axis_lengths <- apply(
  dca_res_3$rproj,
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

species_hel_3 <- decostand(species_data_three, method = "hellinger")
pca_res_3 <- rda(species_hel_3)

### 2. CA
ca_res <- cca(species_dca_3)

### 3. NMDS (Bray-Curtis distance)
set.seed(123)
nmds_res_3 <- metaMDS(species_dca_3,
                    distance = "bray",
                    k = 2,
                    trymax = 100)

nmds_res
###############################################
# PART 5: COMPARE MODEL QUALITY
###############################################

# NMDS stress (lower is better)
nmds_res$stress

# stress is 0.2449739 which is quite high 

# PCA explained variance
summary(pca_res_3)

# CA eigenvalues
summary(ca_res)
###############################################
# PART 6: PLOTS
###############################################

### PCA plot
plot(pca_res_3, main = "PCA")

### CA plot
plot(ca_res, main = "CA")

### NMDS plot
plot(pca_res_3, type = "t", main = "NMDS")

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
# PART 8: ADD GROUPING TO PCA BASED ON PHYSIOTOPE
###############################################

# make a new data_env from the data_three
data_env_3 <- data_three_summed_final %>%
  mutate(
    physiotope = case_when(
      str_detect(pot_ID, "_DS")  ~ "duneslack",
      str_detect(pot_ID, "_FD2") ~ "foredune2",
      str_detect(pot_ID, "_FD")  ~ "foredune",
      str_detect(pot_ID, "_HD")  ~ "highdensity",
      str_detect(pot_ID, "_LD")  ~ "lowdensity",
      str_detect(pot_ID, "_B2")  ~ "bare2",
      str_detect(pot_ID, "_B")   ~ "bare",
      TRUE ~ NA_character_
    ),
    
    location = case_when(
      str_detect(pot_ID, "^KH_")  ~ "Kaap Hoorn",
      str_detect(pot_ID, "^SDL_") ~ "Schouwen-Duiveland",
      str_detect(pot_ID, "^IJM_") ~ "IJmuiden",
      str_detect(pot_ID, "^HBD_") ~ "Hondsbossche",
      str_detect(pot_ID, "^KWA_") ~ "Kwade Hoek",
      TRUE ~ NA_character_
    )
  )

# Define grouping variable
group_phys <- factor(data_env_3$physiotope)
group_loc <- factor(data_env_3$location)


site_scores_3 <- scores(pca_res_3, display = "sites", scaling = "symmetric")

# Calculate distance from origin
dist_sp <- sqrt(sp_scores[,1]^2 + sp_scores[,2]^2)


# Base PCA plot
plot(pca_res_3, display = "sites", type = "n", scaling = "symmetric",
     main = "PCA  with Physiotope and Location Grouping (DATA_THREE)", 
     xlab = "PC1 (17.9%)", 
    ylab = "PC2 (13.9%)")

# Add sites (samples)
points(
  site_scores_3[,1],
  site_scores_3[,2],
  col = phys_cols[as.numeric(group_phys)],
  pch = loc_shapes[as.numeric(group_loc)],
  cex = 1.2
)

# Add ellipses per physiotope
ordiellipse(pca_res_3,
            groups = group,
            draw = "polygon",
            col = phys_cols,
            scaling = "symmetric",
            kind = "sd",
            conf = 0.4)

# Add legend
legend("topright",
       legend = levels(group_phys),
       col = phys_cols,
       pch = 19,
       bty = "n")

# add point shape legend 
legend("bottomright",
       legend = levels(group_loc),
       pch = loc_shapes,
       col = "black",
       bty = "n")



ggsave("plots/PCA_data_three.png", width = 8, height = 6, dpi = 300)

########
# MAKE THE SAME ORDINATION PLOT BUT MAKE ELLIPS BASED ON LOCATION 
######

site_scores <- scores(pca_res_3, display = "sites", scaling = "symmetric")


# Base PCA plot
plot(pca_res_3, display = "sites", type = "n", scaling = "symmetric", 
     main = "PCA  with Physiotope and Location Grouping (DATA_THREE)", 
     xlab = "PC1 (17.9%)", 
    ylab = "PC2 (13.9%)")

# Add sites (samples)
points(
  site_scores[,1],
  site_scores[,2],
  col = loc_col[as.numeric(group_loc)],
  pch = phys_shapes[as.numeric(group_phys)],
  cex = 1.2
)

# Add ellipses per location
ordiellipse(pca_res_3,
            groups = group_loc,
            draw = "polygon",
            col = loc_col,
            scaling = "symmetric",
            kind = "sd",
            conf = 0.4)

# Add legend location 
legend("topright",
       legend = levels(group_loc),
       col = loc_col,
       pch = 19,
       bty = "n")

# add legend physiotpes 
legend("bottomright",
       legend = levels(group_phys),
       pch = phys_shapes,
       col = "black",
       bty = "n")

# save the plot
ggsave("plots/PCA_data_thre_with_eliipse_location_10_8.png", width = 8, height = 6, dpi = 300)
