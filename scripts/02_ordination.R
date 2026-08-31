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

phys_cols <- c(
  "bare"        = "#E8D7B0",  # heel licht zand
  "bare2"       = "#9B6F3E",  # duidelijk donkerder bruin-zand
  
  "duneslack"   = "#4FA3A5",  # blauwgroen
  
  "foredune"    = "#F2C94C",  # helder geel
  "foredune2"   = "#D97706",  # duidelijk oranje
  
  "lowdensity"  = "#8FBF68",  # lichtgroen
  "highdensity" = "#356B3A"   # donkergroen
)

site_scores <- scores(pca_res_2, display = "sites", scaling = "symmetric")


# Site scores
site_scores <- scores(
  pca_res_2,
  display = "sites",
  scaling = "symmetric"
)

# Percentage variance explained automatisch berekenen
eig <- eigenvals(pca_res_2)
var_exp <- eig / sum(eig) * 100

# Kleuren per punt
point_cols <- phys_cols[as.numeric(group_phys)]

# PCA plot
plot(
  pca_res_2,
  display = "sites", type = "n", scaling = "symmetric",
  main = "PCA – Physiotope",
  xlab = paste0("PC1 (", round(var_exp[1], 1), "%)"),
  ylab = paste0("PC2 (", round(var_exp[2], 1), "%)"),
  las = 1,
  bty = "l", 
  xlim = c(-0.6, 1),
ylim = c(-0.8, 0.50)
)

# points 
points(
  site_scores[,1],
  site_scores[,2],
  pch = 21,                
  bg = point_cols,
  cex = 1,
  lwd = 0.8
)

# Ellipses per physiotope
ordiellipse(
  pca_res_2,
  groups = group_phys,
  display = "sites",
  scaling = "symmetric",
  kind = "sd",
  draw = "polygon",
  col = adjustcolor(
    phys_cols[seq_along(levels(group_phys))],
    alpha.f = 0.15
  ),
  border = phys_cols[seq_along(levels(group_phys))],
  lwd = 2
)

# Points again 
points(
  site_scores[,1],
  site_scores[,2],
  pch = 21,
  bg = point_cols,
  col = "grey20",
  cex = 1,
  lwd = 0.8
)


# Legenda

legend_labels <- c(
  "Bare",
  "Bare 2",
  "Duneslack",
  "Foredune",
  "Foredune 2",
  "High-density dunes",
  "Low-density dunes"
)

legend(
  "topright",
  legend = legend_labels,
  pt.bg = phys_cols[seq_along(levels(group_phys))],
  col = "grey20",
  pch = 21,
  pt.cex = 1.3,
  bty = "n",
  title = "Physiotopes"
)

# Calculate distance from origin
dist_sp <- sqrt(sp_scores[,1]^2 + sp_scores[,2]^2)

# Select 3 most extreme species
top3 <- names(sort(dist_sp, decreasing = TRUE))[1:3]
top3

top7 <- names(sort(dist_sp, decreasing = TRUE))[1:7]
top7


text(
  sp_scores[top7, 1],
  sp_scores[top7, 2],
  labels = gsub("_sp", "", top7),
  cex = 0.8,
  col = "#274C77",
  font = 3
)

# plot opslaan 
ggsave("plots/PCA_data_two_with_eliipse_physio_14_8.png", width = 8, height = 6, dpi = 300)


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


# make a new column of pot id which is site _ physiotope 
envdata <- envdata %>%
  mutate(pot_ID = paste(site,physiotope, sep = "_"))
envdata$pot_ID <- gsub(" ", "", envdata$pot_ID)

# orden the environmental data rows on the same order as the species data
envdata <- envdata[match(data_two_summed_final$pot_ID, envdata$pot_ID), ]

site_ids <- rownames(scores(pca_res_2, display = "sites"))
# de rownames van de pca_res zijn nog niet de pot_id's

nrow(species_hel_2)
nrow(envdata)

rownames(species_hel_2) <- envdata$pot_ID
pca_res_2 <- rda(species_hel_2)

# environmetnal data for the envfit function
env_vars <- envdata %>%
  dplyr::select(
    soil_moisture_percentage,
    soil_om_percentage,
    D50, 
    grain_sorting, 
    shannon, 
    richness
  )


env_labels <- c(
  "D50",
  "Soil moisture",
  "Grain sorting",
  "Soil OM",
  "Richness",
  "Shannon"
)

env_fit <- envfit(pca_res_2, env_vars, permutations = 999)

env_fit


#plot environmental variables to the earlier plot 
plot(env_fit, add = T,  col = "grey30", labels = env_labels, p.max = 0.05 )

# save the plot!!!
dev.copy(
  png,
  filename = "plots/PCA_physiotope.png",
  width = 4000,
  height = 1800,
  res = 300,
  bg = "white"
)

dev.off()

############
## PCA WITH ENVIRONMENTAL VARIABLES BETWEEN PHYSTIOPES 
# nog niet af want ik heb nog niet alle variables, en tot nu toe maken er veel ook niks uit 
###########

env_pca_data <- envdata %>%
  dplyr::select(
    soil_moisture_percentage,
    soil_om_percentage,
    D50,
    grain_sorting
  )

# correlatie van de variabelen testen 
library(corrplot)

cor_env <- cor(
  env_pca_data,
  use = "complete.obs"
)

corrplot(
  cor_env,
  method = "color",
  type = "upper",
  addCoef.col = "black"
)


env_pca <- rda(
  env_pca_data,
  scale = TRUE
)

summary(env_pca)

# scores
env_site_scores <- scores(
  env_pca,
  display = "sites",
  scaling = "symmetric"
)

env_var_scores <- scores(
  env_pca,
  display = "species",
  scaling = "symmetric"
)

group_phys <- factor(envdata$physiotope)

var_env <- env_pca$CA$eig / sum(env_pca$CA$eig) * 100

env_var_scores

# PCA
plot(
  env_pca,
  display = "sites",
  type = "n",
  scaling = "symmetric",
  xlab = paste0("PC1 (", round(var_env[1], 1), "%)"),
  ylab = paste0("PC2 (", round(var_env[2], 1), "%)"),
  main = "PCA of environmental variables", 
  xlim = c(-1, 2.4),
ylim = c(-2.2, 1.8))


point_cols <- phys_cols[as.numeric(group_phys)]

# points 
points(
  env_site_scores[,1],
  env_site_scores[,2],
  pch = 21,
  bg = point_cols,
  col = "grey20",
  cex = 1.3
)


# adding ellipses per phystiope 
ordiellipse(
  env_pca,
  groups = group_phys,
  display = "sites",
  scaling = "symmetric",
  kind = "sd",
  draw = "polygon",
  col = adjustcolor(
    phys_cols[seq_along(levels(group_phys))],
    alpha.f = 0.15
  ),
  border = phys_cols[seq_along(levels(group_phys))],
  lwd = 2
)

# adding the arrows with the labels for the environmental factors 
env_var_scores_plot <- env_var_scores * 0.7
arrows(
  x0 = 0,
  y0 = 0,
  x1 = env_var_scores_plot[,1],
  y1 = env_var_scores_plot[,2],
  length = 0.08,
  col = "grey30",
  lwd = 1.2
)

env_labels <- c(
  "soil_moisture_percentage" = "Soil moisture",
  "soil_om_percentage"       = "Soil OM",
  "D50"                      = "D50",
  "grain_sorting"            = "Grain sorting"
)

text(
  env_var_scores_plot[,1],
  env_var_scores_plot[,2],
  labels = env_labels[rownames(env_var_scores_plot)],
  pos = 4,
  offset = 0.3,
  cex = 0.8,
  col = "grey20"
)

# adding a legend for the colours
legend(
  "topright",
  legend = legend_labels,
  pt.bg = phys_cols[seq_along(levels(group_phys))],
  col = "grey20",
  pch = 21,
  pt.cex = 1.3,
  bty = "n",
  title = "Physiotopes"
)

# save the plot! 
dev.copy(
  png,
  filename = "plots/PCA_env_variables.png",
  width = 4000,
  height = 1800,
  res = 300,
  bg = "white"
)  

dev.off()

# adding the labels of the pot_id's 
# orditorp(
  env_pca,
  display = "sites",
  scaling = "symmetric",
  labels = envdata$pot_ID,
  cex = 0.6,
  col = "grey20"
)

