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
# PART 8: ADD GROUPING TO PCA BASED ON PHYSIOTOPE
###############################################

# Define grouping variable
group_phys <- factor(data_env$physiotope)

phys_cols <- c(
  "bare"        = "#E8D7B0",  # heel licht zand
  "bare2"       = "#9B6F3E",  # duidelijk donkerder bruin-zand
  "duneslack"   = "#4FA3A5",  # blauwgroen
  "lowdensity"  = "#8FBF68",  # lichtgroen
  "highdensity" = "#356B3A",  # donkergroen
  "foredune"    = "#F2C94C",  # helder geel
  "foredune2"   = "#D97706"  # duidelijk oranje
)

######## PLOT

# Site scores
site_scores <- scores(
  pca_res_2,
  display = "sites",
  scaling = "symmetric"
)

# Percentage variance explained
eig <- eigenvals(pca_res_2)
var_exp <- eig / sum(eig) * 100

# BELANGRIJK: kleuren koppelen aan naam physiotope
point_cols <- phys_cols[as.character(group_phys)]

# volgorde die ordiellipse gebruikt
group_order <- names(table(group_phys))
ellipse_cols <- phys_cols[group_order]


################################################
## PCA plot

par(mar = c(5, 4, 4, 12), xpd = FALSE)

plot(
  site_scores[, 1],
  site_scores[, 2],
  type = "n",
  main = "PCA – Physiotope",
  xlab = paste0("PC1 (", round(var_exp[1], 1), "%)"),
  ylab = paste0("PC2 (", round(var_exp[2], 1), "%)"),
  las = 1,
  bty = "l",
  xlim = c(-0.8, 1),
  ylim = c(-0.9, 0.6),
  xaxs = "i",
  yaxs = "i"
)

abline(
  h = 0,
  v = 0,
  lty = 2,
  col = "grey70"
)

# Ellipsen FIRST
ordiellipse(
  pca_res_2,
  groups = group_phys,
  display = "sites",
  scaling = "symmetric",
  kind = "sd",
  draw = "polygon",
  col = adjustcolor(
    ellipse_cols,alpha.f = 0.05
  ),
  border = adjustcolor(
    ellipse_cols, alpha.f = 0.6
  ),
  lwd = 1.3
)

# Punten DAARNA
points(
  site_scores[, 1],
  site_scores[, 2],
  col = point_cols,
  pch = 19,
  cex = 1)


legend_order <- c(
  "bare",
  "bare2", 
  "lowdensity",
  "highdensity",
 "duneslack",
  "foredune",
  "foredune2"  
)

# adding legend 
legend("right",
        inset = c(-0.28, 0),
       xpd = NA,
       legend = c(
    "Bare",
    "Bare 2",    
    "Low-density dunes",
    "High-density dunes", 
    "Green Beach",
    "Foredune",
    "Foredune 2"
    
  ),
       col = phys_cols[legend_order],
       pch = 19,
       bty = "n",
       title = "Physiotope",
       cex = 0.8
)

# Calculate distance from origin
dist_sp <- sqrt(sp_scores[,1]^2 + sp_scores[,2]^2)

# Select 3 most extreme species
top3 <- names(sort(dist_sp, decreasing = TRUE))[1:3]
top3

top7 <- names(sort(dist_sp, decreasing = TRUE))[1:7]
top7

top10 <- names(sort(dist_sp, decreasing = T))[1:10]
top10

text(
  sp_scores[top10, 1],
  sp_scores[top10, 2],
  labels = gsub("_sp", "", top10),
  cex = 0.8,
  col = "#274C77",
  font = 3
)

dev.copy(
  png,
  filename = "PCA_phys.png",
  width = 9,
  height = 6,
  units = "in",
  res = 600
)

dev.off()

################### 


###############
# MAKE THE SAME ORDINATION PLOT BUT MAKE ELLIPS BASED ON LOCATION 
################
group_loc <- factor(data_env$location)

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

