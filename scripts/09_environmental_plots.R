
## PCA WITH ENVIRONMENTAL VARIABLES BETWEEN PHYSTIOPES ####
# nog niet af want ik heb nog niet alle variables, en tot nu toe maken er veel ook niks uit 

env_pca_data <- envdata %>%
  dplyr::select(
    soil_moisture_percentage,
    soil_om_percentage,
    D50,
    grain_sorting, 
    PlantRichness,
    cover
    
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
group_loc <- factor(envdata$location)

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
  "grain_sorting"            = "Grain sorting", 
  "richness"                 = "Plant Richness"
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


## Boxplots of the individual environmental variables ####

env_long <- envdata %>%
  dplyr::select(
    physiotope,
    site,
    soil_moisture_percentage,
    soil_om_percentage,
    D50,
    grain_sorting,
    PlantRichness, 
    percentage_physiotope, 
    beach_width, 
    cover,
    slope
  ) %>%
  pivot_longer(
    cols = c(
      soil_moisture_percentage,
      soil_om_percentage,
      D50,
      grain_sorting,
      PlantRichness, 
      percentage_physiotope, 
    beach_width, 
    cover,
    slope
    ),
    names_to = "variable",
    values_to = "value"
  )

# supplementary figure 
env_long_supp <- env_long %>%
  dplyr::filter(
    variable %in% c(
      "beach_width",
      "slope",
      "percentage_physiotope"
    )
  )

env_long_supp$variable <- factor(
  env_long_supp$variable,
  levels = c(
    "beach_width",
    "slope",
    "percentage_physiotope"
  ),
  labels = c(
    "Beach width", 
    "Slope", 
    "Percentage Physiotope"
  )
)

# figure in report 
env_long_rest <- env_long %>%
  dplyr::filter(
    !variable %in% c(
      "beach_width",
      "slope",
      "percentage_physiotope"
    )
  )

env_long_rest$variable <- factor(
  env_long_rest$variable,
  levels = c(
    "soil_moisture_percentage",
    "soil_om_percentage",
    "D50",
    "grain_sorting",
    "PlantRichness", 
    "cover"
  ),
  labels = c(
    "Soil moisture (%)",
    "Soil organic matter (%)",
    "D50",
    "Grain sorting (D10/D90)",
    "Plant richness", 
    "Plant cover (%)"
  )
)

phys_cols <- c(
  "B"   = "#E8D7B0",
  "B2"  = "#9B6F3E",
  "DS"  = "#4FA3A5",
  "LD"  = "#8FBF68",
  "HD"  = "#356B3A",
  "FD"  = "#F2C94C",
  "FD2" = "#D97706"
)

# plot 
env_plot_supp <-ggplot(
  env_long_supp,
  aes(x = physiotope, y = value, fill = physiotope)
) +
  geom_boxplot(
    alpha = 0.6,
    outlier.shape = NA
  ) +
  geom_jitter(
    aes(color = physiotope),
    width = 0.15,
    size = 2,
    alpha = 0.8
  )+
  facet_wrap(
    ~ variable,
    scales = "free_y"
  ) +
  scale_fill_manual(values = phys_cols) +
  scale_color_manual(values = phys_cols) +
  labs(
    x = "Physiotope",
    y = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )


env_plot_supp


# save the plot 
ggsave("plots/env_boxplot_supp.png", width = 8, height = 6, dpi = 300)

# change order of physiotopes for the plot 
env_long_rest$physiotope <- factor(
  env_long_rest$physiotope,
  levels = c("B", "B2", "LD", "HD", "DS", "FD", "FD2")
)

# other plot 
env_plot_rest <-ggplot(
  env_long_rest,
  aes(x = physiotope, y = value, fill = physiotope)
) +
  geom_boxplot(
    alpha = 0.6,
    outlier.shape = NA
  ) +
  geom_jitter(
    aes(color = physiotope),
    width = 0.15,
    size = 2,
    alpha = 0.8
  )+
  facet_wrap(
    ~ variable,
    scales = "free_y"
  ) +
  scale_fill_manual(values = phys_cols) +
  scale_color_manual(values = phys_cols) +
  labs(
    x = "Physiotope",
    y = NULL
  ) +
  scale_y_continuous(
  expand = expansion(mult = c(0.05, 0.20))
) +
  theme_classic() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

env_plot_rest

# save the plot 
ggsave("plots/env_boxplot_rest.png", width = 8, height = 6, dpi = 300)
