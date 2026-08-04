################
# Description of the environmental data 
###############
# load in the data 
###############

envdata <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQU4-VqDxuaSTUqruH83CrjeW6sTD95GdQlZnfCFKQLsLkcKOdmxxXD4G7mSRLf2AJeC3agHe8p_cOo/pub?gid=1256365017&single=true&output=csv")

envdata
str(envdata)

################
# 01. Environmental variables between phystiotopes
###############


env_summary <- envdata %>%
  group_by(physiotope) %>%
  summarise(
    mean_soil_moisture = mean(soil_moisture_percentage, na.rm = TRUE),
    mean_soil_om = mean(soil_om_percentage, na.rm = TRUE),
    mean_D50 = mean(D50, na.rm = TRUE),
    .groups = "drop"
  )

env_summary_long <- env_summary %>%
  pivot_longer(
    cols = -physiotope,
    names_to = "variable",
    values_to = "mean"
  ) %>%
  mutate(
    variable = case_when(
      variable == "mean_soil_moisture_percentage" ~ "Soil moisture (%)",
      variable == "mean_soil_om_percentage" ~ "Soil organic matter (%)",
      variable == "mean_D50" ~ "Median grain size D50 (µm)",
      TRUE ~ variable
    ),
    physiotope = factor(
      physiotope,
      levels = c("B", "DS", "WS", "LD", "HD", "FD", "B2", "FD2")
    )
  )

# plot in 3 panels 
plot1 <- ggplot(
  env_summary_long,
  aes(x = physiotope, y = mean, fill = physiotope)
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(
    ~ variable,
    scales = "free_y",
    nrow = 1
  ) +
  labs(
    x = "Physiotope",
    y = "Mean value per physiotope"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text()
  )

plot1
   
# save the plot
ggsave(
  filename = "plots/env_summary_plot.png",
  plot = plot1,
  width = 10,
  height = 5,
  dpi = 300
)
   
################
# 02. Environmental variables between phystiotopes & location
###############


env_summary_site <- envdata %>%
  group_by(site , physiotope) %>%
  summarise(
    mean_soil_moisture = mean(soil_moisture_percentage, na.rm = TRUE),
    mean_soil_om = mean(soil_om_percentage, na.rm = TRUE),
    mean_D50 = mean(D50, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(mean_soil_moisture, mean_soil_om, mean_D50),
    names_to = "variable",
    values_to = "mean"
  ) %>%
  mutate(
    variable = factor(
      variable,
      levels = c(
        "mean_soil_moisture",
        "mean_soil_om",
        "mean_D50"
      ),
      labels = c(
        "Soil Moisture (%)",
        "Organic matter (%)",
        "Median D50 (µm)"
      )
    ),
    physiotope = factor(
      physiotope,
      levels = c("B", "DS", "WS", "LD", "HD", "FD", "B2", "FD2")
    )
  )


# plot 

plot2 <- ggplot(
  env_summary_site,
  aes(x = physiotope, y = mean, fill = physiotope)
) +
  geom_col(show.legend = FALSE) +
  facet_grid(variable ~ site, scales = "free_y") +
  labs(
    x = "Physiotope",
    y = ""
  ) +
  theme_classic2() +
  theme(
    strip.text = element_text()
  )

plot2

# save the plot
ggsave(
  filename = "plots/env_summary_site_plot.png",
  plot = plot2,
  width = 10,
  height = 5,
  dpi = 300
)

############
# 03. Grain size distribution per location & physiotope with D10, D50 and D90
############

envdata

grain_plot_data <- envdata %>%
  mutate(
    site_physiotope = paste(site,physiotope, sep = "-"),
    physiotope = factor(
      physiotope,
      levels = c("B", "DS", "WS", "LD", "HD", "FD", "B2", "FD2")
    )
  )


plot3 <- ggplot(grain_plot_data, aes(y = physiotope)) +
  geom_segment((aes(x = D10, xend = D90, yend = physiotope, color = physiotope)), linewidth = 1) + 
  geom_point(aes(x = D50, color = physiotope), size = 3) +
  labs(
    x = "Grain size (µm)",
    y = "Physiotope"
  ) +
  facet_wrap(~site)

# save the plot 
ggsave(filename = "plots/grain_size_plot.png",
  plot = plot2,
  width = 10,
  height = 5,
  dpi = 300
       )


###############
# 04. FLora per site individually 
##############

# load flora data 

flora_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQU4-VqDxuaSTUqruH83CrjeW6sTD95GdQlZnfCFKQLsLkcKOdmxxXD4G7mSRLf2AJeC3agHe8p_cOo/pub?gid=144268895&single=true&output=csv")

str(flora_data)

################
# Description of the environmental data 
###############
# load in the data 
###############

envdata <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQU4-VqDxuaSTUqruH83CrjeW6sTD95GdQlZnfCFKQLsLkcKOdmxxXD4G7mSRLf2AJeC3agHe8p_cOo/pub?gid=1256365017&single=true&output=csv")

envdata
str(envdata)

################
# 01. Environmental variables between phystiotopes
###############


env_summary <- envdata %>%
  group_by(physiotope) %>%
  summarise(
    mean_soil_moisture = mean(soil_moisture_percentage, na.rm = TRUE),
    mean_soil_om = mean(soil_om_percentage, na.rm = TRUE),
    mean_D50 = mean(D50, na.rm = TRUE),
    .groups = "drop"
  )

env_summary_long <- env_summary %>%
  pivot_longer(
    cols = -physiotope,
    names_to = "variable",
    values_to = "mean"
  ) %>%
  mutate(
    variable = case_when(
      variable == "mean_soil_moisture_percentage" ~ "Soil moisture (%)",
      variable == "mean_soil_om_percentage" ~ "Soil organic matter (%)",
      variable == "mean_D50" ~ "Median grain size D50 (µm)",
      TRUE ~ variable
    ),
    physiotope = factor(
      physiotope,
      levels = c("B", "DS", "WS", "LD", "HD", "FD", "B2", "FD2")
    )
  )

# plot in 3 panels 
plot1 <- ggplot(
  env_summary_long,
  aes(x = physiotope, y = mean, fill = physiotope)
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(
    ~ variable,
    scales = "free_y",
    nrow = 1
  ) +
  labs(
    x = "Physiotope",
    y = "Mean value per physiotope"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text()
  )

plot1
   
# save the plot
ggsave(
  filename = "plots/env_summary_plot.png",
  plot = plot1,
  width = 10,
  height = 5,
  dpi = 300
)
   
################
# 02. Environmental variables between phystiotopes & location
###############


env_summary_site <- envdata %>%
  group_by(site , physiotope) %>%
  summarise(
    mean_soil_moisture = mean(soil_moisture_percentage, na.rm = TRUE),
    mean_soil_om = mean(soil_om_percentage, na.rm = TRUE),
    mean_D50 = mean(D50, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(mean_soil_moisture, mean_soil_om, mean_D50),
    names_to = "variable",
    values_to = "mean"
  ) %>%
  mutate(
    variable = factor(
      variable,
      levels = c(
        "mean_soil_moisture",
        "mean_soil_om",
        "mean_D50"
      ),
      labels = c(
        "Soil Moisture (%)",
        "Organic matter (%)",
        "Median D50 (µm)"
      )
    ),
    physiotope = factor(
      physiotope,
      levels = c("B", "DS", "WS", "LD", "HD", "FD", "B2", "FD2")
    )
  )


# plot 

plot2 <- ggplot(
  env_summary_site,
  aes(x = physiotope, y = mean, fill = physiotope)
) +
  geom_col(show.legend = FALSE) +
  facet_grid(variable ~ site, scales = "free_y") +
  labs(
    x = "Physiotope",
    y = ""
  ) +
  theme_classic2() +
  theme(
    strip.text = element_text()
  )

plot2

# save the plot
ggsave(
  filename = "plots/env_summary_site_plot.png",
  plot = plot2,
  width = 10,
  height = 5,
  dpi = 300
)

############
# 03. Grain size distribution per location & physiotope with D10, D50 and D90
############

envdata

grain_plot_data <- envdata %>%
  mutate(
    site_physiotope = paste(site,physiotope, sep = "-"),
    physiotope = factor(
      physiotope,
      levels = c("B", "DS", "WS", "LD", "HD", "FD", "B2", "FD2")
    )
  )


plot3 <- ggplot(grain_plot_data, aes(y = physiotope)) +
  geom_segment((aes(x = D10, xend = D90, yend = physiotope, color = physiotope)), linewidth = 1) + 
  geom_point(aes(x = D50, color = physiotope), size = 3) +
  labs(
    x = "Grain size (µm)",
    y = "Physiotope"
  ) +
  facet_wrap(~site)

# save the plot 
ggsave(filename = "plots/grain_size_plot.png",
  plot = plot2,
  width = 10,
  height = 5,
  dpi = 300
       )


###############
# 04. FLora per site individually 
##############

# load flora data 

flora_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQU4-VqDxuaSTUqruH83CrjeW6sTD95GdQlZnfCFKQLsLkcKOdmxxXD4G7mSRLf2AJeC3agHe8p_cOo/pub?gid=144268895&single=true&output=csv")

str(flora_data)

flora_data <- flora_data[2,3,10:ncol(flora_data)]

################
# Description of the environmental data 
###############
# load in the data 
###############

envdata <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQU4-VqDxuaSTUqruH83CrjeW6sTD95GdQlZnfCFKQLsLkcKOdmxxXD4G7mSRLf2AJeC3agHe8p_cOo/pub?gid=1256365017&single=true&output=csv")

envdata
str(envdata)

################
# 01. Environmental variables between phystiotopes
###############


env_summary <- envdata %>%
  group_by(physiotope) %>%
  summarise(
    mean_soil_moisture = mean(soil_moisture_percentage, na.rm = TRUE),
    mean_soil_om = mean(soil_om_percentage, na.rm = TRUE),
    mean_D50 = mean(D50, na.rm = TRUE),
    .groups = "drop"
  )

env_summary_long <- env_summary %>%
  pivot_longer(
    cols = -physiotope,
    names_to = "variable",
    values_to = "mean"
  ) %>%
  mutate(
    variable = case_when(
      variable == "mean_soil_moisture_percentage" ~ "Soil moisture (%)",
      variable == "mean_soil_om_percentage" ~ "Soil organic matter (%)",
      variable == "mean_D50" ~ "Median grain size D50 (µm)",
      TRUE ~ variable
    ),
    physiotope = factor(
      physiotope,
      levels = c("B", "DS", "WS", "LD", "HD", "FD", "B2", "FD2")
    )
  )

# plot in 3 panels 
plot1 <- ggplot(
  env_summary_long,
  aes(x = physiotope, y = mean, fill = physiotope)
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(
    ~ variable,
    scales = "free_y",
    nrow = 1
  ) +
  labs(
    x = "Physiotope",
    y = "Mean value per physiotope"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text()
  )

plot1
   
# save the plot
ggsave(
  filename = "plots/env_summary_plot.png",
  plot = plot1,
  width = 10,
  height = 5,
  dpi = 300
)
   
################
# 02. Environmental variables between phystiotopes & location
###############


env_summary_site <- envdata %>%
  group_by(site , physiotope) %>%
  summarise(
    mean_soil_moisture = mean(soil_moisture_percentage, na.rm = TRUE),
    mean_soil_om = mean(soil_om_percentage, na.rm = TRUE),
    mean_D50 = mean(D50, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(mean_soil_moisture, mean_soil_om, mean_D50),
    names_to = "variable",
    values_to = "mean"
  ) %>%
  mutate(
    variable = factor(
      variable,
      levels = c(
        "mean_soil_moisture",
        "mean_soil_om",
        "mean_D50"
      ),
      labels = c(
        "Soil Moisture (%)",
        "Organic matter (%)",
        "Median D50 (µm)"
      )
    ),
    physiotope = factor(
      physiotope,
      levels = c("B", "DS", "WS", "LD", "HD", "FD", "B2", "FD2")
    )
  )


# plot 

plot2 <- ggplot(
  env_summary_site,
  aes(x = physiotope, y = mean, fill = physiotope)
) +
  geom_col(show.legend = FALSE) +
  facet_grid(variable ~ site, scales = "free_y") +
  labs(
    x = "Physiotope",
    y = ""
  ) +
  theme_classic2() +
  theme(
    strip.text = element_text()
  )

plot2

# save the plot
ggsave(
  filename = "plots/env_summary_site_plot.png",
  plot = plot2,
  width = 10,
  height = 5,
  dpi = 300
)

############
# 03. Grain size distribution per location & physiotope with D10, D50 and D90
############

envdata

grain_plot_data <- envdata %>%
  mutate(
    site_physiotope = paste(site,physiotope, sep = "-"),
    physiotope = factor(
      physiotope,
      levels = c("B", "DS", "WS", "LD", "HD", "FD", "B2", "FD2")
    )
  )


plot3 <- ggplot(grain_plot_data, aes(y = physiotope)) +
  geom_segment((aes(x = D10, xend = D90, yend = physiotope, color = physiotope)), linewidth = 1) + 
  geom_point(aes(x = D50, color = physiotope), size = 3) +
  labs(
    x = "Grain size (µm)",
    y = "Physiotope"
  ) +
  facet_wrap(~site)

# save the plot 
ggsave(filename = "plots/grain_size_plot.png",
  plot = plot2,
  width = 10,
  height = 5,
  dpi = 300
       )


###############
# 04. FLora per site as species and as families  
##############

# load flora data 

flora_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQU4-VqDxuaSTUqruH83CrjeW6sTD95GdQlZnfCFKQLsLkcKOdmxxXD4G7mSRLf2AJeC3agHe8p_cOo/pub?gid=144268895&single=true&output=csv")

str(flora_data)

# select only the columns that include site and physiotope and all the species 
flora_data <- flora_data %>%
  dplyr::select(2, 3, 10:dplyr::last_col())

str(flora_data)

# change all NA's to zero's 
flora_data[is.na(flora_data)] <- 0

# make all columns numeric except for site and physiotope
flora_data <- flora_data %>%
  mutate(across(-c(site, physiotope), as.numeric))

str(flora_data) # all x's have now become zero's which is not what should be in the end but for now is fine 

# now i want to make a bar plot for every site with all the physiotpes as 1 bar with parts of the bars as species 
# first convert to long format 


flora_long <- flora_data %>%
  pivot_longer(cols = 3:ncol(flora_data), 
               names_to = "species", 
               values_to = "cover") %>%
  filter(cover >0)


# make the barplot 

flora_plot <- ggplot(flora_long, aes(x = physiotope, y = cover, fill = species)) +
  geom_col() + facet_wrap(~site, scales = "free_y") +
  labs(x = "Physiotope", y = "Total plant cover", fill = "Species")

flora_plot  

# make a new table with the families the species belong to as the bar plot is too crowded for species accurancy 
# assing a family to each species 

species_family <- tibble::tribble(
  ~species,                       ~family,
  "Calamagrostis_arenaria",       "Poaceae",
  "Elytrigia_atherica",           "Poaceae",
  "Elytrigia_juncea",             "Poaceae",
  "Senecio_vulgaris",             "Asteraceae",
  "Sonchus_palustris",            "Asteraceae",
  "Sonchus_oleraceus",            "Asteraceae",
  "Oenothera_biennis",            "Onagraceae",
  "Hypochaeris_radicata",         "Asteraceae",
  "Cerastium_diffusum",           "Caryophyllaceae",
  "Draba_verna",                  "Brassicaceae",
  "Leontodon_saxatilis",          "Asteraceae",
  "Jacobaea_vulgaris",            "Asteraceae",
  "Bryophyta_spec2",              "Unidentified bryophyte",
  "Trifolium_sp.",                "Fabaceae",
  "Myosotis_ramosissima",         "Boraginaceae",
  "Hieracium_pilosella",          "Asteraceae",
  "Leymus_arenarius",             "Poaceae",
  "Tragopogon_pratensis",         "Asteraceae",
  "Hieracium_umbellatum",         "Asteraceae",
  "Salix_repens",                 "Salicaceae",
  "Cirsium_arvense",              "Asteraceae",
  "Glaux_maritima",               "Primulaceae",
  "Cerastium_fontanum",           "Caryophyllaceae",
  "Syntrichia_ruralis",           "Pottiaceae",
  "Spergularia_rubra",            "Caryophyllaceae",
  "Juncus_articulatus",           "Juncaceae",
  "Cirsium_vulgare",              "Asteraceae",
  "Samolus_valerandi",            "Primulaceae",
  "Plantago_major",               "Plantaginaceae",
  "Juncus_maritimus",             "Juncaceae",
  "Phragmites_australis",         "Poaceae",
  "Euphorbia_paralias",           "Euphorbiaceae",
  "Lythrum_salicaria",            "Lythraceae",
  "Hippophae_rhamnoides",         "Elaeagnaceae",
  "Poaceae_spec",                 "Poaceae",
  "Littorella_uniflora",          "Plantaginaceae",
  "Plantago_coronopus",           "Plantaginaceae",
  "Apiaceae_spec",                "Apiaceae",
  "Eleocharis_palustris",         "Cyperaceae",
  "X2026_512",                    "Unknown",
  "Taraxacum_officinale",         "Asteraceae",
  "Holcus_lanatus",               "Poaceae",
  "Aira_praecox",                 "Poaceae",
  "Jasione_montana",              "Campanulaceae",
  "Phleum_arenarium",             "Poaceae",
  "Sedum_acre",                   "Crassulaceae",
  "Bryophyta_spec",               "Unidentified bryophyte",
  "X2026_458",                    "Unknown",
  "X2026_459",                    "Unknown",
  "Raphanus_raphanistrum",        "Brassicaceae",
  "Senecio_inaequidens",          "Asteraceae",
  "Cakile_maritima",              "Brassicaceae",
  "Artemisia_spec",               "Asteraceae",
  "Tussilago_farfara",            "Asteraceae",
  "Atriplex_glabriuscula",        "Amaranthaceae",
  "Mentha_aquatica",              "Lamiaceae",
  "Carex_arenaria",               "Cyperaceae",
  "Bolboschoenus_maritimus",      "Cyperaceae",
  "Salicornia_europaea",          "Amaranthaceae",
  "Hypochaeris_glabra",           "Asteraceae",
  "Erigeron_annuus",              "Asteraceae",
  "Tragopogon_dubius",            "Asteraceae",
  "Lagurus_ovatus",               "Poaceae",
  "Eryngium_maritimum",           "Apiaceae",
  "Honckenya_peploides",          "Caryophyllaceae",
  "Calystegia_soldanella",        "Convolvulaceae",
  "Poa_pratensis",                "Poaceae",
  "Viola_curtisii",               "Violaceae", 
  "bare",                      "Bare"
)

str(species_family)

# left joint the two dataframes by species and then group by site, physiotope and family and sum the cover values


flora_family <- flora_data %>%
  pivot_longer(cols = -c(site, physiotope), 
               names_to = "species", 
               values_to = "cover") %>%
  left_join(species_family, by = "species") %>%
  mutate(family = replace_na(family, "Unmatched")) %>%
  group_by(site, physiotope, family) %>%
  summarise(total_cover = sum(cover, na.rm = T), .groups = "drop")

# make the bar plot 
# first make bare a factor so i can put it on top of the bar plots in grey so it does not look like a plant family 
bare_name <- "Bare"

family_levels <- c(
  bare_name,
  sort(setdiff(unique(flora_family$family), bare_name))
)

flora_family <- flora_family %>%
  mutate(
    family = factor(family, levels = family_levels)
  )

# Automatisch kleuren maken voor alle families
family_colours <- setNames(
  scales::hue_pal()(length(family_levels)),
  family_levels
)

# Alleen Bare grijs maken
family_colours[bare_name] <- "grey70"


flora_family_plot <- ggplot(
  flora_family,
  aes(
    x = physiotope,
    y = total_cover,
    fill = family
  )
) +
  geom_col(
    width = 0.8,
    colour = "white",
    linewidth = 0.15
  ) +
  facet_wrap(
    ~site,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = family_colours,
    breaks = family_levels
  ) +
  labs(
    x = "Physiotope",
    y = "Total plant cover (%)",
    fill = "Family"
  ) +
  theme_bw()

flora_family_plot

# save the plot 
ggsave(
  filename = "plots/flora_family_plot.png",
  plot = flora_family_plot,
  width = 10,
  height = 5,
  dpi = 300
)

#############
# 0.5 Calculate plant diversity per plot 
############

species_data <- flora_data %>%
  dplyr::select(-site, - physiotope, -bare) %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

str(species_data)

# calculate diversity for each row 
flora_diversity <- flora_data %>%
  dplyr::select(site, physiotope) %>%
  mutate(richness = vegan::specnumber(species_data), 
         Shannon = vegan::diversity(species_data, index = "shannon"))

Shannon_diversity plot <- 
  ggplot(
  flora_diversity,
  aes(
    x = physiotope,
    y = Shannon,
    fill = physiotope
  )
) +
  geom_col(
    width = 0.75,
    colour = "black",
    linewidth = 0.2
  ) +
  facet_wrap(~ site) +
  labs(
    x = "Physiotope",
    y = "Shannon diversity",
    fill = "Physiotope"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "none"
  )

# save the plot 
ggsave(filename = "plots/shannon_plot.png",
  plot = flora_family_plot,
  width = 10,
  height = 5,
  dpi = 300
)
