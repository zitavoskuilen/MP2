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



###############
# 04. FLora per site individually 
##############

# load flora data 

flora_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQU4-VqDxuaSTUqruH83CrjeW6sTD95GdQlZnfCFKQLsLkcKOdmxxXD4G7mSRLf2AJeC3agHe8p_cOo/pub?gid=144268895&single=true&output=csv")

str(flora_data)

# select only the columns that include site and physiotope and all the species 
flora_data <- flora_data %>%
  dplyr::select(2, 3, 10:dplyr::last_col())

str(flora_data)

# change all x's to 0.01
flora_data <- flora_data %>%
  mutate(
    across(2:last_col(), ~ replace(.x, .x == "x", 0.01))
  )

# change all NA's to zero's 
flora_data[is.na(flora_data)] <- 0

# make all columns numeric except for site and physiotope
flora_data <- flora_data %>%
  mutate(across(-c(site, physiotope), as.numeric))

str(flora_data)  

# only the species 
flora_species <- flora_data[, 3:ncol(flora_data)] %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))

# Add richness and shannon diversity to flora_data
flora_data <- flora_data %>%
  mutate(
    PlantRichness = rowSums(flora_species > 0),
    PlantsShannon = diversity(flora_species, index = "shannon"))

# add the shannon, richness and eveness to the environmental data 
envdata <- envdata %>%
  left_join(
    flora_data %>%
      dplyr::select(
        site,
        physiotope,
        cover,
        PlantRichness,
        PlantsShannon
      ),
    by = c("site", "physiotope")
  )

str(envdata)
str(flora_data)


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
  "Bryophyta_spec2",              "Bryophyte",
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
  "Taraxacum_officinale",         "Asteraceae",
  "Holcus_lanatus",               "Poaceae",
  "Aira_praecox",                 "Poaceae",
  "Jasione_montana",              "Campanulaceae",
  "Phleum_arenarium",             "Poaceae",
  "Sedum_acre",                   "Crassulaceae",
  "Bryophyta_spec",               "Bryophyte",
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
# 0.5 Calculate plant diversity and shannon index per plot 
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

flora_diversity_table <- as.data.frame(flora_diversity)
view(flora_diversity_table)

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

# 6.0 Elevation adding to the envdata dataframe

# load the data 

elevationTS <-read.csv("~/Msc Ecology & Conservation/Master project 2/JOB_202604-Terschelling-Janne.csv")

elevationNLCOAST <- read.csv("~/Msc Ecology & Conservation/Master project 2/JOB_202605.NLCOAST.JANNE.csv")

# select only the data point taht we need and make one data frame of it 

# terschelling only select where ObjName has KH, SDL, or KWA in it 
elv_ts<- elevationTS %>%
  filter(
    str_detect(ObjName, "^(KH|SDL|KWA)"),
    !str_detect(ObjName, "GRID")
  )

elevation <- elv_ts %>%
  mutate(
    physiotope_plot = str_remove(ObjName, "[-_]\\d+$"),
    physiotope_plot = str_replace_all(physiotope_plot, "-", "_")
  ) %>%
  group_by(physiotope_plot) %>%
  summarise(
    elevation = mean(Ht, na.rm = TRUE),
    .groups = "drop"
  )

elevation

# do the same for the other data set 
elv_nlcoast <- elevationNLCOAST %>%
  filter(
    str_detect(ObjName, "^(IJM|HBD)"))

elv_nlcoast_clean <- elv_nlcoast %>%
  filter(ObjName != "IJM_B") %>%
  mutate(
    ObjName = str_replace(
      ObjName,
      "^IJM_(LD|HD|FD|DS|B)([123])$",
      "IJM_\\1_\\2"
    )
  )

elevation_mean_nlcoast <- elv_nlcoast_clean %>%
  mutate(
    location = str_extract(ObjName, "^[^_]+"),
    physiotope = str_extract(ObjName, "(?<=_)[^_]+(?=_\\d+$)")
  ) %>%
  group_by(location, physiotope) %>%
  summarise(
    Ht = mean(Ht, na.rm = TRUE),
    .groups = "drop"
  )

elevation_mean_nlcoast

# make the same as the other 
elevation_nl <- elevation_mean_nlcoast %>%
  transmute(
    site = location,
    physiotope = physiotope,
    elevation = Ht
  )

# split into physiotope 

elevation <- elevation %>%
  separate(
    physiotope_plot,
    into = c("site", "physiotope"),
    sep = "_"
  )


# bind 
elevation_all <- bind_rows(elevation, elevation_nl)

# change WS to DS to match the other dataset 
elevation_all <- elevation_all %>%
  mutate(
    physiotope = ifelse(physiotope == "WS", "DS", physiotope)
  )


# add it to the envdata set
envdata <- envdata %>%
  dplyr::select(-elevation) %>%
  left_join(
    elevation_all,
    by = c("site", "physiotope")
  )
