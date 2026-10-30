################
# Description of the environmental data 
###############
# load in the data 
###############

envdata <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQU4-VqDxuaSTUqruH83CrjeW6sTD95GdQlZnfCFKQLsLkcKOdmxxXD4G7mSRLf2AJeC3agHe8p_cOo/pub?gid=1256365017&single=true&output=csv")

# an offline version of this data was included in the data folder of the zip file project under the name Environmental_data
# can be imported with: 
# envdata <- read_csv("~/Msc Ecology & Conservation/Master project 2/Thesis_Zita_Voskuilen_2026/data/Environmental_data.csv")

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

# an offline version of this data was included in the data folder of the zip file project under the name Flora_data
# can be imported with: 
# flora_data <- read_csv("~/Msc Ecology & Conservation/Master project 2/Thesis_Zita_Voskuilen_2026/data/Flora_data.csv")

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




#############
# 05. Calculate plant diversity and shannon index per plot 
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

# 06. Elevation adding to the envdata dataframe ####

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
