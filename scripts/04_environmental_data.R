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
# 03. Grain size distribution per physiotope & location with D10, D50 and D90
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
  geom_segment((aes(x = D10, xend = D90, yend = physiotope, color = physiotope)), linewidth = 1)