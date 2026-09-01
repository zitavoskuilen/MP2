###
# Making a stacked bar plot for the food web place data
###

# read in the feeding mode data 
feedingmode <- read_delim("feedingmode.csv", 
     delim = ";", escape_double = FALSE, trim_ws = TRUE)

str(feedingmode)
ncol(feedingmode)

# changing the names in the feedingmode data frame to match the species names in the traits data frame
feedingmode <- feedingmode %>%
  mutate(
    species_name = case_when(
      !is.na(Species) & str_squish(Species) != "" ~ 
        str_replace_all(str_squish(Species), "\\s+", "_"),
      
      TRUE ~ paste0(
        coalesce(
          na_if(str_squish(Genus), ""),
          na_if(str_squish(Subfamily), ""),
          na_if(str_squish(Family), ""),
          na_if(str_squish(Order), ""),
          na_if(str_squish(Class), ""),
          na_if(str_squish(Phylum), "")
        ),
        "_sp"
      )
    )
  )

# apply structure 
feedingmode <- feedingmode %>%
  dplyr::select(
    species_name,
    microbivore,
    detrivore,
    herbivore,
    granivore,
    predator
  )

# only getting the feedingmode data for the species i analyze the traits from 

data_two_species_traits

abundance_long <- data_two_species_traits %>%
  pivot_longer(
    cols = -c(pot_ID, location, physiotope),
    names_to = "species_name",
    values_to = "abundance"
  )


species_keep <- names(data_two_species_traits)[
  !names(data_two_species_traits) %in% c("pot_ID", "location", "physiotope")
]

feedingmode_filtered <- feedingmode %>%
  filter(species_name %in% species_keep)

nrow(feedingmode_filtered)

setdiff(
  species_keep,
  feedingmode_filtered$species_name
)


feedingmode_long <- feedingmode_filtered %>%
  pivot_longer(
    cols = c(
      microbivore,
      detrivore,
      herbivore,
      granivore,
      predator
    ),
    names_to = "feeding_mode",
    values_to = "score"
  ) %>%
  mutate(
    score = as.numeric(score)
  )

feedingmode

feedingmode_long <- feedingmode_long %>%
  group_by(species_name) %>%
  mutate(
    feeding_prop = score / sum(score, na.rm = TRUE)
  ) %>%
  ungroup()


feeding_abundance <- abundance_long %>%
  left_join(
    feedingmode_long,
    by = "species_name"
  ) %>%
  mutate(
    feeding_abundance = abundance * feeding_prop
  )

feeding_abundance %>%
  filter(is.na(feeding_mode)) %>%
  distinct(species_name)


feeding_phys <- feeding_abundance %>%
  group_by(physiotope, feeding_mode) %>%
  summarise(
    abundance = sum(feeding_abundance, na.rm = TRUE),
    .groups = "drop"
  )

feeding_cols <- c(
  "microbivore" = "#756BB1",  # paars
  "detrivore"   = "#B87333",  # koper/bruin
  "herbivore"   = "#7A9E66",  # zacht groen
  "granivore"   = "#D4A84F",  # oker
  "predator"    = "#A65365"   # donker oudroze/rood
)

#plot

feeding_phys <- feeding_phys %>%
  group_by(physiotope) %>%
  mutate(
    proportion = abundance / sum(abundance),
    percentage = proportion * 100
  ) %>%
  ungroup()

barplot <- ggplot(
  feeding_phys,
  aes(
    x = physiotope,
    y = proportion,
    fill = feeding_mode
  )
) +
  geom_col() +
  
  geom_text(
    aes(
      label = ifelse(
        percentage >= 5,
        paste0(round(percentage, 1), "%"),
        ""
      )
    ),
    position = position_stack(vjust = 0.5),
    size = 3.5
  ) +
  
  scale_fill_manual(
    values = feeding_cols,
    labels = c(
      "microbivore" = "Microbivore",
      "detrivore"   = "Detritivore",
      "herbivore"   = "Herbivore",
      "granivore"   = "Granivore",
      "predator"    = "Predator"
    )
  ) +
  
  scale_y_continuous(
    labels = scales::percent
  ) +
  
  labs(
    x = "Physiotope",
    y = "Relative abundance",
    fill = "Feeding mode", 
    title = "Feeding mode composition across physiotopes"
  ) +
  
  theme_classic()

barplot

# save the plot
ggsave(
  filename = "feeding_mode_barplot.png",
  plot = barplot,
  width = 8,
  height = 6,
  dpi = 300
)