
# TESTING IF LOCATION HAS AN EFFECT ON THE ABUNDANCE OF THE SPECIES 

#eerst een kolom maken die locatie bevat 
data_two_summed_final <- data_two_summed_final %>%
  dplyr::mutate( location = sub("_.*", "", pot_ID)) %>%
  relocate(location, .after = pot_ID)

# Metadata
metadata <- data_two_summed_final %>%
  dplyr::select(pot_ID, physiotope, location)

# Alleen soorten
species <- data_two_summed_final %>%
  dplyr::select(-pot_ID, -physiotope, -location)

# Bray-Curtis dissimilarity
bray <- vegdist(species, method = "bray")

# PERMANOVA
permanova <- adonis2(
  bray ~ location + physiotope,
  data = metadata,
  permutations = 999,
  by = "margin"
)

permanova

## VERSCHILLEN DE LOCATIES VAN ELKAAR 

# test voor verschil tusse loatie met correctie voor physiotope 
permanova <- adonis2(
  bray ~ location,
  data = metadata,
  permutations = 999,
  by = "margin"
)

# pairwise test 
set.seed(123)
pairwise_permanova <- adonis2(
  bray ~location,
  data = metadata,
  permutations = 999) 


# Pairwise PERMANOVA
pairwise_results <- lapply(location_pairs, function(pair) {

  # Selecteer alleen deze twee locaties
  keep <- metadata$location %in% pair

  meta_pair <- droplevels(metadata[keep, ])

  species_pair <- species[keep, ]

  # Bray-Curtis opnieuw berekenen
  bray_pair <- vegdist(
    species_pair,
    method = "bray"
  )

  # PERMANOVA, gecorrigeerd voor physiotope
  result <- adonis2(
    bray_pair ~ location + physiotope,
    data = meta_pair,
    permutations = 999,
    by = "margin"
  )

  data.frame(
    group1 = pair[1],
    group2 = pair[2],
    F = result["location", "F"],
    R2 = result["location", "R2"],
    p = result["location", "Pr(>F)"]
  )
})

# Samenvoegen
pairwise_results <- bind_rows(pairwise_results)

# Correctie voor multiple testing
pairwise_results$p_adjusted <- p.adjust(
  pairwise_results$p,
  method = "holm"
)

pairwise_results


# VERSCHILLEN DE PHYSIOTOPEN VAN ELKAAR 
set.seed(123)
permanova_phys <- adonis2(
  bray ~ physiotope,
  data = metadata,
  permutations = 999,
  strata = metadata$location
)

permanova <- adonis2(
  bray_pair ~ physiotope,
  data = meta_pair,
  permutations = 999,
  strata = meta_pair$location
)
