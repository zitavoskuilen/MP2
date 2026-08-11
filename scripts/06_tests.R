
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


## VERSCHILLEN DE LOCATIES VAN ELKAAR 

# test voor verschil tussen loaties 
permanova <- adonis2(
  bray ~ location,
  data = metadata,
  permutations = 999,
  by = "margin"
)

pairwiseAdonis::pairwise.adonis(
  bray,
  metadata$location,
  p.adjust.m = "holm"
)


# VERSCHILLEN DE PHYSIOTOPEN VAN ELKAAR 
set.seed(123)
permanova_phys <- adonis2(
  bray ~ physiotope,
  data = metadata,
  permutations = 999,
  strata = metadata$location)



 
