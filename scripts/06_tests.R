###

## Permanova abundance location ####
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


## VERSCHILT DE ABUNDANTIE TUSSEN DE LOCATIES 

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

library(pairwiseAdonis)

## Permanova abundance physiotopes  ####
set.seed(123)
permanova_phys <- adonis2(
  bray ~ physiotope,
  data = metadata,
  permutations = 999,
  strata = metadata$location)

permanova_phys

# welke physiotope paren verschillen significant van elkaar?
# moet met adonis twee maar dan moet je er daarna een tabel van maken 

metadata_df <- as.data.frame(metadata)

pairwise_phys_species <- pairwiseAdonis::pairwise.adonis2(
  bray ~ physiotope,
  data = metadata_df,
  strata = "location",
  nperm = 999
)

pairwise_phys_species

# tabel van de resultaten maken 
pw <- pairwise_phys_species[-1]

results <- data.frame(
  comparison = names(pw),
  R2 = sapply(pw, function(x) x$R2[1]),
  F = sapply(pw, function(x) x$F[1]),
  p = sapply(pw, function(x) x$`Pr(>F)`[1])
)

results$p_holm <- p.adjust(results$p, method = "holm")

results


## Permanova traits physiotopes ####

meta_traits <- traits_per_pot_wide %>%
  dplyr::select(pot_ID, physiotope) %>%
  mutate(
    location = sub("_.*", "", pot_ID)
  )

# scaled because some traits 
trait_matrix_scaled <- scale(trait_matrix)

permanova_traits <- adonis2(
  trait_matrix_scaled ~ location + physiotope,
  data = meta_traits,
  method = "euclidean",
  permutations = 999, 
  by = "margin"
)

permanova_traits

# which physiotopes differ from each other?
pairwise_physio <- pairwiseAdonis::pairwise.adonis(
  trait_matrix_scaled,
  meta_traits$physiotope,
  sim.method = "euclidean",
  p.adjust.m = "holm",
  perm = 999
)

pairwise_physio

trait_dist <- dist(trait_matrix_scaled, method = "euclidean")

disp_physio <- betadisper(
  trait_dist,
  meta_traits$physiotope
)

permutest(disp_physio, permutations = 999)


## Tests environmental variables physiotopes ####

# didnt use this??
env_scaled <- scale(env_pca_data)
env_dist <- dist(env_pca_data, method = "euclidean")

permanova_env <- adonis2(
  env_dist ~ physiotope + site,
  data = envdata,
  permutations = 999,
  by = "margin"
)

permanova_env


# moisture #### 

moisture <- lmer(
  soil_moisture_percentage ~ physiotope + (1 | site),
  data = envdata
)

# model assumptions
performance::check_model(moisture)

# singularity
check_singularity(moisture)

# test effect physiotope
anova(moisture)

# Model without site
moisture_lm <- lm(
  soil_moisture_percentage ~ physiotope,
  data = envdata
)

# Model with site as random effect
moisture_lmer <- lmer(
  soil_moisture_percentage ~ physiotope + (1 | site),
  data = envdata,
  REML = FALSE
)

AIC(moisture_lm, moisture_lmer, moisture_log)

# use model without random effect 
# check assumptions
par(mfrow = c(2, 2))
plot(moisture_log)

anova(moisture_lm)

emmeans(
  moisture_lm,
  pairwise ~ physiotope,
  adjust = "tukey"
)

# log transformed
moisture_log <- lm(
  log(soil_moisture_percentage) ~ physiotope,
  data = envdata
)


moisture_log_lmer <- lmer(
  log(soil_moisture_percentage) ~ physiotope + (1 | site),
  data = envdata,
  REML = FALSE
)

AIC(moisture_log, moisture_log_lmer)

performance::check_singularity(moisture_log)

anova(moisture_log)

emmeans(
  moisture_log,
  pairwise ~ physiotope,
  adjust = "tukey"
)

library(emmeans)
library(multcomp)

emm_moisture <- emmeans(
  moisture_log,
  ~ physiotope
)

moisture_letters <- cld(
  emm_moisture,
  adjust = "tukey",
  Letters = letters
)

moisture_letters

# organic matter ####

organic_matter <- lmer(
  soil_om_percentage ~ physiotope + (1 | site),
  data = envdata,
  REML = FALSE
)
# model assumptions
performance::check_model(organic_matter)

# singularity
check_singularity(organic_matter)

VarCorr(organic_matter)


organic_matter_simple <- lm(
  soil_om_percentage ~ physiotope,
  data = envdata
)

AIC(organic_matter_simple, organic_matter)

# keep lmm 

# use model with random effect 
# check assumptions
performance::check_model(
  organic_matter)

#try log transformed 

# log transformed
moisture_log <- lm(
  log(soil_moisture_percentage) ~ physiotope,
  data = envdata
)


moisture_log_lmer <- lmer(
  log(soil_moisture_percentage) ~ physiotope + (1 | site),
  data = envdata,
  REML = FALSE
)

AIC(moisture_log, moisture_log_lmer)

performance::check_singularity(moisture_log)

anova(moisture_log)

emmeans(
  moisture_log,
  pairwise ~ physiotope,
  adjust = "tukey"
)

library(emmeans)
library(multcomp)

emm_moisture <- emmeans(
  moisture_log,
  ~ physiotope
)

moisture_letters <- cld(
  emm_moisture,
  adjust = "tukey",
  Letters = letters
)

moisture_letters



# D50  #### 
D50 <- lm(
  D50 ~ physiotope + site,
  data = envdata
)

Anova(D50, type = 2)
emmeans(D50, pairwise ~ physiotope, adjust = "holm")

# grain sorting  #### 
grain_sorting <- lm(
  grain_sorting ~ physiotope + site,
  data = envdata
)

Anova(grain_sorting, type = 2)
emmeans(grain_sorting, pairwise ~site, adjust = "holm")

# plant richness  #### 
plantrichness <- lm(
  richness ~ physiotope + site,
  data = envdata
)

Anova(plantrichness, type = 2)
emmeans(plantrichness, pairwise ~physiotope, adjust = "holm")
emmeans(plantrichness, pairwise ~ site, adjust = "holm")

plantrichness <- emmeans(
  plantrichness,
  ~ physiotope
)


letters_plantrichness<- emmeans:::cld.emmGrid(
  plantrichness,
  adjust = "holm",
  Letters = letters,
  alpha = 0.05
)


letters_plantrichness
