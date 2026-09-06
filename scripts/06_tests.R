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
  species_hel ~ physiotope,
  data = metadata,
  method = "euclidean",
  permutations = 999,
  strata = metadata$location)

permanova_phys

# welke physiotope paren verschillen significant van elkaar?
# moet met adonis twee maar dan moet je er daarna een tabel van maken 

metadata_df <- as.data.frame(metadata)

pairwise_phys_species <- pairwiseAdonis::pairwise.adonis2(
  species_hel ~ physiotope,
  data = metadata_df,
  method = "euclidean",
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

# beta dispr
# Euclidische afstand op Hellinger-getransformeerde soortendata
dist_hel <- dist(species_hel, method = "euclidean")

# Test op homogeniteit van multivariate dispersie
disp_phys <- betadisper(
  dist_hel,
  group = metadata$physiotope,
  type = "median"
)

# Permutaties beperken binnen location
perm_design <- permute::how(
  nperm = 999,
  blocks = metadata$location
)

# Permutatietest
disp_test <- permutest(
  disp_phys,
  permutations = perm_design
)

disp_test
boxplot(
  disp_phys,
  xlab = "Physiotope",
  ylab = "Distance to group median"
)

aggregate(
  disp_phys$distances,
  by = list(physiotope = metadata$physiotope),
  FUN = mean
)

## Permanova traits physiotopes ####

traits_per_pot_wide <- traits_per_pot_wide %>%
  mutate(site = sub("_.*", "", pot_ID))

trait_perm <- adonis2(
  traits_hel ~ physiotope,
  data = traits_per_pot_wide,
  method = "euclidean",
  permutations = 999,
  strata = traits_per_pot_wide$site
)

trait_perm


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
om_log <- lm(
  log(soil_om_percentage) ~ physiotope,
  data = envdata
)


om_log_lmer <- lmer(
  log(soil_om_percentage) ~ physiotope + (1 | site),
  data = envdata,
  REML = FALSE
)

AIC(om_log, om_log_lmer)

# keep lmm met log transformatie 
om_final <- lmer(
  log(soil_om_percentage) ~ physiotope + (1 | site),
  data = envdata,
  REML = TRUE
)


performance::check_singularity(om_final)
performance::check_model(om_final)

anova(om_final)

emmeans(
  om_final,
  pairwise ~ physiotope,
  adjust = "tukey"
)

library(emmeans)
library(multcomp)

emm_om <- emmeans(
  om_final,
  ~ physiotope
)

om_letters <- cld(
  emm_om,
  adjust = "tukey",
  Letters = letters
)

om_letters



# D50  #### 
D50 <- lmer(
  D50 ~ physiotope + (1 | site),
  data = envdata, 
  REML = FALSE
)

D50_lm <- lm(
  D50 ~ physiotope,
  data = envdata
)

AIC(D50, D50_lm)
# simpler model is better 

D50_final <- lm(
  D50 ~ physiotope,
  data = envdata
)

par(mfrow = c(1,1))
plot(D50_final)

# looks fine 

anova(D50_final)
# no sign effect of physiotope



# grain sorting  #### 
grain_sorting <- lmer(
  grain_sorting ~ physiotope + (1|site),
  data = envdata, 
  REML = FALSE
)

grain_sorting_lm <- lm(
  grain_sorting ~ physiotope,
  data = envdata
)

AIC(grain_sorting, grain_sorting_lm)


performance::check_singularity(grain_sorting)

grain_sorting_final <- lmer(
  grain_sorting ~ physiotope + (1|site),
  data = envdata, 
  REML = TRUE
)

performance::check_model(grain_sorting_final)
# looks good 

anova(grain_sorting_final)
# no physiotope effect 


# plant richness  #### 
# is a count variable, so glm with poisson family 
plantrichness <- glm(
  richness ~ physiotope,
  data = envdata,
  family = poisson(link = "log")
)

# with random effect
plantrichness_glmer <- glmer(
  richness ~ physiotope + (1 | site),
  data = envdata,
  family = poisson(link = "log")
)

AIC(plantrichness, plantrichness_glmer)
# keep the model with site as random effect

performance::check_singularity(plantrichness_glmer)
VarCorr(plantrichness_glmer)

# check overdispersion 
performance::check_overdispersion(plantrichness_glmer)

car::Anova(
  plantrichness_glmer,
  type = 3
)

# post hoc 
richness_emm <- emmeans(
  plantrichness_glmer,
  ~ physiotope,
  type = "response"
)

pairs(
  richness_emm,
  adjust = "tukey"
)

# letters 
richness_letters <- cld(
  richness_emm,
  adjust = "tukey",
  Letters = letters
)

richness_letters
