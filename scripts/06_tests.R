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


## Testing richness and shannon between ecotopes ####

library(dplyr)
library(vegan)
library(ggplot2)

diversity_df <- data_two_summed_final %>%
  mutate(
    richness = specnumber(as.matrix(select(., -(1:3)))),
    shannon = diversity(as.matrix(select(., -(1:3))),
                        index = "shannon")
  ) %>%
  select(pot_ID, location, physiotope, richness, shannon)

diversity_df

diversity_summary <- diversity_df %>%
  group_by(physiotope) %>%
  summarise(
    n = n(),
    mean_richness = mean(richness),
    sd_richness = sd(richness),
    mean_shannon = mean(shannon),
    sd_shannon = sd(shannon),
    .groups = "drop"
  )

diversity_summary


model_richness <- glm(
  richness ~ location + physiotope,
  family = poisson,
  data = diversity_df
)

# Test het effect van physiotope,
# nadat rekening is gehouden met location
drop1(model_richness, test = "Chisq")

# Controleer overdispersie
sum(residuals(model_richness, type = "pearson")^2) /
  df.residual(model_richness)

model_shannon <- lm(
  shannon ~ location + physiotope,
  data = diversity_df
)

drop1(model_shannon, test = "F")

# Controleer de modelaannames
par(mfrow = c(2, 2))
plot(model_shannon)
par(mfrow = c(1, 1))



## Testing abundance between ecotopes ####
abundance_df <- data_two_summed_final %>%
  mutate(
    abundance = rowSums(
      as.matrix(select(., -pot_ID, -location, -physiotope)),
      na.rm = TRUE
    )
  ) %>%
  select(pot_ID, location, physiotope, abundance)

abundance_summary <- abundance_df %>%
  group_by(physiotope) %>%
  summarise(
    n = n(),
    total_abundance = sum(abundance),
    mean_abundance = mean(abundance),
    sd_abundance = sd(abundance),
    .groups = "drop"
  ) %>%
  arrange(desc(total_abundance))

abundance_summary
table(abundance_df$location, abundance_df$physiotope)


model_abundance_nb <- glm.nb(
  abundance ~ location + physiotope,
  data = abundance_df
)

drop1(model_abundance_nb, test = "Chisq")

emmeans(
  model_abundance_nb,
  pairwise ~ physiotope,
  adjust = "holm",
  type = "response"
)
top_species_LD <- data_two_summed_final %>%
  filter(physiotope == "LD") %>%
  dplyr::select(-pot_ID, -location, -physiotope) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "species",
    values_to = "abundance"
  ) %>%
  arrange(desc(abundance)) %>%
  slice_head(n = 15)

top_species_LD

# Bepaal de 10 talrijkste soorten in LD
top10 <- top_species_LD %>%
  slice_head(n = 10) %>%
  pull(species)

# Vergelijk hun abundantie tussen fysiotopen
species_comparison <- data_two_summed_final %>%
  pivot_longer(
    cols = -c(pot_ID, location, physiotope),
    names_to = "species",
    values_to = "abundance"
  ) %>%
  filter(species %in% top10) %>%
  group_by(physiotope, species) %>%
  summarise(
    total_abundance = sum(abundance),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = physiotope,
    values_from = total_abundance,
    values_fill = 0
  )

# how many duptera in DS
data_two_summed_final %>%
  filter(physiotope == "DS") %>%
  summarise(
    total_diptera = sum(Diptera_sp, na.rm = TRUE),
    total_abundance = 341,
    percentage_diptera = round(total_diptera / total_abundance * 100, 1)
  )

# in any ecotope 
diptera_summary <- data_two_summed_final %>%
  mutate(
    total_abundance = rowSums(
      across(-c(pot_ID, location, physiotope)),
      na.rm = TRUE
    )
  ) %>%
  group_by(physiotope) %>%
  summarise(
    total_abundance = sum(total_abundance),
    total_diptera = sum(Diptera_sp, na.rm = TRUE),
    percentage_diptera = round(
      total_diptera / total_abundance * 100, 1
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(percentage_diptera))

diptera_summary

# abundance withoout diptera 
abundance_without_diptera <- data_two_summed_final %>%
  mutate(
    abundance = rowSums(
      across(-c(pot_ID, location, physiotope, Diptera_sp)),
      na.rm = TRUE
    )
  ) %>%
  dplyr::select(pot_ID, location, physiotope, abundance)

library(MASS)

model_no_diptera <- MASS::glm.nb(
  abundance ~ location + physiotope,
  data = abundance_without_diptera
)

drop1(model_no_diptera, test = "Chisq")


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

# see which phystiopes are most similiar based on the centroid distances

trait_centroids <- aggregate(
  traits_hel,
  by = list(physiotope = traits_per_pot_wide$physiotope),
  FUN = mean
)

# keep physiotopes names 
physio_names <- trait_centroids$physiotope

# only trait columns 
centroid_traits <- trait_centroids[, -1]

# distances 
centroid_dist <- as.matrix(dist(centroid_traits, method = "euclidean"))

rownames(centroid_dist) <- physio_names
colnames(centroid_dist) <- physio_names

round(centroid_dist, 3)

# make a dataframe of the distances between physiotopes
dist_df <- as.data.frame(as.table(centroid_dist))

dist_df <- dist_df %>%
  filter(Var1 != Var2) %>%
  mutate(
    pair = ifelse(
      as.character(Var1) < as.character(Var2),
      paste(Var1, Var2, sep = " - "),
      paste(Var2, Var1, sep = " - ")
    )
  ) %>%
  distinct(pair, .keep_all = TRUE) %>%
  arrange(Freq)

dist_df


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

# correlations between variables 




# moisture #### 

moisture <- lmer(
  moisture_LRR ~ physiotope + (1 | site),
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
  moisture_LRR ~ physiotope,
  data = envdata
)

# Model with site as random effect
moisture_lmer <- lmer(
  moisture_LRR ~ physiotope + (1 | site),
  data = envdata,
  REML = FALSE
)

AIC(moisture_lm, moisture_lmer)

# use model with random effect 
# check assumptions
par(mfrow = c(2, 2))
plot(moisture_lmer)

anova(moisture_lmer)

emmeans(
  moisture_lmer,
  pairwise ~ physiotope,
  adjust = "tukey"
)


library(emmeans)
library(multcomp)

emm_moisture <- emmeans(
  moisture_lmer,
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
  PlantRichness ~ physiotope,
  data = envdata,
  family = poisson(link = "log")
)

# with random effect
plantrichness_glmer <- glmer(
  PlantRichness ~ physiotope + (1 | site),
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

# cover ####

envdata$cover_prop <- envdata$cover / 100

cover_mod_ord <- glmmTMB(
  cover_prop ~ physiotope + (1|site),
  data = envdata,
  family = ordbeta(link = "logit")
)


summary(cover_mod_ord)
car::Anova(cover_mod_ord, type = 3)

emmeans(
  cover_mod_ord,
  pairwise ~ physiotope,
  type = "response",
  adjust = "tukey"
)

library(DHARMa)

res_cover <- simulateResiduals(cover_mod_ord)
plot(res_cover)


# Estimated marginal means
emm_cover <- emmeans(
  cover_mod_ord,
  ~ physiotope,
  type = "response"
)

# Tukey pairwise comparisons
letters_cover <- cld(
  emm_cover,
  adjust = "tukey",
  Letters = letters
) %>%
  mutate(.group = trimws(.group))

letters_cover


# elevation ####

# no random effect 
elevation_lm <- lm(
  elevation ~ physiotope,
  data = envdata
)

# with random effect 
elevation_lmer <- lmer(
  elevation ~ physiotope + (1 | site),
  data = envdata,
  REML = FALSE
)


AIC(elevation_lm, elevation_lmer)
# lmer better 

# check whether random effect is singular
performance::check_singularity(elevation_lmer)

# variance explained by site
VarCorr(elevation_lmer)

# model assumptions
performance::check_model(elevation_lmer)

# looks good 

# final model 
elevation_lmer <- lmer(
  elevation ~ physiotope + (1 | site),
  data = envdata,
  REML = TRUE
)

# test 
car::Anova(
  elevation_lmer,
  type = 3
)

elevation_emm <- emmeans(
  elevation_lmer,
  ~ physiotope
)

pairs(
  elevation_emm,
  adjust = "tukey"
)


elevation_letters <- cld(
  elevation_emm,
  adjust = "tukey",
  Letters = letters
)

elevation_letters



