
## PCA WITH TRAITS ORDINATION ####

## CLEANING THE TRAITS DATA ####

# read in the traits data 
traits <- read_delim("traits_5.csv", delim = ";", 
     escape_double = FALSE, trim_ws = TRUE)

ncol(traits)
view(traits)

# first i will fix the trait names in the trait data set 

# eerste twee rijen bevatten hoofdtrait + subtrait
group <- as.character(unlist(traits[1, ]))
subtrait <- as.character(unlist(traits[2, ]))

group[group == ""] <- NA
group <- fill(data.frame(group), group, .direction = "down")$group


abbr <- c(
  "Adult living habitat" = "ALH",
  "Adult living depth (soil or water, cm)" = "ALD",
  "Adult body size (mm)" = "ABS",
  "Seasonal activty pattern" = "SAP",
  "Daily activity pattern" = "DAP",
  "Feeding mode" = "FM",
  "Adult locomotion" = "AL",
  "Reproductive mode" = "RM",
  "Larval/juvenile development location" = "LDL")

clean <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

new_names <- ifelse(
  clean(subtrait) == "parental_care",
  "parental_care",
  ifelse(
    group %in% names(abbr),
    paste0(abbr[group], "_", clean(subtrait)),
    clean(subtrait)
  )
)

new_names[1:9] <- c(
  "Taxon_ID", "Phylum", "Class", "Order", "Family",
  "Subfamily", "Genus", "Species", "Reference_ID"
)


# take off the column that we do not need 

names(traits) <- new_names
names(traits)
traits <- traits[, !is.na(names(traits)) & names(traits) != ""]
traits <- traits[-c(1, 2), ]

str(traits)


# Delete the columns we will not need 
# delete all RID (reference id) columns
# and the foodwebplace score becasue we will make a seperate figure on that 
traits <- traits %>%
  dplyr::select(-matches("rid")) %>%
  dplyr::select(-c("foodweb_place"))


# only select the column with the traits that i want and the order, family, subfamily, genus ans species columns
traits <- traits %>%
  dplyr::select(
    Order,
    Family,
    Subfamily,
    Genus, 
    Species,
    matches("^[A-Z]+_")
  )

str(traits)


# make a new column with species where if the species column is already filled, it stays the same name but with a _ in the middle and if it not filled, take the first column on the left of species that is filled and put it in the new column with _sp behind it. and delte allt he other columns that are not species or the traits columns. so also the reference Id and the taxon_id

traits <- traits %>%
  mutate(
    Species = case_when(
      !is.na(Species) & Species != "" ~ str_replace_all(Species, "\\s+", "_"),
      
      TRUE ~ paste0(
        coalesce(
          na_if(Genus, ""),
          na_if(Subfamily, ""),
          na_if(Family, ""),
          na_if(Order, "")
        ),
        "_sp"
      )
    )
  )

str(traits)


## SELECTING THE SPECIES FROM THE OCCURRENCE DATA THAT ARE ALSO IN THE TRAITS DATA ####

# i have to make sure that the species names in the traits data can match the ones in the data_two_summed_final data frame. 
colnames(data_two_summed_final)
traits$Species

# checking species ooccurence for the traits table 

species_data_occ <- data_two_summed_final[, 5:ncol(data_two_summed_final)]

# Aantal plekken waarop elke soort voorkomt
occurence <- colSums(species_data_occ > 0, na.rm = TRUE)

# totale abundatie per soort 
total_abundance <- colSums(species_data_occ, na.rm = TRUE)

# singletons = op 1 plke en minder dan 3 individuen 
singletons <- names(occurence[occurence == 1  & total_abundance < 3])

singletons

# doubletons = op twee plekken en minder dan 3 individuen
doubletons <- names(occurence[occurence == 2 & total_abundance < 3])


## removing the species with too little occurence from the abundance dataset for later 
# species to remove from the data_two_summed_final data frame for the traits analysis 
remove_species <- c(singletons, doubletons)

# i want to remove the names from the species tn the remove species from the data_two_summed_final 

length(remove_species)

# met ook diptera, lepidoptera larve en hymeoptera want dat zijn gevleugelde soorten 
extra_remove <- c(
  "Diptera_sp",
  "Hymenoptera_sp",
  "Coleoptera_larvae", 
  "Lepidoptera_larvae"     
)


# haal deze soorten uit de data_two_summed_final data frame 
data_two_species_traits <- data_two_summed_final %>%
  dplyr::select(-any_of(c(remove_species, extra_remove)))


# i now have the dataset of the species abundance that i want to use for the traits analysis 
data_two_species_traits 
colnames(data_two_species_traits)

# now i need to only take the rows from the traits data that contain the species that are in my data_two_species_traits data frame. so i will make a vector of the species names in the data_two_species_traits data frame and then filter the traits data frame for those species.

colnames(data_two_species_traits)
species_in_traits_data <- colnames(data_two_species_traits)[3:ncol(data_two_species_traits)]


sort(traits$Species)

species_in_traits_data %in% traits$Species


species_check <- data.frame(
  Species = species_in_traits_data,
  in_traits = species_in_traits_data %in% traits$Species
)

species_check

# i have to change the names in data_two_species_traits om ze te machten met de namen in de traits dataset

names(data_two_species_traits) <- dplyr::recode(
  names(data_two_species_traits),
  "Collembola_sp" = "Entomobryomorpha_sp",
  "Philopedon_plagiatus" = "Philopedon_plagiatum",
  "Prostigmata_sp" = "Trombidiidae_sp"
)

## FOR NOW ONLY SELECT THE SPECIES THAT I HAVE FILLED IN THE TRAITS DATA 


traits_filtered <- traits %>%
  dplyr::filter(Species %in% species_in_traits_data) %>%
  dplyr::arrange(match(Species, species_in_traits_data)) 

# take out the first 7 columns, and the 9th column with Reference_ID
colnames(traits_filtered)

traits_filtered <- traits_filtered %>%
  dplyr::select(-c(Taxon_ID, Order, Family, Subfamily, Genus, Reference_ID)) 

traits_filtered

# make a long format of the traits data frame 

traits_long <- traits_filtered %>%
  pivot_longer(
    cols = -Species,
    names_to = "trait",
    values_to = "value"
  ) %>%
  mutate(
    value = parse_number(value, locale = locale(decimal_mark = ","))
  )

# nu heb ik een long format van de traits data, 
# nu moet ik nog een long format van mn abundantie species data 

## LONG FORMAT OF THE ABUNDANCE DATA & JOINING THE DATA SETS ####
data_two_species_traits

data_two_long <- data_two_species_traits %>%
  pivot_longer(
    cols = -c(pot_ID, physiotope),
    names_to = "Species",
    values_to = "abundance"
  ) %>% 
  dplyr::select(-c(physiotope))


## LEFT JOINING THE TWO LONG FORMAT TABLES ## 

traits_abundance <- left_join(
  data_two_long,
  traits_long,
  by = "Species"
)

# makeing a new column with the "weighted traits code' which is the abundance * the trait value 

traits_abundance <- traits_abundance %>%
  mutate(weighted_trait = abundance * value)


# view(traits_abundance)


# but we want a community weighted mean per pitfaal because otherwise you get a higher score for a trait if the abundance of individuals is higher in a pitfall

traits_per_pot_long <- traits_abundance %>%
  group_by(pot_ID, trait) %>%
  summarise(
    weighted_sum = sum(abundance * value, na.rm = TRUE),
    
    abundance_known = sum(
      abundance[!is.na(value)]
    ),
    
    CWM = ifelse(
      abundance_known > 0,
      weighted_sum / abundance_known,
      NA_real_
    ),
    
    .groups = "drop"
  )

# now make the format wide again to make a pca 

traits_per_pot_wide <- traits_per_pot_long %>%
  dplyr::select(pot_ID, trait, CWM) %>%
  pivot_wider(
    names_from = trait,
    values_from = CWM
  ) %>%
  left_join(
    data_two_species_traits %>%
      dplyr::select(pot_ID, physiotope) %>%
      distinct(),
    by = "pot_ID"
  )

view(traits_per_pot_wide)

## PCA TRAITS ####

# select only the trait values and delete the column with only NA's 
trait_matrix <- traits_per_pot_wide %>%
  dplyr::select( -c(pot_ID  , physiotope)) %>%
  dplyr::select(where(~ !all(is.na(.))))

dim(trait_matrix)
str(trait_matrix)


# testing which ordination method i should use 

trait_matrix_test <- trait_matrix %>%
  dplyr::select(where(~ sum(.x, na.rm = TRUE) > 0)) %>%   # remove empty species
  filter(rowSums(.) > 0)                          # ✅ remove empty sites

dca_res_2 <- decorana(trait_matrix_test)
print(dca_res_2) 

# Extract gradient length (first axis)

axis_lengths <- apply(
  dca_res_2$rproj,
  2,
  function(x) diff(range(x, na.rm = TRUE))
)

axis_lengths

gradient_length <- axis_lengths[1]


###############################################
# INTERPRETATION RULE
###############################################
if (gradient_length < 3) {
  cat("→ Linear methods recommended: PCA or RDA\n")
} else if (gradient_length >= 3 & gradient_length <= 4) {
  cat("→ Mixed methods: PCA or NMDS (compare both)\n")
} else {
  cat("→ Unimodal methods recommended: CA, CCA or NMDS\n")
}

# recommended is the PCA but we need to do a hellinger transformation 


# PCA

traits_hel <- decostand(trait_matrix, method = "hellinger")

trait_pca <- rda(
  traits_hel
)

summary(trait_pca)

# plotting 
# colors of the plot
phys_cols <- c(
  "B"   = "#E8D7B0",  # bare
  "B2"  = "#9B6F3E",  # bare2
  "DS"  = "#4FA3A5",  # duneslack
  "FD"  = "#F2C94C",  # foredune
  "FD2" = "#D97706",  # foredune2
  "LD"  = "#8FBF68",  # lowdensity
  "HD"  = "#356B3A"   # highdensity
)

eig <- eigenvals(trait_pca)

eig_percent <- eig / sum(eig) * 100

eig_percent[1:2]


site_scores <- scores(
  trait_pca,
  display = "sites",
  scaling = "symmetric"
)

group <- factor(traits_per_pot_wide$physiotope)

png(
  "PCA_traits_AL.png",
  width = 9,
  height = 6,
  units = "in",
  res = 600
)

# make extra space for legend
par(mar = c(5, 4, 4, 12),xpd = FALSE)

ordiplot(
  trait_pca,
  type = "n",
  scaling = "symmetric",
  main = "PCA -Adult Locomotion Traits",
  xlab = paste0("PC1 (", round(eig_percent[1], 1), "%)"),
  ylab = paste0("PC2 (", round(eig_percent[2], 1), "%)"),
  xlim = c(-0.6, 0.8),
  ylim = c(-0.4, 0.4)
)

# ellips 
ordiellipse(
  trait_pca,
  groups = group,
  display = "sites",
  scaling = "symmetric",
  kind = "sd",
  draw = "polygon",,
  col = adjustcolor(
    phys_cols[seq_along(levels(group_phys))],
    alpha.f = 0.05
  ),
  border = adjustcolor(
    phys_cols[seq_along(levels(group))],
    alpha.f = 0.6
  ),
  lwd = 1.3
)


# points 
points(
  site_scores,
  col = phys_cols[as.character(group)],
  pch = 19,
  cex = 1
)

# legend 
legend(
  "right",
  inset = c(-0.32, 0),
  xpd = NA,
  legend = c(
    "Bare",
    "Bare 2",
    "Low-density dunes",
    "High-density dunes",
    "Green Beach",
    "Foredune",
    "Foredune 2"
    
  ),
  col = phys_cols[c("B", "B2", "LD", "HD", "DS", "FD", "FD2")],
  pch = 19,
  bty = "n",
  title = "Physiotopes"
)



# Trait-scores uit PCA halen
trait_scores <- scores(
  trait_pca,
  display = "species",
  scaling = "symmetric",
  choices = c(1, 2)
)

# Afstand tot oorsprong berekenen
trait_scores_df <- data.frame(
  trait = rownames(trait_scores),
  PC1 = trait_scores[, 1],
  PC2 = trait_scores[, 2]
) %>%
  mutate(
    distance = sqrt(PC1^2 + PC2^2)
  ) %>%
  arrange(desc(distance))

 
#only the arrows for the AL traits 

trait_scores_df_AL <- trait_scores_df %>%
 dplyr::filter(startsWith(trait, "AL"))


label_pos <- with(
  trait_scores_df_AL,
  ifelse(
    abs(PC1) >= abs(PC2),
    ifelse(PC1 >= 0, 4, 2),  # rechts / links
    ifelse(PC2 >= 0, 3, 1)   # boven / onder
  )
)

text(
  trait_scores_df_AL$PC1,
  trait_scores_df_AL$PC2,
  labels = gsub("_", " ", gsub("^AL_", "", trait_scores_df_AL$trait)),
  pos = label_pos,
  offset = 0.35,
  cex = 0.7,
  xpd = NA
)

arrows(
  0, 0,
  trait_scores_df_AL$PC1,
  trait_scores_df_AL$PC2,
  length = 0.08
)



dev.off()


# make a figure with the LDL traits 
png(
  "PCA_traits_other.png",
  width = 9,
  height = 6,
  units = "in",
  res = 600
)


# make extra space for legend
par(mar = c(5, 4, 4, 12),xpd = FALSE)

ordiplot(
  trait_pca,
  type = "n",
  scaling = "symmetric",
  main = "PCA - Other Traits",
  xlab = paste0("PC1 (", round(eig_percent[1], 1), "%)"),
  ylab = paste0("PC2 (", round(eig_percent[2], 1), "%)"),
  xlim = c(-0.7, 0.75),
  ylim = c(-0.5, 0.5)
)

# ellips 
ordiellipse(
  trait_pca,
  groups = group,
  display = "sites",
  scaling = "symmetric",
  kind = "sd",
  draw = "polygon",,
  col = adjustcolor(
    phys_cols[seq_along(levels(group_phys))],
    alpha.f = 0.05
  ),
  border = adjustcolor(
    phys_cols[seq_along(levels(group))],
    alpha.f = 0.6
  ),
  lwd = 1.3
)


# points 
points(
  site_scores,
  col = phys_cols[as.character(group)],
  pch = 19,
  cex = 1
)

# legend 
legend(
  "right",
  inset = c(-0.32, 0),
  xpd = NA,
  legend = c(
    "Bare",
    "Bare 2",
    "Low-density dunes",
    "High-density dunes",
    "Green Beach",
    "Foredune",
    "Foredune 2"
    
  ),
  col = phys_cols[c("B", "B2", "LD", "HD", "DS", "FD", "FD2")],
  pch = 19,
  bty = "n",
  title = "Physiotopes"
)

trait_scores_df_other <- trait_scores_df %>%
  dplyr::filter(!startsWith(trait, "AL"))

trait_scores_df_other <- trait_scores_df_other %>%
  mutate(
    vector_length = sqrt(PC1^2 + PC2^2),
    label = gsub("_", " ", trait)
  )

top_traits <- trait_scores_df_other %>%
  slice_max(vector_length, n = 10)



arrows(
  0, 0,
  top_traits$PC1,
  top_traits$PC2,
  length = 0.08
)

label_pos <- with(
  top_traits,
  ifelse(
    abs(PC1) >= abs(PC2),
    ifelse(PC1 >= 0, 4, 2),
    ifelse(PC2 >= 0, 3, 1)
  )
)

text(
  top_traits$PC1,
  top_traits$PC2,
  labels = top_traits$label,
  pos = label_pos,
  offset = 0.35,
  cex = 0.7
)


dev.off()

# make table with all PC values, also for traits taht were not shown in PCA
trait_scores_all <- as.data.frame(
  scores(
    trait_pca,
    display = "species",
    choices = 1:4,
    scaling = "symmetric"
  )
)

trait_scores_all$trait <- rownames(trait_scores_all)

write.csv(
  trait_scores_all,
  "trait_PCA_scores.csv"
)
