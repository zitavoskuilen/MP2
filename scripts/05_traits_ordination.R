
## PCA WITH TRAITS ORDINATION ####

## CLEANING THE TRAITS DATA ####

# read in the traits data 
traits <- read_delim("traits_5.csv", delim = ";", 
     escape_double = FALSE, trim_ws = TRUE)

ncol(traits)
#View(traits)
#View(data_two_summed_final)

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
  group %in% names(abbr),
  paste0(abbr[group], "_", clean(subtrait)),
  clean(subtrait)
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

# delete all RID columns
traits <- traits %>%
  dplyr::select(-matches("rid"))

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

# now i need to only take the rows from the traits data that containt he species that are in my data_two_species_traits data frame. so i will make a vector of the species names in the data_two_species_traits data frame and then filter the traits data frame for those species.

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
  dplyr::select(-c(Taxon_ID, Phylum, Class, Order, Family, Subfamily, Genus, Reference_ID)) 

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

## LONG FORMAT OF THE ABUNDANCE DATA ####


