
## PCA WITH TRAITS ORDINATION ####

# checking species ooccurence for the traits table 

#Now I will check how many species only occur
occurrence <- colSums(
  data_two_summed_final[, 5:ncol(data_two_summed_final)] > 0,
  na.rm = TRUE
)

occurrence

total_abundance <- colSums(data_two_summed_final[, 5:ncol(data_summed_final)], na.rm = TRUE)

#... on one site with an abundance lower than 5
singletons <- names(which(occurrence == 1 & total_abundance < 5))

colSums(
  data_summed_final[, colnames(data_summed_final) %in% singletons,
                       drop = FALSE]
)
#... on two sites with a total abundance lower than 7
doubletons <- names(
  which(colSums(data_summed_final > 0) == 2 & total_abundance < 7)
)
colSums(
  data_summed_final[, colnames(data_summed_final) %in% doubletons,
                       drop = FALSE]
)
#... three times (so occuring one time on three sites)
three_times_one <- names(
  which(colSums(data_summed_final > 0) == 3 & total_abundance == 3)
)

# none in my data set 


# read in the traits data 
traits <- read_delim("traits.csv", delim = ";", 
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
  "Larval/juvenile development location" = "LDL",
  "Proxy species" = "Proxy_species"
)

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

traits <- traits[-c(1, 2), ]
names(traits) <- new_names

# delete all RID columns
traits <- traits %>%
  dplyr::select(-matches("RID"))

# only select the column witht he trits that i want and the amily, subfamily, genus ans species columns
traits <- traits %>%
  dplyr::select(
    Family,
    Subfamily,
    Genus, 
    Species,
    matches("^[A-Z]+_")
  )

# delete all the rows that have non numerical values in the columns > 6 
traits <- traits %>%
  dplyr::filter(!if_all(6:last_col(), ~ .x == "NF")
  )

# replace all the NF's with zero's 
traits <- traits %>% 
  dplyr::mutate(across(everything(), ~ replace(.x, .x == "NF", 0))
  )

# make a new column with species where if the species column is already filled, it stays the same name but with a _ in the middle and if it not filled, take the first column on the left of species that is filled and put it in the new column with _sp behind it. and delte allt he other columns that are not species or the traits columns. so also the reference Id and the taxon_id
traits1 <- traits %>%
  mutate(
    Species = if_else(
      !is.na(Species) & Species != "",
      gsub(" ", "_", Species),
      paste0(coalesce(na_if(Genus, ""), na_if(Subfamily, ""), na_if(Family, "")), "_sp")
    )
  ) %>%
  dplyr::select(Species, matches("^[A-Z]+_")) %>%
  dplyr::select(-Reference_ID, -Taxon_ID)





# i have to make sure that the species names in the traits data can match the ones in the data_two_summed_final data frame. 



