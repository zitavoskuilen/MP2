############################################################
# COMPLETE ORDINATION WORKFLOW
############################################################

library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)


############################################################
# PART 1: CREATE PERMANENT SAMPLE IDs
############################################################

# Namen van de eerste zes metadatakolommen opslaan
metadata_names <- names(data)[1:6]

# Geef iedere oorspronkelijke rij een vaste ID
data_id <- data %>%
  dplyr::mutate(
    sample_id = as.character(dplyr::row_number()),
    .before = 1
  )


############################################################
# PART 2: SEPARATE METADATA AND SPECIES DATA
############################################################

# Alleen metadata
metadata_all <- data_id %>%
  dplyr::select(
    sample_id,
    dplyr::all_of(metadata_names)
  ) %>%
  as.data.frame()

# Alleen soortenkolommen
species_raw <- data_id %>%
  dplyr::select(
    -sample_id,
    -dplyr::all_of(metadata_names)
  )


############################################################
# PART 3: REMOVE TEMPORARILY PROBLEMATIC SPECIES COLUMNS
############################################################

columns_to_remove <- c(
  "Anurida.maritima",
  "Clubiona.sp.",
  "Entomobryomorpha.sp.",
  "Entomobryomorpha.sp..2",
  "unknown",
  "Collembola.sp."
)

# any_of() geeft geen fout als een kolom niet bestaat
species_raw <- species_raw %>%
  dplyr::select(
    -dplyr::any_of(columns_to_remove)
  )


############################################################
# PART 4: CHECK FOR REMAINING NON-NUMERIC VALUES
############################################################

bad_columns <- names(species_raw)[
  vapply(
    species_raw,
    function(x) {

      values <- trimws(as.character(x))

      any(
        !is.na(values) &
          values != "" &
          is.na(suppressWarnings(as.numeric(values)))
      )
    },
    logical(1)
  )
]

cat("Columns with non-numeric values:\n")
print(bad_columns)


# Stop het script wanneer er nog probleemkolommen zijn
if (length(bad_columns) > 0) {

  bad_values <- lapply(
    species_raw[bad_columns],
    function(x) {

      values <- trimws(as.character(x))

      unique(
        values[
          !is.na(values) &
            values != "" &
            is.na(suppressWarnings(as.numeric(values)))
        ]
      )
    }
  )

  print(bad_values)

  stop(
    "There are still non-numeric values in the species data. ",
    "Inspect bad_columns and bad_values."
  )
}


############################################################
# PART 5: CONVERT SPECIES DATA TO NUMERIC
############################################################

species_raw <- species_raw %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~ {

        values <- trimws(as.character(.x))

        # Lege tekst omzetten naar NA
        values[values == ""] <- NA_character_

        as.numeric(values)
      }
    )
  )

# Ontbrekende waarnemingen als nul behandelen
species_raw[is.na(species_raw)] <- 0

# Omzetten naar gewone dataframe
species_raw <- as.data.frame(species_raw)

# Oorspronkelijke sample-ID als rownames bewaren
rownames(species_raw) <- data_id$sample_id
rownames(metadata_all) <- metadata_all$sample_id


############################################################
# PART 6: REMOVE EMPTY SAMPLES AND EMPTY SPECIES
############################################################

# Opnames met minimaal één waargenomen individu behouden
keep_nonempty <- rowSums(
  species_raw,
  na.rm = TRUE
) > 0

cat("\nNon-empty samples:\n")
print(table(keep_nonempty))


# Exact dezelfde rijen uit soortenmatrix en metadata selecteren
species0 <- species_raw[
  keep_nonempty,
  ,
  drop = FALSE
]

metadata0 <- metadata_all[
  keep_nonempty,
  ,
  drop = FALSE
]


# Soorten verwijderen die na filtering nergens voorkomen
species0 <- species0[
  ,
  colSums(species0, na.rm = TRUE) > 0,
  drop = FALSE
]


# Controles
stopifnot(
  nrow(species0) == nrow(metadata0),
  identical(
    rownames(species0),
    metadata0$sample_id
  ),
  all(rowSums(species0) > 0),
  all(colSums(species0) > 0),
  all(as.matrix(species0) >= 0)
)

cat("\nDimensions after removing empty rows and columns:\n")
print(dim(species0))


############################################################
# PART 7: CHECK ECOLOGICAL CONNECTIVITY
############################################################

connectivity <- vegan::distconnected(
  vegan::no.shared(species0),
  trace = TRUE
)

cat("\nConnectivity groups:\n")
print(table(connectivity))


# Grootste verbonden groep automatisch bepalen
main_group <- as.integer(
  names(
    which.max(
      table(connectivity)
    )
  )
)

keep_connected <- connectivity == main_group


# Laat zien welke samples buiten de hoofdgroep vallen
show_columns <- intersect(
  c("sample_id", "Poskey", "pot_ID"),
  names(metadata0)
)

cat("\nSamples outside the main connected group:\n")

print(
  metadata0[
    !keep_connected,
    show_columns,
    drop = FALSE
  ]
)


############################################################
# PART 8: SELECT MAIN CONNECTED GROUP
############################################################

# Exact dezelfde selectie toepassen op soorten en metadata
species_group1 <- species0[
  keep_connected,
  ,
  drop = FALSE
]

metadata_group1 <- metadata0[
  keep_connected,
  ,
  drop = FALSE
]


# Soorten verwijderen die alleen voorkwamen in uitgesloten samples
species_group1 <- species_group1[
  ,
  colSums(species_group1, na.rm = TRUE) > 0,
  drop = FALSE
]


# Metadata in exact dezelfde volgorde zetten
metadata_group1 <- metadata_group1 %>%
  dplyr::arrange(
    match(
      sample_id,
      rownames(species_group1)
    )
  )


# Belangrijke controles
stopifnot(
  nrow(species_group1) == nrow(metadata_group1),
  identical(
    rownames(species_group1),
    metadata_group1$sample_id
  ),
  !anyDuplicated(metadata_group1$sample_id)
)

cat("\nDimensions connected species matrix:\n")
print(dim(species_group1))

cat("\nNumber of metadata rows:\n")
print(nrow(metadata_group1))


############################################################
# PART 9: DCA FOR GRADIENT LENGTH
############################################################

dca_group1 <- vegan::decorana(
  species_group1
)

print(dca_group1)


# Sitescores van de vier DCA-assen
dca_site_scores <- vegan::scores(
  dca_group1,
  display = "sites",
  choices = 1:4
)


# Lengte van iedere as berekenen
axis_lengths <- apply(
  dca_site_scores,
  2,
  function(x) {
    diff(
      range(x, na.rm = TRUE)
    )
  }
)

gradient_length <- unname(
  axis_lengths[1]
)

cat(
  "\nDCA axis lengths:",
  round(axis_lengths, 3),
  "\n"
)

cat(
  "Gradient length DCA axis 1:",
  round(gradient_length, 3),
  "\n"
)


############################################################
# PART 10: INTERPRET GRADIENT LENGTH
############################################################

if (gradient_length < 3) {

  cat(
    "Short gradient: linear methods such as PCA or RDA are suitable.\n"
  )

} else if (gradient_length <= 4) {

  cat(
    "Intermediate gradient: compare linear and unimodal methods.\n"
  )

} else {

  cat(
    "Long gradient: CA or CCA fit the unimodal-response assumption.\n"
  )

  cat(
    "NMDS is also suitable as a distance-based ordination method.\n"
  )
}


############################################################
# PART 11: RUN NMDS
############################################################

set.seed(123)

nmds_res <- vegan::metaMDS(
  species_group1,
  distance = "bray",
  k = 2,
  trymax = 200,

  # Geen automatische Wisconsin/square-root transformation
  autotransform = FALSE,

  # Gebruik de oorspronkelijke Bray-Curtis-afstanden
  noshare = FALSE,

  # Houd ordinatie-afstanden op een begrijpelijkere schaal
  halfchange = FALSE,

  trace = TRUE
)


cat("\nNMDS result:\n")
print(nmds_res)

cat(
  "\nNMDS stress:",
  round(nmds_res$stress, 4),
  "\n"
)


############################################################
# PART 12: CHECK NMDS QUALITY
############################################################

# Shepard plot
vegan::stressplot(nmds_res)


# Aantal samples en unieke soortensamenstellingen
cat(
  "\nNumber of samples:",
  nrow(species_group1),
  "\n"
)

cat(
  "Number of unique species compositions:",
  nrow(unique(species_group1)),
  "\n"
)


# Goodness-of-fit per sample
goodness_values <- vegan::goodness(
  nmds_res
)

cat("\nGoodness summary:\n")
print(summary(goodness_values))


############################################################
# PART 13: EXTRACT NMDS SITE SCORES
############################################################

site_scores <- as.data.frame(
  vegan::scores(
    nmds_res,
    display = "sites"
  )
)

site_scores$sample_id <- rownames(site_scores)


# Controleren of alle score-IDs in de metadata bestaan
stopifnot(
  nrow(site_scores) == nrow(metadata_group1),
  all(
    site_scores$sample_id %in%
      metadata_group1$sample_id
  )
)


############################################################
# PART 14: JOIN SITE SCORES AND METADATA
############################################################

plot_data <- site_scores %>%
  dplyr::left_join(
    metadata_group1,
    by = "sample_id"
  )


# Controles
stopifnot(
  nrow(plot_data) == nrow(site_scores),
  !anyDuplicated(plot_data$sample_id)
)

cat("\nDimensions plot data:\n")
print(dim(plot_data))

cat("\nColumns in plot data:\n")
print(names(plot_data))

head(plot_data)


############################################################
# PART 15: BASIC NMDS PLOT
############################################################

nmds_plot <- ggplot(
  plot_data,
  aes(
    x = NMDS1,
    y = NMDS2,
    colour = pot_ID
  )
) +
  geom_point(
    size = 2.7,
    alpha = 0.8
  ) +
  coord_equal() +
  theme_classic() +
  labs(
    title = "NMDS of arthropod community composition",
    subtitle = paste0(
      "Bray-Curtis dissimilarity; stress = ",
      round(nmds_res$stress, 3)
    ),
    x = "NMDS1",
    y = "NMDS2",
    colour = "Poll treatment"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

print(nmds_plot)
