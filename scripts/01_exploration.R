# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    ---- Macrofauna Dutch Coast 2026  ----
#
###             ---- Author: Zita Maria Voskuilen S6043496 ---- 
###                         ---- Start date: 22-7-2026 ---- 
###              ---- Contact: z.m.voskuilen@student.rug.nl---- 
#
#                       ---- SET UP & DATA TIDYING ----
#
###############################################
###############################################
# PART 1: LOAD PACKAGES & R VERSION CONTROL ----

# This code ensures that R is using renv, a version control package manager.   
# Renv ensures that I can use the same package versions continuously. 
# This way, analyses are reproducible in the future regardless the existence 
# of new packages versions. 

# first we set the working directory 
setwd("~/Msc Ecology & Conservation/Master project 2/Master Project 2") 

# Restore packages and versions using renv: 
# - First, check if renv is installed - from the repository (website). 
# - If not, install it
# - Use the function restore from the renv package- 
if("renv" %in% installed.packages()[,"Package"]){
  renv::restore(repos = "https://cloud.r-project.org")
} else{
  install.packages("renv")
  library(renv)
  renv::restore(repos = "https://cloud.r-project.org")
}

# Here, we list and install all needed packages 

## - One of the used packages is "here". "Here" makes sure that all file paths work 
##   regardless of where the project is located

list_of_packages_used <- c("here", "ggplot2", "car", 
                           "tidyverse", "dplyr", "cowplot", 
                           "vegan", "renv", "lme4", "multcomp", 
                            "DHARMa", "emmeans", "performance",  "lme4", "lmerTest", 
                           "brglm2", "glmmTMB", "MuMIn", "remotes", "pairwiseAdonis", 
                           "gridExtra", "multcompView", "survival", "see",
                           "survminer", "readr", "lme4", "MASS",
                           "minqa", "ggnewscale", "qqplotr", "tidyr", "stringr"
)

# Now we check which packages from the list are not installed yet. 

new_packages <- list_of_packages_used[!(list_of_packages_used 
                                        %in% installed.packages()[,"Package"])]

#If not installed, install the missing ones and also keep them in renv 

if(length(new_packages)) renv::install(new_packages)

# Load packages (library) & update lock file
lapply(list_of_packages_used, library, character.only = TRUE)


renv::snapshot(type = "implicit")  # to save a snapshot of my project package 
# versions into a file called renv.lock


####################################
# setting a theme

common_theme <- theme_classic() +
  theme(
    axis.text = element_text(
      size = 14,
      face = "plain"
    ),
    axis.title = element_text(
      size = 14,
      face = "bold"
    ),
    axis.title.x = element_text(
      margin = margin(t = 8)
    ),
    axis.title.y = element_text(
      margin = margin(r = 8)
    ),
    legend.title = element_text(
      face = "bold",
      size = 12
    ),
    legend.text = element_text(
      size = 12
    )
  )


###############################################
# PART 2: LOAD DATA Macrofauna Fact 2026 ----

# load in the data from the online google drive 
data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQU4-VqDxuaSTUqruH83CrjeW6sTD95GdQlZnfCFKQLsLkcKOdmxxXD4G7mSRLf2AJeC3agHe8p_cOo/pub?gid=1054653854&single=true&output=csv")

# an offline version of this data was included in the data folder of the zip file project under the name macrofauna_data
# can be imported with: 
# data <- read_csv("~/Msc Ecology & Conservation/Master project 2/Thesis_Zita_Voskuilen_2026/data/Macrofauna_data.csv")

str(data)

# now filter only the rows that have "done" in the first column
data <- data %>%
  filter(Done. == "done")

# replace all NA's with zero's

data[is.na(data)] <- 0

# Replace the non numerical values in the data set 
# First find them
letters_per_column <- lapply(data, function(x) {
  x <- trimws(as.character(x))

  unique(x[
    !is.na(x) &
    x != "" &
    is.na(suppressWarnings(as.numeric(x)))
  ])
})

# select only columns where letters are found 
letters_per_column <- letters_per_column[
  lengths(letters_per_column) > 0
]

letters_per_column


# replace the h with 10 and the x with 1 in the data set
data[data == "h"] <- 10
data[data == "x"] <- 1


# remove for now the unknown and the last column from the dataset 
data <- data %>%
  dplyr::select(-unknown, -total_individuals)


# look how many rows there are of each pot id before i put them together
pot_ID_count <- data %>%
  count(pot_ID)



# FOR NOW TAKE OUT the first two HARVEST OF KAAP HOORN (TS1 and TS2) because they were pitfalls that were emptied after two days instead of one 
# select the rows that have poskey numbers that start with 2026_57 to 2026_71 and remove them from the dataset
# and the poskeys 2026_126 till 2026_140

data <- data %>%
 filter(!Poskey %in% c("2026_57", "2026_58", "2026_59", "2026_60", "2026_61", 
                        "2026_62", "2026_63", "2026_64", "2026_65", "2026_66",
                        "2026_67", "2026_68", "2026_69", "2026_70", "2026_71", 
                       "2026_126", "2026_127", "2026_128", "2026_129", "2026_130",
                       "2026_131", "2026_132", "2026_133", "2026_134", "2026_135", 
                       "2026_136", "2026_137", "2026_138", "2026_139", "2026_140"))

str(data)

# Percentage empty pitfalls per ecotoop
empty_summary <- empty_pitfalls %>%
  group_by(physiotope) %>%
  summarise(
    total_samples = n(),
    empty_samples = sum(empty),
    percentage_empty = round(mean(empty) * 100, 1),
    .groups = "drop"
  )

empty_summary



########
# FIXING THE SPECIES NAMES
########

# Merging the two cicadellidae columns into one column called Cicadellidae_sp and removing the two original columns

data <- data %>%
  mutate(
    Cicadellidae_sp = rowSums(
      across(c(Cicadellidae, `Cicadellidae.Nymph...88.`)),
      na.rm = TRUE
    )
  ) %>%
  dplyr::select(
    -Cicadellidae,
    -`Cicadellidae.Nymph...88.`
  )

colnames(data)

data <- data %>%
  rename_with(~ str_replace_all(.x, "\\.", "_")) %>%
  rename(
    Aphodius_sp_ = Aphodius_sp____95_,
    Agnonum_Lugens = Agonum_lugens____71_, 
    Alopecosa_pulverulenta = Alopecosa_pulverulenta___97_,
    Bryotropha_sp_ = Bryotropha_spec,
    Chaetocnema_hortensis = Chaetocnema_hortensis___82_,
    Cheiracanthium_sp_ = Cheiracanthium_sp____98_,
    Cleoninae_sp = Cleoninae_sp____134_,
    Clivina_fossor = Clivina_fossor___50_,
    Coccinellidae_sp = Coccinellidae_larve,
    Coleoptera_unkown = Coleoptera_sp___water_kever___42_,
    Coleoptera_larvae = Coleoptera_larve,
    Notaris_scripi = curculionidae__Notaris_scirpi__,
    Dyschirius_globosus = Dyschirius_globosus_,
    Dyschirius_sp_ = Dyschirius_sp___104_,
    Dryopidae_sp_ = Dryopidae_sp____101_,
    Entomobryomorpha_sp = Entomobryomorpha_sp_,
    Entomobryomorpha_sp_2 = Entomobryomorpha_sp__2,
    Tetramorium_caespitum = Formicidae__tetramorium_caespitum_,
    Lasius_psammophilus = Formicidae__Lasius_psammophilus__,
    Hemicrepidius_niger = Hemicrepidius_niger_,
    Latridiidae = Latridiidae___76_,
    Lepidoptera_larvae = Lepidoptera_larve,
    Nabidae = nabidae__sikkelwantsen__,
    Ochthebius_minimus = Ochthebius_sp___minimus__,
    Oedothorax_retusus = Oedothorax_retusus_,
    Neuroptera_larvae = Neuroptera_larf,
    Pardosa_monticola = Pardosa_monticola___68_,
    Philopedon_plagiatum = Philopedon_plagiatum___59_,
    Piesmatidae_sp = Piesmatidae_,
    Planuncus_tingitanus_nymph = Planuncus_tingitanus__nymph_,
    Prostigmata = Prostigmata__Acari_,
    Rhysodromus_fallax2 = Rhysodromus_fallax___74_,
    Saldula_saltatoria = Saldula_saltatoria___85_,
    Scolopostethus_sp_ = scolopostethus_sp_,
    Staphylinidae_Larvae_Certainly_coleoptera =     Staphylinidae_Larvae__Certainly_coleoptera_,
    Philopedon_plagiatum_elytra = schildjes_p__plagiatum,
    Phlaeothripidae_sp = Phlaeothripidae,
    Stenolophus_mixtus = Stenolophus_mixtus___33_,
    Tachyporus_sp_ = Tachyporus_sp____100_,
    Talitridae_sp = Talitridae,
    Thripidae_sp = Thripidae___92_,
    Psocoptera_sp = Psocoptera, 
    Trachyzelotes_pedestris = Trachyzelotes_pedestris___128_,
    Trochosa_sp_ = Trochosa_spec,
    Acrididae_sp = Acrididae,
    Phalangiidae_sp = Phalangiidae,
    Lithobius_sp = Lithobius,
    Julidae_sp = Julidae,
    Formicidae_sp = Formicidae,
    Microvelia_sp = Microvelia,
    Liopterus_haemorrhoidalis = Liopterus_haemorrohoidalis,
    Coccidula_rufa = Coccidule_rufa,
    Altica_sp = altica,
    Chrysomelidae_sp = Chrsyomelidae_sp_,
    Notoxus_monoceros = Noxotus_monoceros,
    Arctosa_perita = arctosa_perita,
    Arctosa_leopardus = arctosa_leopardus,
    Agelenidae_sp = Agelinidae,,
    Zelotes_electus = Zelotus_electus,
    Meligethes_sp = Meligethus,
    Paederus_riparius = Paderus_riparius
    ) %>%
  rename_with(~ str_replace(.x, "_sp_$", "_sp")) %>%
  rename_with(
    ~ if_else(
      str_detect(.x, "_"),
      .x,
      str_c(.x, "_sp")
    ),
    .cols = 7:167
  )


colnames(data)


# take out columns that have all zero's in them
data <- data[, colSums(data != 0) > 0]


############ 


###########
# DATA_TWO 
###################
# select only the harvest HK2 en HK3 and TS3 and TS4

data_two <- data %>%
  dplyr::filter(harvest %in% c("HK2", "HK3", "TS3", "TS4"))

# see which pitfalls are empty
species_cols <- names(data_two)[9:ncol(data_two)]

# total abundance per pitfall 
empty_pitfalls <- data_two %>%
  mutate(
    across(
      all_of(species_cols),
      ~ suppressWarnings(as.numeric(trimws(as.character(.x))))
    ),
    total_abundance = rowSums(
      across(all_of(species_cols)),
      na.rm = TRUE
    ),
    empty = total_abundance == 0
  )

# empty pitfalls per ecotoop
empty_summary <- empty_pitfalls %>%
  group_by(physiotope) %>%
  summarise(
    total_samples = n(),
    empty_samples = sum(empty),
    percentage_empty = round(100 * mean(empty), 1),
    .groups = "drop"
  )

empty_summary


pot_ID_count <- data_two %>%
  count(pot_ID)

# see how many pots are empty 
empty_pitfalls <- data %>%
  mutate(
    total_abundance = rowSums(
      across(where(is.numeric)),
      na.rm = TRUE
    ),
    empty = total_abundance == 0
  )

empty_summary <- empty_pitfalls %>%
  group_by(physiotope) %>%
  summarise(
    total_samples = n(),
    empty_samples = sum(empty),
    percentage_empty = round(mean(empty) * 100, 1),
    .groups = "drop"
  )

empty_summary

# now i have to add the rows toegteher that have the same pot_ID 
# first i'll do ot for the harvests seperately 

metadata_cols <- c(
  "Done.",
  "Harvest_total",
  "harvest",
  "days",
  "pot_ID",
  "physiotope",
  "poll_no_poll",
  "Poskey"
)

# make new dataset with only species columns 
species_cols <- names(data_two)[9:ncol(data_two)]

data_summed_two <- data_two %>%
  mutate(
    across(
      all_of(species_cols),
      ~ {
        x <- trimws(as.character(.x))
        x[x == ""] <- NA
        suppressWarnings(as.numeric(x))
      }
    )
  ) %>%
  mutate(
    pot_group = sub("_[^_]+$", "", pot_ID)
  ) %>%
  group_by(harvest, pot_group) %>%
  summarise(
    Harvest_total = first(Harvest_total),
    days = first(days),
    physiotope = first(physiotope),
    across(
      all_of(species_cols),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  rename(pot_ID = pot_group)


# now add all the pot_ID's together that have the same pot_ID but different harvests and delete all the species that have no occurence 
data_two_summed_final <- data_summed_two %>%
  group_by(pot_ID) %>%
  summarise(
    Harvest_total = first(Harvest_total),
    days = first(days),
    physiotope = first(physiotope),
    across(
      all_of(species_cols),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  dplyr::select(-days, -Harvest_total) %>%
  dplyr::select(
    where(~ !is.numeric(.x) || any(.x != 0, na.rm = TRUE))
  )

str(data_two_summed_final)

# few names are not right yet 
data_two_summed_final <- data_two_summed_final %>%
  dplyr::rename(
    Aphididae_sp = Aphidoidae_sp,
    Philopedon_plagiatus = Philopedon_plagiatum,
    Xantholinus_sp = Xantholinus
  )


# Abundance and species richness ####

physio_order <- c("B", "B2", "LD", "HD", "GB", "FD", "FD2")
physio_colors <- c(
  "B"   = phys_cols["bare"],
  "B2"  = phys_cols["bare2"],
  "LD"  = phys_cols["lowdensity"],
  "HD"  = phys_cols["highdensity"],
  "GB"  = phys_cols["duneslack"],
  "FD"  = phys_cols["foredune"],
  "FD2" = phys_cols["foredune2"]
)

# Delete orginal names 
physio_colors <- unname(physio_colors)
names(physio_colors) <- physio_order


#  select taxon columns 
species_data <- data_two_summed_final %>%
  dplyr::select(-pot_ID, -location, -physiotope)

# total abundance and pooled richness 
plot_data <- data_two_summed_final %>%
  mutate(
    physiotope = recode(physiotope, "DS" = "GB"),
    total_per_sample = rowSums(species_data, na.rm = TRUE)
  ) %>%
  group_by(physiotope) %>%
  summarise(
    total_abundance = sum(total_per_sample),
    richness = sum(
      colSums(across(all_of(names(species_data))), na.rm = TRUE) > 0
    ),
    .groups = "drop"
  ) %>%
  mutate(
    physiotope = factor(physiotope, levels = physio_order)
  )

physio_order <- c("B", "B2", "LD", "HD", "GB", "FD", "FD2")

physio_colors <- c(
  "B"   = "#E8D7B0",
  "B2"  = "#9B6F3E",
  "LD"  = "#8FBF68",
  "HD"  = "#356B3A",
  "GB"  = "#4FA3A5",
  "FD"  = "#F2C94C",
  "FD2" = "#D97706"
)

# Mean abundance and SD
abundance_plot_data <- abundance_df %>%
  mutate(physiotope = recode(as.character(physiotope), "DS" = "GB")) %>%
  group_by(physiotope) %>%
  summarise(
    mean = mean(abundance),
    sd = sd(abundance),
    .groups = "drop"
  ) %>%
  mutate(physiotope = factor(physiotope, levels = physio_order))

# mean richness and SD
physio_order <- c("B", "B2", "LD", "HD", "GB", "FD", "FD2")

abundance_box_data <- abundance_df %>%
  mutate(
    physiotope = recode(as.character(physiotope), "DS" = "GB"),
    physiotope = factor(physiotope, levels = physio_order)
  )

# Significance groups 
abundance_letters <- data.frame(
  physiotope = factor(physio_order, levels = physio_order),
  letter = c("bc", "bc", "a", "bc", "ab", "c", "abc")
)

# letter positions 
letter_positions <- abundance_box_data %>%
  group_by(physiotope) %>%
  summarise(
    y = max(abundance, na.rm = TRUE) + 12,
    .groups = "drop"
  ) %>%
  left_join(abundance_letters, by = "physiotope")

# Boxplot
plot_abundance <- ggplot(
  abundance_box_data,
  aes(x = physiotope, y = abundance)
) +
  geom_boxplot(
    aes(fill = physiotope),
    width = 0.65,
    alpha = 1,
    outlier.shape = NA,
    linewidth = 0.6
  ) +
  geom_jitter(
    width = 0.12,
    size = 2.5,
    alpha = 1
  ) +
  geom_text(
    data = letter_positions,
    aes(x = physiotope, y = y, label = letter),
    inherit.aes = FALSE,
    size = 5
  ) +
  scale_fill_manual(values = physio_colors) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.15))
  ) +
  labs(
    x = NULL,
    y = "Macrofauna abundance per sample"
  ) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "none"
  )

plot_abundance

ggsave(
  plot_abundance,
  filename = "plots/abundance_boxplot.png",
  width = 8,
  height = 6,
  dpi = 300
)

richness_box_data <- diversity_df %>%
  mutate(
    physiotope = recode(as.character(physiotope), "DS" = "GB"),
    physiotope = factor(physiotope, levels = physio_order)
  )

plot_richness <- ggplot(
  richness_box_data,
  aes(x = physiotope, y = richness)
) +
  geom_boxplot(
    aes(fill = physiotope),
    width = 0.65,
    alpha = 1,
    outlier.shape = NA,
    linewidth = 0.6
  ) +
  geom_jitter(
    width = 0.1,
    height = 0,
    size = 2.5,
    alpha = 0.85
  ) +
  scale_fill_manual(values = physio_colors) +
  scale_y_continuous(
    breaks = seq(0, 30, 5),
    expand = expansion(mult = c(0.02, 0.1))
  ) +
  labs(
    x = NULL,
    y = "Taxonomic richness per sample"
  ) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "none"
  )

plot_richness

ggsave(
  plot_richness,
  filename = "plots/richness_boxplot.png",
  width = 8,
  height = 6,
  dpi = 300
)

# see which species are in FD but not in FD2 ####
library(dplyr)

# select onlt taxon columns
species_cols <- setdiff(
  names(data_two_summed_final),
  c("pot_ID", "location", "physiotope")
)

# Total abundance per taxon in FD
fd <- data_two_summed_final %>%
  filter(physiotope == "FD") %>%
  summarise(across(all_of(species_cols), ~sum(.x, na.rm = TRUE)))

# Total abundance per taxon in FD2
fd2 <- data_two_summed_final %>%
  filter(physiotope == "FD2") %>%
  summarise(across(all_of(species_cols), ~sum(.x, na.rm = TRUE)))

# Taxa present  in FD, not present in FD2
only_fd <- species_cols[
  as.numeric(fd[1, species_cols]) > 0 &
  as.numeric(fd2[1, species_cols]) == 0
]

only_fd
length(only_fd)

only_fd2 <- species_cols[
  as.numeric(fd2[1, species_cols]) > 0 &
  as.numeric(fd[1, species_cols]) == 0
]

only_fd2
length(only_fd2)


fd_unique_table <- data.frame(
  Taxon = only_fd,
  Abundance_FD = as.numeric(fd[1, only_fd])
) %>%
  arrange(desc(Abundance_FD))

fd2_unique_table <- data.frame(
  Taxon = only_fd2,
  Abundance_FD2 = as.numeric(fd2[1, only_fd2])
) %>%
  arrange(desc(Abundance_FD2))

fd_unique_table
fd2_unique_table

fd_comparison <- diversity_df %>%
  filter(physiotope %in% c("FD", "FD2")) %>%
  dplyr::select(location, physiotope, richness) %>%
  pivot_wider(
    names_from = physiotope,
    values_from = richness
  ) %>%
  mutate(
    richness_difference = FD - FD2
  )

fd_comparison
