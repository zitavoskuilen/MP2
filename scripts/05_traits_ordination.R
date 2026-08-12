
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

View(traits)
View(data_two_summed_final)

# first i will fix the trait names in the trait data set 

traits <- traits %>%
  {
    names(.) <- make.names(as.character(unlist(.[1, ])), unique = TRUE)
    .[-1, ]
  } %>%
  rename(
    ALH = Adult.living.habitat,
    ALD = Adult.living.depth..soil.or.water..cm.,
    ABS = Adult.body.size..mm.,
    #OFS = Offspring.size..mm.,
    #LG = Longevity..yr.,
    #ASM = Age.of.sexual.maturation,
    SAP = Seasonal.activty.pattern,
    DAP = Daily.activity.pattern,
    FM = Feeding.mode,
    AL = Adult.locomotion,
    RM = Reproductive.mode,
    #RS = Reproductive.season,
    #RF = Reproduction.frequency,
    #FC = Fecundity,
    LDL = Larval.juvenile.development.location,
    #OS = Overwintering.stage,
    #OL = Overwintering.location,
    Proxy_species = Proxy.species  ) %>%
   {
    prefix <- names(.)
    prefix[9:length(prefix)] <- tidyr::fill(
      data.frame(prefix = replace(prefix[9:length(prefix)],
                                  grepl("^X", prefix[9:length(prefix)]),
                                  NA)),
      prefix
    )$prefix} 


# i have to make sure that the species names in the traits data can match the ones in the data_two_summed_final data frame. 



