
## PCA WITH TRIATS ORDINATION ####

# checking species ooccurence for the traits table 
# here i do it for the data two data set (only two harvests of TS and the dutch coast )

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

# select the species which are not singletons or doubletons in the data summed two from the traits dataset 

# read in the traits data 
traits <- read_delim("traits.csv", delim = ";", 
    escape_double = FALSE, trim_ws = TRUE)

View(traits)
View(data_two_summed_final)


