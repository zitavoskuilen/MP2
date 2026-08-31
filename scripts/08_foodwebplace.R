###
# Making a stacked bar plot for the food web place data
###

# read in the feeding mode data 
feeding_mode <- read_delim("Feeding mode.csv", delim = ";", 
     escape_double = FALSE, trim_ws = TRUE)

str(feeding_mode)
ncol(feeding_mode)

species_in_traits_data

# finish 1 sept 