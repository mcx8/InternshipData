# Maxine Cruz
# tmcruz@email.arizona.edu
# April 5, 2021

# Permutation test to test significance of species richness on UArizona campus

# NOTES FOR MYSELF:
# Test statistic: mean of treatment - mean of control
# Here it is going to be difference in species richness of Tucson subsample and UofA
# UofA = "treatment", Tucson = "control"
# Permutation test: repeating this several times with randomized groups
# From the differences we get, we want to see how many of them are = or >test statistic
# Lets us know if there is a significant difference between UofA and Tucson richness

library(tidyverse)

Tucson_MARCH <- read.csv(file = "HymLep_only_Data/Tucson_MARCH.csv")
UofA_MARCH <- read.csv(file = "HymLep_only_Data/UofA_MARCH.csv")

# PERMUTATION TESTS:
# Isolate column with species names
Tucson_species_column <- Tucson_MARCH %>% 
  select(scientific_name)
UofA_species_column <- UofA_MARCH %>% 
  select(scientific_name)

# Save the new species-only tables
# Puts them in the GitHub folders
write.csv(Tucson_species_column, 
          file = "Worked_data/Tucson_species_column.csv")
write.csv(UofA_species_column, 
          file = "Worked_data/UofA_species_column.csv")

# When importing the file back, there's a column with numbers
# Remove that column before running tests
# Also need to convert this table into a list so that the calculations work: pull()
td <- Tucson_species_column %>%
  select(scientific_name) %>%
  pull()
ud <- UofA_species_column %>%
  select(scientific_name) %>%
  pull()

# Designate how many times we want to run permutations
# Max. number of permutations based on n!/(n!-r!)(r!)
# Where n = total and r = subset
# But here we just want to do 1000 because the max is probs too much
num_reps <- 1000

# Making an empty list/vector to keep the results we get from running the permutations
# numeric() takes num_reps and creates that many elements in the list/vector
# All the elements will be 0 for now
rich_diff <- numeric(num_reps)

# Permutation test
# 1) designates how many times what is in the brackets will be repeated
# 2) taking Tucson subsamples that are the same length as number of UofA species
#    replace() changes the subset values every time this runs through
# 3) subtract the richness from Tucson subsample from UofA richness
#    unique() takes what species are there (reduces repeating values)
# Every time this repeats (for num_reps times), the result will be put into rich_diff list
# So rich_diff list would have actual values after this
for (i in 1:num_reps) {
  tucson_sub <- sample(x = td,
                       size = length(ud),
                       replace = TRUE)
  rich_diff[i] <- length(unique(ud)) - length(unique(tucson_sub))
  # tucson_sub <- sample(x = td,
  #                      size = 100,
  #                      replace = FALSE)
  # UA_sub <- sample(x = ud,
  #                  size = 100,
  #                  replace = FALSE)
  # rich_diff[i] <- length(unique(UA_sub)) - length(unique(tucson_sub))
}

# How many of those show that UofA has a greater species richness than Tucson subsample?
# If the values are positive then UofA > Tucson subsample in richness, 
# because we did ud - td, so
greater <- sum(rich_diff > 0)

# What is the probability that a random sample from another random distribution
# has UofA with more species than Tucson?
# What if the UofA IS a hotspot? 
prob_hotspot <- greater / num_reps

# What is the probability that the UofA is NOT a hotspot?
p_value <- 1 - prob_hotspot

# View p-value
p_value

# Plotting this on a graph
# Need to put the values into a table to make plotting it easier
# Number of permutations in one column, and results in rich_diff go in another
rich_df <- data.frame(permutation = 1:num_reps,
                      delta = rich_diff)

# Need ggplot to make the histogram
library(ggplot2)

# Null hypothesis is that the mean difference between UofA and Tucson richness is 0
delta_plot <- ggplot(data = rich_df, mapping = aes(x = delta)) +
  geom_histogram(bins = 16) +
  geom_vline(xintercept = 0, lty = 2) + # dashed line for null hypothesis
  xlab(label = "# Species at UArizona - # Species at Tucson") +
  ylab(label = "# Permutations") +
  theme_bw()

# View the plot
delta_plot

# Save the plot
ggsave(filename = "permutation_plot.jpg", plot = delta_plot)
