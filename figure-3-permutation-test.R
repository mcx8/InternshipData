# Permutation test for UA as richness hotspot
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-03-30

# Do permutation test to see if the species richness captured by UArizona 
# observations is significantly higher than a random sample of same size for 
# Tucson observations. Include both p-value of UArizona richness and 
# visualization of permutations (a histogram)
library(ggplot2)

# Create a sample of data of "species" seen in Tucson; for these mock data we 
# are using the set of lower-case letters
Tucson <- sample(x = c(letters, LETTERS), replace = TRUE, size = 500)
# To make the UA sample we now sample from that Tucson sample, using sampling
# *without* replacement to artificially make the UArizona sample more 
# species-rich
UArizona <- sample(x = Tucson, replace = FALSE, size = 50)
# NOTE: the two data creation steps would be replaced by code to read in data 
# from data files, rather than randomly generating data

# Number of permutation replicates
num_reps <- 1000

# A variable to keep the test statistic, in this case, the difference in species
# richness of the UArizona sample and each sub-sample from the Tucson sample
# In this case, the rich_diff variable is a numeric vector of length equal to 
# the number of permutation replicates to run
rich_diff <- numeric(num_reps)

# Now perform num_reps permutations, each time creating a subset of the Tucson 
# data that is the same size as the UArizona data
for (i in 1:num_reps) {
  tucson_subsample <- sample(x = Tucson, size = length(UArizona), replace = TRUE)
  # Calculate differences in species richness (total number of unique species) 
  # and add the value to our rich_diff vector
  rich_diff[i] <- length(unique(UArizona)) - length(unique(tucson_subsample))
}

# Find out how many permutations had more species in the UArizona sample than 
# in the Tucson subsample.
# Values in rich_diff that are positive indicate that, for that 
# permutation, the number of species in the UArizona sample was greater than 
# the number of species in the Tucson subsample.
num_less <- sum(rich_diff > 0)

# The proportion of permutations with more species in the UArizona sample 
# serves as the probability that UArizona and Tucson are sampled from 
# *different* distributions (i.e. UArizona is a hotspot)
prob_hotspot <- num_less / num_reps

# BUT, we don't report that probability. Instead we report the probability of 
# the null hypothesis, that UArizona and Tucson are drawn from the same 
# distribution. This is our p_value.
p_value <- 1 - prob_hotspot

# If the p_value is less than 0.05, we reject the null!
p_value

# We can also plot the distribution of those differences. Null hypothesis 
# predicts the mean difference should be equal to zero.
# Start by making a data frame for easier plotting
rich_df <- data.frame(permutation = 1:num_reps,
                      delta = rich_diff)
# Histogram of the differences
delta_plot <- ggplot(data = rich_df, mapping = aes(x = delta)) +
  geom_histogram(bins = 16) +
  geom_vline(xintercept = 0, lty = 2) + # dashed line for null hypothesis
  xlab(label = "# Sp. UArizona - # Sp. Tucson") +
  ylab(label = "# Permutations") +
  theme_bw()
delta_plot
