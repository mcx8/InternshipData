# Maxine Cruz
# tmcruz@email.arizona.edu
# April 5, 2021

# Permutation test to test significance of species richness on UArizona campus
# Same purpose as UA Tucson permutation test,
# but this one selects random areas in Tucson are equal of magnitude to that of UofA
# a.k.a. THIS CODE CONTROLS THE SAMPLING AREA

# WHAT DO I WANT TO DO?
# Find max/min latitude and longitude of UofA observations
# Generate random rectangles of that area around Tucson, excluding UofA
# Take observations within those random rectangles
# Calculate species diversity in those random rectangles
# Run this several times,
# Subtracting species diversity of random rectangles from UofA rectangle species diversity
# See result

# -----------------------------------------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggmap)
library(ggplot2)

# -----------------------------------------------------------------------------------------------

# Data we are using
Tucson_MARCH <- read.csv(file = "HymLep_only_Data/Tucson_MARCH.csv")
UofA_MARCH <- read.csv(file = "HymLep_only_Data/UofA_MARCH.csv")

# Isolate columns with species names and location coordinates
Tucson_speciescoord <- Tucson_MARCH %>% 
  select(scientific_name, latitude, longitude)
UofA_speciescoord <- UofA_MARCH %>% 
  select(scientific_name, latitude, longitude)

# Tucson data currently has UA in it, but we do not want that
# because then we would be taking random samples from UA as well
# So we want to make a new Tucson data set with data unique to Tucson (no UA data)
# Species will usually be too similar to separate them, so we'll use coordinates
Tucson_speciescoord <- anti_join(Tucson_speciescoord, UofA_speciescoord, by = "latitude")

# -----------------------------------------------------------------------------------------------

# BOUNDARY EDIT 1
# Since we want to compare whether any area in urban/suburban Tucson has
# more species that UofA,
# We want to constrain the data to only urban/suburban Tucson
# NOTE: Originally we didn't, so the distribution showed two humps

# Code from Dr. Prudic
# Filter to Tucson latitude and longitude using boxes
# rectangle 1: 32.351802, 32.209744, -110.962849, -111.046889 
# rectangle 2: 32.260557, 32.209744, -110.841198, -110.962849
# Whenever a point falls within the box, it is assigned TRUE
# If not, it is FALSE
# And then all of those results are put back into a table (tucson_spp)

# tucson_spp <- Tucson_speciescoord %>% 
#   mutate(rectangle1 = if_else(condition = between(latitude, 32.209744, 32.351802) &
#                               between(longitude, -111.046889, -110.962849), 
#                             true = TRUE, false = FALSE), 
#        rectangle2 = if_else(condition = between(latitude, 32.209744, 32.260557) &
#                               between(longitude, -110.962849, -110.841198), 
#                             true = TRUE, false = FALSE)) %>%
#   mutate(tucson = rectangle1 | rectangle2)

# This modifies tucson_spp
# We are only keeping the points inside the rectangles (TRUEs)
# AKA in urban/suburban Tucson
# And then I believe the minus signs make it so the values are not TRUE/FALSE,
# but the data values (if you run this without the minus signs it is just a bunch of TRUE/FALSE)
# tucson_spp <- tucson_spp %>%
#   filter(tucson == TRUE) %>%
#   select(-rectangle1, -rectangle2, -tucson)

# -----------------------------------------------------------------------------------------------

# BOUNDARY EDIT 2
# Restricting points to a rectangle in urban Tucson
# Removes Sweetwater and Tohono Chul hotspots from data pool
# Boundaries:
# North: River, South: Ajo (32.177922)
# West: I-10, East: Kolb (-110.840651)
# River and I-10 aren't straight lines, so we need to make a few boxes to cover the area

# Roger to River (32.279213, 32.297346) / La Cholla to 1st (-111.012016, -110.961095)
# Speedway to Roger (32.235993, 32.279213) / N Fairview to Kolb (-110.986553, -110.840651)
# Speedway to E 36th (32.235993, 32.192389) / I-10 to Kolb (-110.979606, -110.840651)
# E 36th to Ajo (32.192389, 32.177926) / S Park to Kolb (-110.956259, -110.840651)

# Roger to Wetmore (32.279213, 32.286582) / 1st to Kelvin (-110.961095, -110.920330)
# Wetmore to River (32.286582, 32.294404) / 1st to Mountain (-110.961095, -110.952424)
# Wetmore to River (32.286582, 32.291290) / Mountain to Campbell (-110.952424, -110.943813)

# Roger to River (32.279213, 32.306120) / Kain to La Cholla (-111.020541, -111.012016)
# Zinnia to River (32.290661, 32.308577) / Kain to N Davis (-111.020541, -111.026898)
# Ruthrauff to River (32.294224, 32.310832) / N Davis to N Emerald (-111.026898, -111.032816)
# Curtis to River (32.301487, 32.311629) / Emerald to N Camino de la Tierra (-111.032816, -111.037383)

# Grant to Roger (32.250341, 32.279213) / Flowing Wells to Fairview (-110.995910, -110.986553)
# Glenn to Roger (32.257578, 32.279213) / Flowing Wells to Romero (-110.995910, -111.003494)
# Fort Lowell to Roger (32.264722, 32.279213) / Romero to N Sun Tran (-111.003494, -111.008064)
# Prince to Roger (32.271996, 32.279213) / N Sun Tran to La Cholla (-111.008064, -111.012016)
# W Weymouth to Roger (32.274288, 32.279213) / La Cholla to N Sullinger (-111.012016, -111.017794)

# W 42nd to 36th (32.186822, 32,192389) / 12th to S Park (-110.977564, -110.956259)

tucson_spp2 <- Tucson_speciescoord %>% 
  mutate(rectangle1 = if_else(condition = between(latitude, 32.279213, 32.297346) &
                                between(longitude, -111.012016, -110.961095), 
                              true = TRUE, false = FALSE), 
         rectangle2 = if_else(condition = between(latitude, 32.235993, 32.279213) &
                                between(longitude, -110.986553, -110.840651), 
                              true = TRUE, false = FALSE),
         rectangle3 = if_else(condition = between(latitude, 32.192389, 32.235993) &
                                between(longitude, -110.979606, -110.840651), 
                              true = TRUE, false = FALSE), 
         rectangle4 = if_else(condition = between(latitude, 32.177926, 32.192389) &
                                between(longitude, -110.956259, -110.840651), 
                              true = TRUE, false = FALSE),
         rectangle5 = if_else(condition = between(latitude, 32.279213, 32.286582) &
                                between(longitude, -110.961095, -110.920330), 
                              true = TRUE, false = FALSE), 
         rectangle6 = if_else(condition = between(latitude, 32.286582, 32.294404) &
                                between(longitude, -110.961095, -110.952424), 
                              true = TRUE, false = FALSE),
         rectangle7 = if_else(condition = between(latitude, 32.286582, 32.291290) &
                                between(longitude, -110.952424, -110.943813), 
                              true = TRUE, false = FALSE), 
         rectangle8 = if_else(condition = between(latitude, 32.279213, 32.306120) &
                                between(longitude, -111.020541, -111.012016), 
                              true = TRUE, false = FALSE),
         rectangle9 = if_else(condition = between(latitude, 32.290661, 32.308577) &
                                between(longitude, -111.026898, -111.020541), 
                              true = TRUE, false = FALSE), 
         rectangle10 = if_else(condition = between(latitude, 32.294224, 32.310832) &
                                between(longitude, -111.032816, -111.026898), 
                              true = TRUE, false = FALSE),
         rectangle11 = if_else(condition = between(latitude, 32.301487, 32.311629) &
                                between(longitude, -111.037383, -111.032816), 
                              true = TRUE, false = FALSE), 
         rectangle12 = if_else(condition = between(latitude, 32.250341, 32.279213) &
                                between(longitude, -110.995910, -110.986553), 
                              true = TRUE, false = FALSE),
         rectangle13 = if_else(condition = between(latitude, 32.257578, 32.279213) &
                                between(longitude, -111.003494, -110.995910), 
                              true = TRUE, false = FALSE), 
         rectangle14 = if_else(condition = between(latitude, 32.264722, 32.279213) &
                                between(longitude, -111.008064, -111.003494), 
                              true = TRUE, false = FALSE),
         rectangle15 = if_else(condition = between(latitude, 32.271996, 32.279213) &
                                between(longitude, -111.012016, -111.008064), 
                              true = TRUE, false = FALSE), 
         rectangle16 = if_else(condition = between(latitude, 32.274288, 32.279213) &
                                 between(longitude, -111.017794, -111.012016), 
                               true = TRUE, false = FALSE),
         rectangle17 = if_else(condition = between(latitude, 32.186822, 32.192389) &
                                 between(longitude, -110.977564, -110.956259), 
                               true = TRUE, false = FALSE)) %>%
  mutate(tucson = rectangle1 | rectangle2 | rectangle3 | 
           rectangle4 | rectangle5 | rectangle6 | 
           rectangle7 | rectangle8 | rectangle9 |
           rectangle10 | rectangle11 | rectangle12 |
           rectangle13 | rectangle14 | rectangle15 |
           rectangle16 | rectangle17)

tucson_spp2 <- tucson_spp2 %>%
  filter(tucson == TRUE) %>%
  select(-rectangle1, -rectangle2, -rectangle3,
         -rectangle4, -rectangle5, -rectangle6, 
         -rectangle7, -rectangle8, -rectangle9,
         -rectangle10, -rectangle11, -rectangle12,
         -rectangle13, -rectangle14, -rectangle15,
         -rectangle16, -rectangle17, -tucson)

# -----------------------------------------------------------------------------------------------

# EDIT 3
# Permutation plot from EDIT 2 moves most permutations to right side (UofA has more species)
# But there is a high number of permutations that are in the -18 region
# That is probably coming from the Tucson Botanical Garden
# So here, we will remove that region

# The Tucson Botanical Garden on Google Maps is a nice rectangle
# So we can just find the coordinates for it and exclude that area
# And then see what that does for us
# Latitude: 32.247916, 32.248955
# Longitude: -110.907607, -110.909718



# -----------------------------------------------------------------------------------------------

# Find boundaries of UofA
# Search "University of Arizona" in iNaturalist
# Select "Download" but only scroll down to lat/long coord under (1) Create a Query
# Copy / paste below: lower lat, lower long, upper lat, upper long
UofA_bound <- c(32.22246534999999, -110.9648986, 32.24394755, -110.9370766)

# Length between boundaries
lat_range <- UofA_bound[3]-UofA_bound[1]
long_range <- UofA_bound[4]-UofA_bound[2]

# Distance from bound to center of bounds
# This will give us the "bounds"
lat_dist <- lat_range/2
long_dist <- long_range/2

# -----------------------------------------------------------------------------------------------

# HOW ARE WE GETTING RANDOM SAMPLES FROM TUCSON?
# First, we want to randomly select a location in Tucson
# This gives us a random data point with: species, latitude, longitude
# as.matrix turns the row into a vector
rand_samp <- as.matrix(sample_n(tucson_spp, 1, fac="latitude"))

# For the following steps to work,
# we need to assign the random sample's latitude and longitude to a variable
# We also need to re-convert the coordinates back to numeric values, so we use as.numeric
center_lat <- as.numeric(rand_samp[2])
center_long <- as.numeric(rand_samp[3])

# Now that we have a random location and know what species was found there,
# we can use that point as a "center"
# and then take all points +/- lat_dist and +/- long_dist away from it
# The idea is to create similar bounds to UofA's area,
# and take data from Tucson that is within those generated bounds based off a "center"

# This one restricts points to those within the latitude bounds
Tucson_sub <- filter(tucson_spp, 
       ((center_lat-lat_dist)<latitude & latitude<(center_lat+lat_dist)))

# This one further restricts the filtered latitude points to those within the longitude bounds as well
Tucson_sub <- filter(Tucson_sub, 
                     ((center_long-long_dist)<longitude & longitude<(center_long+long_dist)))

# -----------------------------------------------------------------------------------------------

# THIS SECTION IS FOR CHECKING PURPOSES:
# We can make a visual for the points to make sure it is working within the bounds
# ggmap requires an API key, so we need to get one first to use it here
# Run this function to register the API key so we can use Google Maps
register_google (key = "AIzaSyD9Sn7gxF4xsmtGLFp90ZSVkM5b4H4TPA0")

# Get the base map
# The center is (longitude, latitude)
base_map <- get_googlemap(center = c(center_long, center_lat), 
                          zoom = 14, 
                          size = c(640, 640), 
                          maptype = "roadmap")

# Make a bounding box for the map
bbx <- c(left = center_long-long_dist,
         bottom = center_lat-lat_dist,
         right = center_long+long_dist,
         top = center_lat+lat_dist)
xs <- c(bbx["left"], bbx["left"], bbx["right"], bbx["right"])
ys <- c(bbx["bottom"], bbx["top"], bbx["top"], bbx["bottom"])
df <- data.frame(xs, ys)

# Plot the sampled points (Tucson_sub) on the map
rand_samp_map <- ggmap(base_map) +
  geom_point(data = Tucson_sub,
             mapping = aes(x = longitude, y = latitude)) +
  geom_polygon(data = df, aes(x = xs, y = ys), size = 1, color = "black", fill = NA) +
  xlab ("Longitude") + 
  ylab ("Latitude")

ggsave ("Rand_samp.png",
        plot = rand_samp_map,
        path = "C:/Users/maxin/OneDrive/Documents/Space Grant Internship/UAhotspot")

# -----------------------------------------------------------------------------------------------

# Make sure bounds are UofA bounds
base_UA <- get_googlemap(center = c((UofA_bound[4]+UofA_bound[2])/2, 
                                    (UofA_bound[3]+UofA_bound[1])/2), 
                         zoom = 14, 
                         size = c(640, 640), 
                         maptype = "roadmap")

bbx2 <- c(left = -110.9648986,
          bottom = 32.22246534999999,
          right = -110.9370766,
          top = 32.24394755)
xs2 <- c(bbx2["left"], bbx2["left"], bbx2["right"], bbx2["right"])
ys2 <- c(bbx2["bottom"], bbx2["top"], bbx2["top"], bbx2["bottom"])
df2 <- data.frame(xs2, ys2)

UA_samp_map <- ggmap(base_UA) +
  geom_point(data = UofA_speciescoord,
             mapping = aes(x = longitude, y = latitude)) +
  geom_polygon(data = df2, aes(x = xs2, y = ys2), size = 1, color = "black", fill = NA) +
  xlab ("Longitude") + 
  ylab ("Latitude")

ggsave ("UA_samp.png",
        plot = UA_samp_map,
        path = "C:/Users/maxin/OneDrive/Documents/Space Grant Internship/UAhotspot")

# -----------------------------------------------------------------------------------------------
  
# PERMUTATION TEST:
  
# So, we have our random sample function from Tucson
# Random samples that are within the bounding box of the exact UA area
# After plotting tests on ggmap, I know it is working correctly (yay!)
# Now we can get back to the question:
# Does UofA have more species than any random sample in Tucson?
  
# We need to make this so that the loop:
# 1) Runs the random center picker
# 2) Filters the sample so it is within bounds
# 3) Outputs Tucson_sub data (our filtered sample)
# 4) Subtracts unique Tucson_sub species from unique UA species
# 5) Places results in a list
  
# How many times do we want to run the test?
num_reps <- 1000

# Making an empty list to keep the results we get from running the tests
# numeric() takes num_reps and creates that many elements in the list/vector
# All the elements will be 0 for now
rich_diff <- numeric(num_reps)

# For purposes during the tests, we will make a table isolating the species for UofA
# (We are just removing the latitude and longitude)
UofA_species <- UofA_speciescoord %>% 
  select(scientific_name)

# Running the tests
# This is the same code used above, but in a loop
# For the last line, we are using nrow to count the number of rows with unique species
# This is because the data is arranged in a column, 
# so we need to count the rows the unique species are stored in
# And then each result from the last line will be stored in rich_diff
for(i in 1:num_reps) {
  rand_samp <- as.matrix(sample_n(tucson_spp2, 1, fac="latitude"))
  center_lat <- as.numeric(rand_samp[2])
  center_long <- as.numeric(rand_samp[3])
  Tucson_sub <- filter(tucson_spp2, 
                       ((center_lat-lat_dist)<latitude & latitude<(center_lat+lat_dist)))
  Tucson_sub <- filter(Tucson_sub, 
                       ((center_long-long_dist)<longitude & longitude<(center_long+long_dist)))
  Tucson_sub_species <- Tucson_sub %>% 
    select(scientific_name)
  rich_diff[i] <- nrow(unique(UofA_species)) - nrow(unique(Tucson_sub_species))
}

# -----------------------------------------------------------------------------------------------

# How many of those show that UofA has a greater species richness than Tucson subsample?
# If the values are positive, UofA > Tucson subsample in terms of richness
# Need to run these lines individually -- idk what's going on here
greater <- sum(rich_diff > 0)

# What is the probability that a random sample from another random distribution
# shows that UofA has more species than Tucson?
# What if the UofA IS a hotspot? 
prob_hotspot <- greater / num_reps

# What is the probability that the UofA is NOT a hotspot?
p_value <- 1 - prob_hotspot

# Plotting this on a graph
# We need to put the values into a table to make plotting it easier
# Number of permutations in one column, and results in rich_diff go in another
rich_df <- data.frame(permutation = 1:num_reps,
                      delta = rich_diff)

# -----------------------------------------------------------------------------------------------

# The null hypothesis: mean difference between UofA and Tucson richness is 0
delta_plot <- ggplot(data = rich_df, mapping = aes(x = delta)) +
  geom_histogram(bins = 25, color = "black", fill = "grey", size = 1) +
  geom_vline(xintercept = 0, lty = 2, size = 1) + # dashed line for null hypothesis
  xlab(label = "# Species at UArizona - # Species at Tucson") +
  ylab(label = "# Permutations") +
  theme_bw()

# ALTERNATIVE WITH DENSITY PLOT
# delta_plot <- ggplot(data = rich_df, mapping = aes(x = delta)) +
#   geom_histogram(aes(y = ..density..), bins = 25, color = "black", fill = "grey", size = 1) +
#   geom_density() +
#   geom_vline(xintercept = 0, lty = 2, size = 1) + # dashed line for null hypothesis
#   xlab(label = "# Species at UArizona - # Species at Tucson") +
#   ylab(label = "# Permutations") +
#   theme_bw()

# View the plot
delta_plot

# And now we can save the plot :)
ggsave(filename = "perm_plot_controlled_area_3.jpg", plot = delta_plot)

