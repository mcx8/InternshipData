# Maxine Cruz
# tmcruz@email.arizona.edu
# May 14, 2021

# Where are most of Tucson's observations?
# Make a map with observation density

# At the bottom is data restricted to urban/suburban Tucson
# And then replotting a map to make sure the function worked
# Before using it in UA_Tucson_perm_test_using_area

# -----------------------------------------------------------------------------------------------
  
library(tidyverse)
library(dplyr)
library(ggmap)
library(ggplot2)

# -----------------------------------------------------------------------------------------------

# The data we are using here
Tucson_MARCH <- read.csv(file = "HymLep_only_Data/Tucson_MARCH.csv")

# ggmap requires an API key, so we need to get one first to use it here
# Run this function to register the API key so we can use Google Maps
register_google (key = "key")


# First, get the base map
# The center is (longitude, latitude)
# I googled Tucson's coordinates
# This method allows us to crop the map
base_map <- get_map(location = c(-110.9747, 32.2226),
                    source = "google",
                    maptype = "terrain")

# Now, we can plot our points
Tucson_map <- ggmap(base_map) +
  scale_y_continuous(limits = c(32.05, 32.4)) +
  scale_x_continuous(limits = c(-111.3, -110.6)) +
  stat_density2d(aes(x = longitude, y = latitude, 
                     fill = ..level..,
                     alpha = ..level..),
                 bins = 100, 
                 data = Tucson_MARCH,
                 geom = "polygon") +
  geom_point(data = Tucson_MARCH,
             mapping = aes(x = longitude, y = latitude),
             size = 1) +
  theme(legend.position = "none") +
  xlab ("Longitude") + 
  ylab ("Latitude")

# Save our map!
ggsave ("Tucson_map.png",
        plot = Tucson_map,
        path = "C:/Users/maxin/OneDrive/Documents/Space Grant Internship/UAhotspot")

# -----------------------------------------------------------------------------------------------
  
# Alternative map to see that the bounds are only in urban/suburban area

# Borrowed code from Dr. Prudic
# This makes two rectangles that cover the urban/suburban area of Tucson
# If the data point falls within the rectangle, it is assigned to TRUE
# If not, it is FALSE
# Then it only takes points within those rectangles (TRUE)
# And makes a new table with only the points from these two rectangles
tucson_c <- Tucson_MARCH %>% 
  mutate(rectangle1 = if_else(condition = between(latitude, 32.209744, 32.351802) &
                                between(longitude, -111.046889, -110.962849), 
                              true = TRUE, false = FALSE), 
         rectangle2 = if_else(condition = between(latitude, 32.209744, 32.260557) &
                                between(longitude, -110.962849, -110.841198), 
                              true = TRUE, false = FALSE)) %>%
  mutate(tucson = rectangle1 | rectangle2)

# I think this filters the data to those that are only TRUE
# Then the second part with the minus signs does something so that the data
# is no longer in terms of TRUE/FALSE
# (If you run this section without the minus signs and view the tucson_c table, you'll see)
tucson_c <- tucson_c %>%
  filter(tucson == TRUE) %>%
  select(-rectangle1, -rectangle2, -tucson)

# Now we get the base map of Tucson again
base_map <- get_map(location = c(-110.9747, 32.2226),
                    source = "google",
                    maptype = "terrain")

# And plot the points to make sure we got it right
Tucson_map2 <- ggmap(base_map) +
  scale_y_continuous(limits = c(32.18, 32.38)) +
  scale_x_continuous(limits = c(-111.1, -110.8)) +
  stat_density2d(aes(x = longitude, y = latitude, 
                     fill = ..level..,
                     alpha = ..level..),
                 bins = 100, 
                 data = tucson_c,
                 geom = "polygon") +
  geom_point(data = tucson_c,
             mapping = aes(x = longitude, y = latitude),
             size = 1) +
  theme(legend.position = "none") +
  xlab ("Longitude") + 
  ylab ("Latitude")

# Save our map!
ggsave ("Tucson_map_2.png",
        plot = Tucson_map2,
        path = "C:/Users/maxin/OneDrive/Documents/Space Grant Internship/UAhotspot")

Tucson_map3 <- ggmap(base_map) +
  scale_y_continuous(limits = c(32.18, 32.38)) +
  scale_x_continuous(limits = c(-111.1, -110.8)) +
  geom_point(data = tucson_c,
             mapping = aes(x = longitude, y = latitude),
             size = 1) +
  theme(legend.position = "none") +
  xlab ("Longitude") + 
  ylab ("Latitude")

# -----------------------------------------------------------------------------------------------

# Alternative map (part 2) to see that the bounds are only in primarily urban area

# Modified code from Dr. Prudic
# This makes two rectangles that cover most of the urban area of Tucson
tucson_c2 <- Tucson_MARCH %>% 
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

tucson_c2 <- tucson_c2 %>%
  filter(tucson == TRUE) %>%
  select(-rectangle1, -rectangle2, -rectangle3,
         -rectangle4, -rectangle5, -rectangle6, 
         -rectangle7, -rectangle8, -rectangle9,
         -rectangle10, -rectangle11, -rectangle12,
         -rectangle13, -rectangle14, -rectangle15,
         -rectangle16, -rectangle17, -tucson)

base_map <- get_map(location = c(-110.9747, 32.2226),
                    source = "google",
                    maptype = "terrain")

Tucson_map3 <- ggmap(base_map) +
  scale_y_continuous(limits = c(32.17, 32.32)) +
  scale_x_continuous(limits = c(-111.1, -110.8)) +
  stat_density2d(aes(x = longitude, y = latitude, 
                     fill = ..level..,
                     alpha = ..level..),
                 bins = 100, 
                 data = tucson_c2,
                 geom = "polygon") +
  geom_point(data = tucson_c2,
             mapping = aes(x = longitude, y = latitude),
             size = 1) +
  theme(legend.position = "none") +
  xlab ("Longitude") + 
  ylab ("Latitude")

ggsave ("Tucson_map_3.png",
        plot = Tucson_map3,
        path = "C:/Users/maxin/OneDrive/Documents/Space Grant Internship/UAhotspot")


