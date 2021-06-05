# Maxine Cruz
# tmcruz@email.arizona.edu
# April 5, 2021


# Venn diagram of UArizona species vs. Tucson species
# Larger circle would be number of pollinator species in Tucson
# Circle of how many of these species are at UArizona on the inside

# Package for Venn diagrams
# Before running library, run: 
# install.packages("VennDiagram") 
library(VennDiagram)

# We will also need tidyverse to get number of species from Tucson and UArizona
library(tidyverse)

# Starting data: Tucson_MARCH and UofA_MARCH
# Data already has desired pollinator families isolated
# (Apidae, Hesperiidae, Lycaenidae, Nymphalidae, Papillionidae, Pieridae, Riodinidae)

Tucson_MARCH <- read.csv(file = "HymLep_only_Data/Tucson_MARCH.csv")
UofA_MARCH <- read.csv(file = "HymLep_only_Data/UofA_MARCH.csv")

# What species are present in each dataset?
# We can take the unique species names to find out
Tucson_species_present <- Tucson_MARCH %>% 
  select(scientific_name) %>%
  unique()
UofA_species_present <- UofA_MARCH %>% 
  select(scientific_name) %>%
  unique()

# Save the new unique species-only tables
# Puts them in the GitHub folders
write.csv(Tucson_species_present, 
          file = "Worked_data/Tucson_species_present.csv")
write.csv(UofA_species_present, 
          file = "Worked_data/UofA_species_present.csv")

# Aside from venn diagram:
# Percentage of species UArizona holds
# Running this comes out to: 31.03%
(count(UofA_species_present)/count(Tucson_species_present))*100

# Back to venn diagram
# Bundle the two together in a list
# "pull" turns the table column into a vector so we can use it in "venn.diagram"
venn_data <- list(UArizona = pull(UofA_species_present),
                  Tucson = pull(Tucson_species_present))

# Make the venn diagram
final_venn <- venn.diagram(x = venn_data, # What data we are using
                           filename = NULL, # Prints diagram to screen
                           offset = 1, # Moves UArizona circle to touch left edge of Tucson circle
                           rotation.degree = 90, # Now moves UArizona circle to base of Tucson circle
                           category.names = c("UArizona", "Tucson"), # So we can assign different colors to each
                           fill = c("grey", "white"),
                           fontfamily = "sans",
                           cex = 1.5,
                           cat.fontfamily = "sans",
                           cat.fontface = "bold",
                           cat.cex = 2,
                           cat.pos = 0,
                           cat.dis = c(-0.09, -0.07))

# Clear out the drawing area
# Clears out the Plots screen
grid.newpage()

# Saving the venn diagram in a new file
png(filename = "final_venn.png", 
    width = 500, 
    height = 500,
    units = "px")

# Draw the venn diagram
# Might want to full screen Plots to get actual circles
grid.draw(final_venn)

# Close file
dev.off()
