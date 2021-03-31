# Proportion of Tucson species seen at UA sites
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-03-30

# Create a Venn diagram for (1) number of species seen at sites on the campus 
# of the University of Arizona and (2) numer of species seen anywhere in Tucson
# The latter should be entirely contained within the former

# Package for Venn diagrams
library(VennDiagram)

# Create a sample of data of "species" seen in Tucson; for these mock data we 
# are using the set of lower-case letters
Tucson <- sample(x = letters, replace = TRUE, size = 500)
# To make the UA sample we now sample from that Tucson sample
UArizona <- sample(x = tucson, replace = TRUE, size = 50)

# Bundle the two together in a list
mock_data <- list(UArizona = UArizona,
                  Tucson = Tucson)

# Make the Venn diagram object
# This will require some tweaking to get numbers and labels to show up in 
# the right places
vd <- venn.diagram(x = mock_data, 
                   filename = NULL,      # print to the screen
                   offset = 1,           # center align circles
                   rotation.degree = 90) # bottom justify circles
# Clear out the drawing area
grid.newpage()
# Draw the Venn diagram
grid.draw(vd)
