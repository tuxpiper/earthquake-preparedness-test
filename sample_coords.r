#!/usr/bin/env Rscript

require('rgdal')
require('plyr')

random_points <- function(geo_id, n) {
  if (geo_id == "0") {   # all Oregon
    poly <- or_area
    n <- n * 1.20   # Increase N by 20% to account for overlap with populated areas
  } else {
    poly <- pop_areas[ pop_areas@data$GEOID10 == geo_id ,]
  }
  coordinates(spsample(poly, n, "random"))
}

pop_areas <- readOGR(".", "oregonPopulatedAreas")
or_area <- readOGR(".", "oregon")
pop_census_or <- 3831074
outstanding_census <- pop_census_or - sum(pop_areas$POP)

# Two lists used for weighted sampling of batches over the different distinct geographical areas
# geo_ids contains ids of geographical areas
# pops contains population for each area (used for weighting)
geo_ids <- as.character(pop_areas$GEOID10)
geo_ids[[length(geo_ids)+1]] <- 0                   # Append shape of all oregon
pops <- as.integer(pop_areas$POP)
pops[[length(pops)+1]] <- pop_census_or - sum(pops) # Assign weight with remaining population

# In batches of batch_size up to total rows
batch_size <- 250000
total <- 10000000
for (n in 1:(total/batch_size)) {
  # Calculate how many samples we have to generate for each polygon area
  samples_per_poly <- count(sample(geo_ids, batch_size, replace=TRUE, prob= pops))
  # Generate samples and merge into a single dataframe
  rez <- apply(samples_per_poly, 1, function(x) random_points(as.character(x[1]), as.integer(x[2])))
  rez <- lapply(rez, function(l) data.frame(l))
  rez <- Reduce(function(...) merge(..., all=T), rez)
  # Shuffle the dataframe by rows and save
  rez1 <- rez[sample(nrow(rez)),]
  rownames(rez1) <- NULL
  write.table(rez1, "coords.csv", col.names= FALSE, row.names= FALSE, append = TRUE)
}
