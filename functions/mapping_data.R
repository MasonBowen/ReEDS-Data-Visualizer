# This will load in the shape file and format

# Load file
country.shape <- readOGR(shape.file.loc)

# remove islands
country.shape <- country.shape[!(country.shape$NAME_1corr %in% c("Andaman_Nicobar", "Lakshadweep")),]

# format
names(country.shape@polygons) <- country.shape$NAME_1corr
country.shape@data$id = rownames(country.shape@data)
