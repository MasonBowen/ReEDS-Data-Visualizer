# This is to read in all data that the user might feasibly want to manually change
# saving these data in csv's allows non r users to also be able to change them relatively easily

library(data.table)


# Year Data: csv with years in model, single column
years <- fread(file.path(basicdata.directory, "year_set.csv"), sep = ",", header = FALSE, col.names = "Year")

# Timslice data: csv with timeslice, season and time of day information, 3 columns
timeslice <- fread(file.path(basicdata.directory, "hour_szn_timeslice.csv"), sep = ",", header = TRUE)

# Technology Data: csv with technology types (full, simplified, thermal/renewable, vre) and color for simplified, 6 columns
technology <- fread(file.path(basicdata.directory, "gen_tech_set.csv"), sep = ",", header = TRUE)[,1:4]

# Country region data:
state_data <- fread(file.path(basicdata.directory, "country_region_state.csv"), header = TRUE)

# Category Order for plotting
category.order <- fread(file.path(basicdata.directory, "category_order.csv"), sep = ",", header = TRUE)
category.order <- as.data.table(apply(category.order, MARGIN = c(1,2), function(x){if(x == ""){return(NA)}else{return(x)}}))

# Category Color for plotting
category.color <- fread(file.path(basicdata.directory, "gen_tech_set.csv"), header = TRUE)


# ADDITIONAL FILES AVAILABLE CONCERNING THE RESOURCE CURVE DATA, NOT SURE IF WE NEED THEM OR NOT ...