# All of these are for formatting data based on parameters expressed in the 'simplify_list' input

# This function is for variables such as capacity and generation that have 1 state and a technology type and timeslice/year column
plot_data_simplification <- function(dataset, gdx_str_match, gdx_var, agg_type = "sum", simplify_list, year_vs_ts = "year"){
  
  # INCAPABLE OF SIMPLIFYING TECHNOLOGY FOR VARIABLES WITH 'STATE.1' AND 'STATE.2'

  # Create copy of data to prevent overwritting original 
  ds <- copy(dataset)
  var_y <- gdx_str_match[[gdx_var]][["obj.name"]]
  
  # Warnings
  if(!(agg_type %fin% c("sum", "mean"))){stop("'agg_type' must be either 'sum' or 'mean'")}
  if(!("Technology" %fin% colnames(ds))){stop("Cannot simplify technologies as there is no column for Technology ...")}
  if(!("State" %fin% colnames(ds))){
    stop("Cannot simplify states as there is no column for states ...")}
  if(any(c("State.1", "State.2") %fin% colnames(ds))){
    stop("Code cannot currently support variables with multiple state inputs while reducing technology types (selects for 'State', not 'State.1'")}
  if(!(year_vs_ts %fin% c("year", "ts"))){stop("'year_vs_ts' must be either 'year' (for data aggregated annually) or'ts' (for data aggregated on a timeslice basis")}
  if(!("Time.Slice" %fin% colnames(ds)) && year_vs_ts == "ts"){
    stop("Time.Slice is not in the data.table, cannot aggregate by Time.Slice")}
  
  # Year Simplification -- Must be first (in other iterations) so that years can be excluded even when simplifying by time.slice
  ds <- ds[Year %fin% simplify_list[["Year.V"]],,]
  
  # Time Slice conversion
  if(year_vs_ts == "ts"){
    ds[["Time.Slice"]] <- mgsub(pattern = simplify_list[["TimeSlice.Re"]][["pattern"]], 
                                replacement = simplify_list[["TimeSlice.Re"]][["replacement"]], 
                                text.var = ds[["Time.Slice"]], fixed = TRUE, order.pattern = TRUE)
  }
  
  # Technology Simplifications
  # Simplify by year or by timeslice?
  if(year_vs_ts == "year"){
    by_vec_tech <- c("Year", "State", "Technology", "Scenario")
  }else{
    by_vec_tech <- c("Time.Slice", "State", "Technology", "Scenario")
  }
  
  # replace technology types and aggregate by vector
  ds[["Technology"]] <-  mgsub(pattern = simplify_list[["Tech.Re"]][["pattern"]], 
                               replacement = simplify_list[["Tech.Re"]][["replacement"]], 
                               text.var = ds[["Technology"]], fixed = TRUE, order.pattern = TRUE)
  
  # So far 'mean' is never used, but could be a parameter in the future
  if(agg_type == "sum"){
    
    ds <- ds[, .(Agg_Temp = unlist(lapply(.SD, sum, na.rm = TRUE))), .SDcols = c(var_y), by = by_vec_tech]
    
  }else{
    
    ds <- ds[, .(Agg_Temp = unlist(lapply(.SD, mean, na.rm = TRUE))), .SDcols = c(var_y), by = by_vec_tech]
    
  }
  
  setnames(ds, old = "Agg_Temp", new = var_y)
  
  # State simplification
  # Simplify by year or by timeslice?
  if(year_vs_ts == "year"){
    by_vec_state <- c("Year", "Technology", "State", "Scenario")
  }else{
    by_vec_state <- c("Time.Slice", "Technology", "State", "Scenario")
  }
  
  # replace states and aggregate by vector
  ds[["State"]] <-  mgsub(pattern = simplify_list[["State.Re"]][["pattern"]], 
                          replacement = simplify_list[["State.Re"]][["replacement"]], 
                          text.var = ds[["State"]], fixed = TRUE, order.pattern = TRUE)
  
  # So far 'mean' is never used, but could be a parameter in the future
  if(agg_type == "sum"){
    
    ds <- ds[, .(Agg_Temp = unlist(lapply(.SD, sum, na.rm = TRUE))), .SDcols = c(var_y), by = by_vec_state]
    
  }else{
    
    ds <- ds[, .(Agg_Temp = unlist(lapply(.SD, mean, na.rm = TRUE))), .SDcols = c(var_y), by = by_vec_state]
    
  }
  
  setnames(ds, old = "Agg_Temp", new = var_y)
  
  return(ds)
}

# This is used when comparing across scenarios (taking the difference) (currently only for 'capacity' and generation)
plot_data_comparison_config <- function(dataset, gdx_str_match, gdx_var, scenario_baseline, scenario_names, comp_type = "absolute"){
  
  # Warnings
  if("BASELINESCENARIO" %in% scenario_names){stop("'BASELINESCENARIO' cannot be a scenario name")}
  if(!(comp_type %in% c("absolute", "percent"))){stop("'comp_type' must be either 'absolute' or 'percent'")}
  
  # copy and reshape so that scenarios are columns
  ds <- copy(dataset)
  dcast_form <- paste0(paste0(colnames(ds)[colnames(ds) %in% gdx_str_match[[gdx_var]][["col.new"]]], collapse = "+"), "~Scenario")
  ds_temp <- dcast(ds, formula = dcast_form, value.var = gdx_str_match[[gdx_var]][["obj.name"]], fill = 0)
  
  setnames(ds_temp, old = scenario_baseline, new = "BASELINESCENARIO")
  
  # currently only absolute used but could be different in future
  ds_temp_1 <- ds_temp[,colnames(ds)[colnames(ds) %in% gdx_str_match[[gdx_var]][["col.new"]]], with = FALSE]
  
  if(comp_type == "absolute"){
    
    ds_temp_2 <- ds_temp[,  .SD - BASELINESCENARIO, .SDcols = c(setdiff(scenario_names, scenario_baseline))]
    
  }
  
  if(comp_type == "percent"){
    
    ds_temp_2 <- ds_temp[, (.SD - BASELINESCENARIO)/BASELINESCENARIO, .SDcols = c(setdiff(scenario_names, scenario_baseline))]
    
  }
  
  
  ds_temp <- cbind(ds_temp_1, ds_temp_2)
  
  # reshape after taking difference across scenarios
  ds_temp <- melt(ds_temp, id.vars = colnames(ds_temp)[colnames(ds_temp) %in% gdx_str_match[[gdx_var]][["col.new"]]], 
                  measure.vars = setdiff(scenario_names, scenario_baseline), 
                  variable.name = "Scenario", value.name = gdx_str_match[[gdx_var]][["obj.name"]])
  
  return(ds_temp)
  
}


# Function looks at year over year change for any variable (made for looking at new capacity)
# 'ignore.retire' allows you to overwrite all negative year over year changes with 0, thus supressing them in plotting
#       currently hardcoded in teh capacity server file
find_yoy_change <- function(dataset, gdx_var, gdx_str_match = gdx.structure, ignore.retire = TRUE){
  
  col_pres <- c(gdx_str_match[[gdx_var]][["col.new"]][gdx_str_match[[gdx_var]][["col.new"]] %fin% colnames(dataset)], "Scenario")
  col_pres <- grep(col_pres, pattern = "Year", fixed = TRUE, invert = TRUE, value = TRUE)
  
  ds <- copy(dataset)
  
  ds[ , "PLACEHOLDER" := get(gdx_str_match[[gdx_var]][["obj.name"]]) - data.table::shift(get(gdx_str_match[[gdx_var]][["obj.name"]]), fill = get(gdx_str_match[[gdx_var]][["obj.name"]])[1], type = "lag"), by = col_pres]  
  ds[, gdx_str_match[[gdx_var]][["obj.name"]] := NULL,]
  
  setnames(ds, old = "PLACEHOLDER", new = gdx_str_match[[gdx_var]][["obj.name"]])
  
  if(ignore.retire){
    ds[get(gdx_str_match[[gdx_var]][["obj.name"]]) < 0, gdx_str_match[[gdx_var]][["obj.name"]] := 0,]
  }
  
  return(ds)
  
}


# for making maps of data of capacity (or generation, any dataset with only 1 state column and atechnology column)
cap_map_data_prep <- function(dataset, shp.file, gdx_str_match, gdx_var, simplify_list, year.focus, tech.focus, region_aggr){
  
  # region_aggr allows for states to be dissolved into regions (also must be held across the plotting functions)
  if(region_aggr){
    
    # Generate IDs for grouping
    shp.file.id.reg <- qdap::mgsub(text.var = shp.file@data$NAME_1corr, pattern = simplify_list[["State.Re"]][["pattern"]], 
                                   replacement = simplify_list[["State.Re"]][["replacement"]],fixed = TRUE)
    shp.file.id.reg <- factor(shp.file.id.reg, levels = unique(simplify_list[["State.Re"]][["replacement"]]))
    
    # Merge polygons by ID
    shp.file.union <- unionSpatialPolygons(shp.file, shp.file.id.reg)
    # simplify
    shp.file <- gSimplify(shp.file.union, tol=.005, topologyPreserve=TRUE)
    
    shp.df = fortify(shp.file)
    shp.df = as.data.table(shp.df) 
    
    shp.df$NAME_1corr <- shp.df$id
    
  }else{
    shp.file.simple <- gSimplify(shp.file, tol=.05, topologyPreserve=TRUE)
    shp.file.poly <- SpatialPolygonsDataFrame(Sr = shp.file.simple, data = shp.file@data, match.ID = TRUE)
    
    # keep data from shapefile and convert into object that ggplot can use
    shp.file.poly@data$id = rownames(shp.file.poly@data)
    #country.poly@data = join(country.poly@data, data, by="ID_1")
    shp.df = fortify(shp.file.poly)
    shp.df = plyr::join(shp.df, shp.file.poly@data, by="id")
    
    shp.df = as.data.table(shp.df)
    
    
  }
  
  # keep coordinates if necessary
  shp.coords <- coordinates(shp.file)
  
  
  # get names of regions in shape file
  shp.regions = unique(as.character(shp.df$NAME_1corr))
  
  # subselect data to focus on year chosen as func. input (should be able to handle single value or multiple and facet accordingly)
  dataset = dataset[Year %fin% year.focus,,]
  
  # subselect data to focus on tech chosen as func. input
  dataset = dataset[Technology %fin% tech.focus,,]
  
  # Expand data to create all possible matches so that data can be properly mapped
  dataset.empty <- as.data.table(expand.grid("Technology" = tech.focus,
                                             "State" = unique(simplify_list[["State.Re"]][["replacement"]]),
                                             "Year" = year.focus,
                                             "Scenario" = unique(dataset[["Scenario"]])))
  
  dataset.empty[, gdx_str_match[[gdx_var]][["obj.name"]] := 0,]
  
  # Merge actual data with simulated empty data
  dataset.complete <- merge(dataset, dataset.empty, by = c("Technology", "State", "Year", "Scenario"), all = TRUE)
  dataset.complete[is.na(get(paste0(gdx_str_match[[gdx_var]][["obj.name"]], ".x"))), paste0(gdx_str_match[[gdx_var]][["obj.name"]], ".x") := 0,]
  setnames(dataset.complete, old = c(paste0(gdx_str_match[[gdx_var]][["obj.name"]], ".x"), paste0(gdx_str_match[[gdx_var]][["obj.name"]], ".y")), new = c(gdx_str_match[[gdx_var]][["obj.name"]], "TEMP"))
  dataset.complete[, TEMP := NULL]
  
  # merge complete dataset with country data
  shp.df = merge(shp.df, dataset.complete, 
                 by.x = "NAME_1corr", by.y = "State", all.x = T,
                 allow.cartesian = T)
  
  # change all 0's to null to allow blotting out of states
  shp.df[[gdx_str_match[[gdx_var]][["obj.name"]]]][shp.df[[gdx_str_match[[gdx_var]][["obj.name"]]]] == 0] <- NA
  
  if(region_aggr){setnames(shp.df, "NAME_1corr", "State")}
  
  return(shp.df)
  
}

# simplifying data for datasets in which there are 2 state columns and no technology categories/columns
#     Currently only looking at year data, not aggregating for timeslices
plot_data_simplification_flow_trans <- function(dataset, gdx_str_match, gdx_var, agg_type = "sum", simplify_list, year_vs_ts = "year"){
  
  # Create copy of data to prevent overwritting original 
  ds <- copy(dataset)
  var_y <- gdx_str_match[[gdx_var]][["obj.name"]]
  if(var_y == "totflow"){var_y = "FLOW"}
  
  # Warnings
  if(!(agg_type %fin% c("sum", "mean"))){stop("'agg_type' must be either 'sum' or 'mean'")}
  if(!all(c("State.1", "State.2") %fin% colnames(ds))){
    stop("Cannot simplify states as there is no column for 'State.1' or 'State.2'")}
  if(!(year_vs_ts %fin% c("year", "ts"))){stop("'year_vs_ts' must be either 'year' (for data aggregated annually) or'ts' (for data aggregated on a timeslice basis")}
  if(!("Time.Slice" %fin% colnames(ds)) && year_vs_ts == "ts"){
    stop("Time.Slice is not in the data.table, cannot aggregate by Time.Slice")}
  
  # Year Simplification -- Must be first (in other iterations) so that years can be excluded even when simplifying by time.slice
  ds <- ds[Year %fin% simplify_list[["Year.V"]],,]
  
  # Time Slice conversion
  if(year_vs_ts == "ts"){
    ds[["Time.Slice"]] <- mgsub(pattern = simplify_list[["TimeSlice.Re"]][["pattern"]], 
                                replacement = simplify_list[["TimeSlice.Re"]][["replacement"]], 
                                text.var = ds[["Time.Slice"]], fixed = TRUE, order.pattern = TRUE)
  }
  
  
  # State simplification
  # Simplify by year or by timeslice?
  if(year_vs_ts == "year"){
    by_vec_state <- c("Year", "State.1", "State.2", "Scenario")
  }else{
    by_vec_state <- c("Time.Slice", "State.1", "State.2", "Scenario")
  }
  
  # replace states and aggregate by vector
  ds[["State.1"]] <-  mgsub(pattern = simplify_list[["State.Re"]][["pattern"]], 
                            replacement = simplify_list[["State.Re"]][["replacement"]], 
                            text.var = ds[["State.1"]], fixed = TRUE, order.pattern = TRUE)
  
  # replace states and aggregate by vector
  ds[["State.2"]] <-  mgsub(pattern = simplify_list[["State.Re"]][["pattern"]], 
                            replacement = simplify_list[["State.Re"]][["replacement"]], 
                            text.var = ds[["State.2"]], fixed = TRUE, order.pattern = TRUE)
  
  if(agg_type == "sum"){
    
    ds <- ds[, .(Agg_Temp = unlist(lapply(.SD, sum, na.rm = TRUE))), .SDcols = c(var_y), by = by_vec_state]
    
  }else{
    
    ds <- ds[, .(Agg_Temp = unlist(lapply(.SD, mean, na.rm = TRUE))), .SDcols = c(var_y), by = by_vec_state]
    
  }
  
  setnames(ds, old = "Agg_Temp", new = var_y)
  
  return(ds)
}

# This function simplifies data (which was previously simplified by 'plot_data_simplification_flow_trans()') so that only the
#     net power flow for each region-pair/year/scenario is returned
plot_data_prep_flow <- function(dataset, gdx_str_match, gdx_var, simplify_list, region_aggr, year.focus = c(2017, 2047)){
  
  ds <- copy(dataset)
  
  setnames(ds, c("State.1", "State.2"), c("State.Orig", "State.Dest"))
  
  # Focus on specific years
  ds <- ds[Year %fin% year.focus,,]
  
  
  # Now for flow - create dataframe of all possible combos (prevents empty reions on maps)
  empty <- copy(ds)
  empty[, `:=`(Year = NULL, Scenario = NULL, FLOW = NULL),]
  empty <- unique(empty)
  
  empty <- rbindlist(lapply(unique(ds[["Scenario"]]), function(scen, eF. = empty){
    
    eF.yr <- rbindlist(lapply(simplify_list[["Year.V"]][simplify_list[["Year.V"]] %fin% year.focus], function(yr, eF = eF.){
      
      return(copy(eF[, "Year" := yr]))
      
    }))
    
    return(copy(eF.yr[, "Scenario" := scen,]))
    
  }))
  
  empty[,`:=`(FLOW = 0),]
  
  # merge actual data with all possible data
  ds.final <- merge(ds, empty, by = c("State.Orig", "State.Dest", "Year", "Scenario"), all.y = TRUE, allow.cartesian=TRUE)
  ds.final[is.na(FLOW.x), FLOW.x := 0,]
  ds.final[, `:=` (FLOW.y = NULL),]
  setnames(ds.final, "FLOW.x", "FLOW")
  
  # this copies the data set (while reversing flow and making it negative) and combines and 
  #     sums across all region-pairs/year/scenario combinations, reversing the direction of 
  #     the flow so that all flow represented as positive
  ds.final <- ds.final[State.Orig != State.Dest, .(FLOW = sum(FLOW)), by = .(State.Orig, State.Dest, Year, Scenario)]
  ds.final <- rbindlist(list(ds.final, ds.final[,.(State.Orig = State.Dest, State.Dest = State.Orig, Year, Scenario, FLOW = -FLOW)]),
                        use.names = TRUE, fill = TRUE)
  ds.final <- ds.final[,.(FLOW = sum(FLOW)), by = .(State.Orig, State.Dest, Year, Scenario)]
  ds.final <- ds.final[FLOW > 0,,]
  
  return(ds.final)
  
}
