# This is an attempt to read in all of the basic information 

# First, keep a list of the data in which we are interested, this will likely grow
source(file.path(functions.directory, "create_gdx_str_list.R"))

# # Second keep a list of all scenarios available
# scenario.dir <- "../capexp-india/ReEDS-2.0/gdxfiles"
# scenarios <- gsub(list.files(scenario.dir, pattern = ".gdx"), pattern = ".gdx$", replacement = "", perl = TRUE)

# Third, load all relevant gdx_functions
source(file.path(functions.directory, "gdxr_functions.R"))

# Start gdx functions and files
source(file.path(functions.directory, "start_gdxr.R"))


# Write a function to change column types into appropriate types
change_col_types <- function(dt, gdx.str = gdx.structure){
  
  for(i in 1:length(colnames(dt))){
    
    col.dt = colnames(dt)[i]
    
    if(col.dt %in% c("Year", "Value")){
      dt[[col.dt]] <- as.numeric(as.character(dt[[col.dt]]))
    }
    
    if(col.dt %in% c("NoIDEA", "NoIDEA2", "Technology", "State", "State.1", "State.2", 
                     "Time.Slice", "Resource.Curve.Bin", "Transmission.Type", 
                     "Operating.Reserve.Type")){
      dt[[col.dt]] <- as.character(dt[[col.dt]])
    }
    
    if(col.dt %in% as.character(unlist(lapply(gdx.str, function(gdx){return(gdx[["obj.name"]])})))){
      dt[[col.dt]] <- as.numeric(as.character(dt[[col.dt]]))
    }
  }

  return(dt)
  
  
}

# This function will subselect a single variable from the larger'gdx_all_var' LIST (Should be a list)
# list and organize them into a single data.table with a new column for scenarios
gdx_single_var <- function(gdx_list, gdx_var){
  
  # Test to see if variable in data table
  if(!(gdx_var %in% unique(as.character(unlist(lapply(gdx_list, names)))))){stop("'gdx_var' not in data!!!")}

  # lapply across scenarios, reshape into single large data.table with column representing scenarios
  rt_val <- rbindlist(lapply(names(gdx_list), function(scenario, gdxl = gdx_list, gdxv = gdx_var){
    
    dt_sing_var <- gdxl[[scenario]][[gdxv]]
    dt_sing_var$Scenario <- scenario
    
    return(dt_sing_var)
    
  }))
  
  
  return(rt_val)
  
}

