# Third write a function which can extract ALL of the information desired above in gdx.structure
gdx_extract_all <- function(scenarios, variable_list = gdx.structure, scen.dir = data.folder, gms.dir = gams.directory, parallel = FALSE){
  
# Extract all variable according to list in the 'variable_list' input
  
  # If running in parallel
  if(parallel){
    
    require(parallel)
    # for parallel operation
    cl <- makeCluster(detectCores()-1)
    
    # following exports certain scripts and variale to the clusters (which cannot see the global environment)
    clusterEvalQ(cl, {
      ## set up each worker.  Could also use clusterExport()
      library(gdxrrw)
      library(data.table)
      gams.directory <- "/../../../../../../Program Files (x86)/GAMS/win64"
      gms.dir <- "/../../../../../../Program Files (x86)/GAMS/win64"
      data.folder <- "../capexp-india/ReEDS-2.0/gdxfiles"
      functions.directory <- "ReEDS - Visualization Tool/functions"
      basicdata.directory <- "ReEDS - Visualization Tool/basic data"
      source("ReEDS - Visualization Tool/functions/read_basic_gdx_info.R")
      NULL
    })
    
   
    # lapply across scenarios
    total_variables <- parLapply(cl = cl, scenarios, function(scen, var_list = variable_list, sd = scen.dir){
      
      # lapply across variables in 'variable_list'
      variables_to_extract <- lapply(var_list, function(vl, vl_tot = var_list, scen. = scen, sd. = sd){
        
        # Values from gdx.structure
        var_extract_nm <- vl[["obj.name"]]
        var_extract_col_orig <- vl[["col.orig"]]
        var_extract_col_new <- vl[["col.new"]]
        var_extract_type <- vl[["obj.type"]]
        
        # load data
        gdx.data <- rgdx.alltype(gdxFileName = paste0(sd., "/", scen., ".gdx"), symbolName = var_extract_nm, objectType = var_extract_type)$DF
        
        # reformat as appropriate 
        if(is.data.frame(gdx.data)){
          setDT(gdx.data)
          setnames(gdx.data, old = var_extract_col_orig, new = var_extract_col_new)
          gdx.data <- change_col_types(gdx.data, gdx.str = vl_tot)
        }
        
        
        
        return(gdx.data)
        
      })
      
      return(variables_to_extract)
      
    })
    
    # need to shut down clusters after finished
    stopCluster(cl)
    
  }else{
    
    # lapply across scenarios
    total_variables <- lapply(scenarios, function(scen, var_list = variable_list, sd = scen.dir){
      
      # lapply across variables
      variables_to_extract <- lapply(var_list, function(vl, vl_tot = var_list, scen. = scen, sd. = sd){
        
        # Values from gdx.structure
        var_extract_nm <- vl[["obj.name"]]
        var_extract_col_orig <- vl[["col.orig"]]
        var_extract_col_new <- vl[["col.new"]]
        var_extract_type <- vl[["obj.type"]]
        
        # load data
        gdx.data <- rgdx.alltype(gdxFileName = paste0(sd., "/", scen., ".gdx"), symbolName = var_extract_nm, objectType = var_extract_type)$DF
        
        # reformat as appropriate
        if(is.data.frame(gdx.data)){
          setDT(gdx.data)
          setnames(gdx.data, old = var_extract_col_orig, new = var_extract_col_new)
          gdx.data <- change_col_types(gdx.data, gdx.str = vl_tot)
        }
        
        
        
        return(gdx.data)
        
      })
      
      return(variables_to_extract)
      
    })
    
  }
  
  # append scenario names
  names(total_variables) <- scenarios
  
  return(total_variables)
  
}
