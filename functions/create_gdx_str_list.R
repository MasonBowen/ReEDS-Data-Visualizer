# Create a list with:
# 
#       1) the object name in the gams file
# 
#       2) the original columns from the gams file
# 
#       3) the new columns we want
# 
#       4) the object type from gams

gdx.structure <- list(
  
  gen        = list(obj.name = "SPLY",                                                   # Generation
                    col.orig = c("i", "c", "r", "h", "t"),
                    col.new = c("Technology", "NoIDEA", "State", "Time.Slice", "Year"),
                    obj.type = "variables"),  
  
  cap        = list(obj.name = "CAP",                                                    # Capacity
                    col.orig = c("i", "c", "r", "rs", "t"),
                    col.new = c("Technology", "NoIDEA", "State", "NoIDEA2", "Year"),
                    obj.type = "variables"),                                    
  
  load       = list(obj.name = "LOAD",                                                   # Demand
                    col.orig = c("r", "h", "t"),                                                 
                    col.new = c("State", "Time.Slice", "Year"),
                    obj.type = "variables"),
  
  ret        = list(obj.name = "RETIRE",                                                # Economic Retirements
                    col.orig = c("i", "c", "r", "rs", "t"),
                    col.new = c("Technology", "NoIDEA", "State", "NoIDEA2", "Year"),
                    obj.type = "variables"),
  
  inv_conv   = list(obj.name = "INV",                                                   # Investments in non-rsc plants
                    col.orig = c("i", "c", "r", "t"),
                    col.new = c("Technology", "NoIDEA", "State", "Year"),
                    obj.type = "variables"),
  
  inv_rsc    = list(obj.name = "INV_RSC",                                               # Investments in rsc plants
                    col.orig = c("i", "c", "r", "rs", "t", "rscbin"),
                    col.new = c("Technology", "NoIDEA", "State", "NoIDEA2", "Year", "Resource.Curve.Bin"),
                    obj.type = "variables"),
  
  flow       = list(obj.name = "FLOW",                                                  # Energy flow
                    col.orig = c("r", "rr", "h", "t", "trtype"),
                    col.new = c("State.1", "State.2", "Time.Slice", "Year", "Transmission.Type"),
                    obj.type = "variables"),
  
  opres      = list(obj.name = "OPRES",                                                 # Operating reserves
                    col.orig = c("ortype", "i", "c", "r", "h", "t"),
                    col.new = c("Operating.Reserve.Type", "Technology", "NoIDEA", "State", "Time.Slice", "Year"),
                    obj.type = "variables"),
  
  emit       = list(obj.name = "EMIT",                                                  # Emissions
                    col.orig = c("r", "t"),
                    col.new = c("State", "Year"),
                    obj.type = "variables"),
  
  tran       = list(obj.name = "CAPTRAN",                                               # Transmission capacity 
                    col.orig = c("r", "rr", "trtype", "t"),
                    col.new = c("State.1", "State.2", "Transmission.Type", "Year"),
                    obj.type = "variables"),
  
  itran      = list(obj.name = "INVTRAN",                                               # transmission investments
                    col.orig = c("r", "rr", "t", "trtype"),
                    col.new = c("State.1", "State.2", "Year", "Transmission.Type"),
                    obj.type = "variables"),
  
  isubst     = list(obj.name = "INVSUBSTATION",                                          # substation investments
                    col.orig = c("r", "vc", "t"),
                    col.new = c("State", "NoIDEA", "Year"),
                    obj.type = "variables"),
  
  obj_fnc    = list(obj.name = "Z", col.orig = "NONE", col.new = "Value", obj.type = "variables"),
  
  totflow    = list(obj.name = "totflow",                                          # total flow
                    col.orig = c("i", "j", "k", "value"),
                    col.new = c("State.1", "State.2", "Year", "FLOW"),
                    obj.type = "parameters")
  
)

# Save list as .RDS
saveRDS(gdx.structure, file = file.path(basicdata.directory,"gdx_structure_list.RDS"))
