# ----------------------------------- /
# Notes
# ----------------------------------- /

# For the scenario comparison plots, need a way either to fill in missing data (primarily years, perhaps grid expansion) 
# or need an appropriate error message

# ----------------------------------- /
# Start of Functions
# ----------------------------------- /

# for capacity/generation
plot_function_basic <- function(dataset, gdx_str_match, gdx_var, x_var = "Year", plot_type, color, 
                                group, fill, x_axis, y_axis, main_title, legend_title, plot_theme = plot_theme,
                                simplify_list, reverse_wrap = FALSE, wrap = "State"){
  # Warnings
  if(any(c("State.1", "State.2") %in% colnames(dataset))){warning("This funciton is only designed to handle single regions (or a single aggregated region at once")}
  if(!(plot_type %in% c("bar", "area"))){stop("At this time only bar and area plot types accepted")}
  
  # Adjust order of technlogy based on user defined preference (created in 'sub_select_csv.R')
  dataset$Technology <- factor(x = dataset$Technology, levels = simplify_list[["Order.V"]])
  
  p <- ggplot(dataset)
  
  if(plot_type == "area"){
    
    p <- p + geom_area(aes(x = get(x_var), y = get(gdx_str_match[[gdx_var]][["obj.name"]]), fill = get(color), group = get(color)),
                       stat= "identity", position = "stack")
  }
  
  if(plot_type == "bar"){
    
    p <- p + geom_bar(aes(x = get(x_var), y = get(gdx_str_match[[gdx_var]][["obj.name"]]), fill = get(color)),
                      stat= "identity")
  }
  
  p <- p + labs(x = x_axis, y = y_axis, title = main_title, fill = legend_title) +
    plot_theme +
    scale_fill_manual(values = simplify_list[["Color.NV"]])
  
  # If changing orientation (most likely when plotting time slice across x-axis), need to adjust x-axis labels (likely)
  dir_wrap <- ifelse(reverse_wrap, yes = "v", no = "h")
  
  
  # Detect if in national mode or regional/user defined/state mode
  regional_mode <- length(unique(dataset$State)) > 1
  # If there are multiple values for state
  if(regional_mode){
    p <- p + facet_grid(get(wrap)~Scenario, scales = "fixed")
  }else{
    p <- p + facet_wrap("Scenario", scales = "fixed", dir = dir_wrap)  
  }
  
  
  # Detect if working with years or timeslice
  year_mode <- "Year" %in% colnames(dataset)
  # Adjust x-axis labels based on if timeslice or year
  if(!year_mode){
    p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))
  }
  
  return(p)
  
}


# for comparing capacity or generation at national level
plot_function_comparison_nation <- function(dataset, gdx_str_match, gdx_var, x_var = "Year", wrap = "State", plot_type, color, 
                                            group, fill, x_axis, y_axis, main_title, legend_title, plot_theme = plot_theme, scenario_baseline){
  
  
  ds <- plot_data_comparison_config(dataset = dataset, gdx_str_match = gdx.structure, gdx_var = "cap", scenario_baseline = "Scenario_1",
                                    scenario_names = c("Scenario_1", "Scenario_2", "Scenario_3"), comp_type = "absolute")
  
  p <- ggplot(ds)
  
  if(plot_type == "area"){
    
    p <- p + geom_area(aes(x = get(x_var), y = get(gdx_str_match[[gdx_var]][["obj.name"]]), fill = get(color), group = get(color)),
                       stat= "identity", position = "stack")
  }
  
  if(plot_type == "bar"){
    
    p <- p + geom_bar(aes(x = get(x_var), y = get(gdx_str_match[[gdx_var]][["obj.name"]]), fill = get(color)),
                      stat= "identity")
  }
  
  p <- p + labs(x = x_axis, y = y_axis, title = main_title, fill = legend_title) +
    plot_theme +
    scale_fill_manual(values = category.color)
  
  
  if("Scenario" %in% colnames(dataset)){
    
    p <- p + facet_grid(get(wrap)~Scenario)  
    
  }else{
    
    p <- p + facet_wrap(wrap)
    
  }
  
  
  
  return(p)
  
  
}  


# for creating plotly html plot objects
plot_function_basic_testplotly <- function(dataset, gdx_str_match, gdx_var, x_var = "Year", plot_type, color, 
                                           group, fill, x_axis, y_axis, main_title, legend_title, plot_theme = plot_theme,
                                           simplify_list, reverse_wrap = FALSE, wrap = "State"){
  
  # pacman::p_load(plotly, ggplot, data.table)
  
  # Warnings
  if(any(c("State.1", "State.2") %in% colnames(dataset))){warning("This funciton is only designed to handle single regions (or a single aggregated region at once")}
  if(!(plot_type %in% c("bar", "area"))){stop("At this time only bar and area plot types accepted")}
  
  # Adjust order of technlogy based on user defined preference (created in 'sub_select_csv.R')
  dataset$Technology <- factor(x = dataset$Technology, levels = simplify_list[["Order.V"]])
  
  p <- ggplot(dataset, aes(text = paste("Year: <b>", Year, 
                                        "</b> \nTechnology: <b>", Technology, 
                                        "</b> \nRegion: <b>", State, 
                                        "</b> \n", gdx_str_match[[gdx_var]][["obj.name"]],": <b>", 
                                        round(get(gdx_str_match[[gdx_var]][["obj.name"]]), digits = 0), "</b> \n")))
  
  if(plot_type == "area"){
    
    p <- p + geom_area(aes(x = get(x_var), y = get(gdx_str_match[[gdx_var]][["obj.name"]]), fill = get(color), group = get(color)),
                       stat= "identity", position = "stack")
  }
  
  if(plot_type == "bar"){
    
    p <- p + geom_bar(aes(x = get(x_var), y = get(gdx_str_match[[gdx_var]][["obj.name"]]), fill = get(color)),
                      stat= "identity")
  }
  
  p <- p + labs(x = x_axis, y = y_axis, title = main_title, fill = legend_title) +
    plot_theme +
    scale_fill_manual(values = simplify_list[["Color.NV"]])
  
  # If changing orientation (most likely when plotting time slice across x-axis), need to adjust x-axis labels (likely)
  dir_wrap <- ifelse(reverse_wrap, yes = "v", no = "h")
  
  
  # Detect if in national mode or regional/user defined/state mode
  regional_mode <- length(unique(dataset$State)) > 1
  # If there are multiple values for state
  if(regional_mode){
    p <- p + facet_grid(get(wrap)~Scenario, scales = "fixed")
  }else{
    p <- p + facet_wrap("Scenario", scales = "fixed", dir = dir_wrap)  
  }
  
  
  # Detect if working with years or timeslice
  year_mode <- "Year" %in% colnames(dataset)
  # Adjust x-axis labels based on if timeslice or year
  if(!year_mode){
    p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))
  }
  
  # https://plot.ly/r/reference/#layout-legend
  ly <- ggplotly(p, tooltip = "text") %>% 
    config(displayModeBar = F) #%>% 
  # layout(xaxis=list(fixedrange=TRUE)) %>% 
  # layout(yaxis=list(fixedrange=TRUE)) %>%
  # layout(legend=list(tracegroupgap=5, plotly_legendclick = FALSE))
  
  # javascript <- "var myPlot = document.getElementsByClassName('plotly')[0]; myPlot.on('plotly_legenddoubleclick', function(d, i) {return false});"
  
  # p <- htmlwidgets::prependContent(p, htmlwidgets::onStaticRenderComplete(javascript), data=list(''))
  
  
  return(list("ggplot" = p, "plotly" = ly))
  
}



# create plotly or ggplot of maps of capacity
cap_map_ggplot_plotly <- function(dataset_shp, gdx_str_match, gdx_var, simplify_list, year.focus, tech.focus, ggplot_vs_plotly, var_name){
  
  
  # ---------------------- /
  # plot the map
  # ---------------------- /
  
  scen.num <- length(unique(dataset_shp[["Scenario"]]))
  year.num <- length(unique(dataset_shp[["Year"]]))
  
  # palette_selection <- "Reds"
  palette_selection <- simplify_list[["Color.Map.NV"]][[tech.focus]]
  
  
  
  # keep text aesthtic even if only doing ggplot as it is not time consuming
  p = ggplot(data = dataset_shp, 
             aes(text = paste("Year: <b>", Year, 
                              "</b> \nTechnology: <b>", Technology, 
                              "</b> \nRegion: <b>", State, 
                              "</b> \n", gdx.structure[[gdx_var]][["obj.name"]],": <b>", 
                              round(get(gdx.structure[[gdx_var]][["obj.name"]]), digits = 0), "</b> \n")))
  
  # Create states with color == Capacity 
  p <- p + geom_polygon(aes(long, lat, group = group, fill = get(gdx.structure[[gdx_var]][["obj.name"]])), 
                        color = "grey") +
    
    # create state outline
    geom_path(color="black",aes(group=group, y=lat, x=long)) +
    
    scale_fill_gradient(var_name, low = brewer.pal(3, palette_selection)[1], high = brewer.pal(3, palette_selection)[3],
                        space = "Lab", na.value = "darkgrey", guide = "colourbar",
                        aesthetics = "fill")+
    coord_map() +
    theme_bw() +
    theme(axis.ticks = element_blank(), 
          axis.title = element_blank(), 
          axis.text =  element_blank(),
          line = element_blank(),
          panel.border = element_blank()) +
    
    ggtitle(paste0(tech.focus, ' ', var_name, ' in ', paste0(as.character(year.focus), collapse = ", ")))
  
  if(year.num > 1){
    p <- p+ facet_grid(Year~Scenario)
  }else{
    p <- p + facet_wrap(~Scenario)
    
  }
  
  
  if(ggplot_vs_plotly == "plotly"){
    
    p <- ggplotly(p, tooltip = "text")
    
  }
  
  return(p)
  
}

# for plotting flows between regions
plot_flow_data <- function(dataset, gdx_str_match, gdx_var = "totflow", 
                           simplify_list, region_aggr, shp.file, year.focus = c(2017,2047), legend.title = "Flow\n(TWh)",
                           main.title = "Transmission Flow Across Regions in Years"){
  
  
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
  shp.coords <- data.table(Lat = shp.coords[,1], Long = shp.coords[,2], Region = rownames(shp.coords))
  
  
  # get names of regions in shape file
  shp.regions = unique(as.character(shp.df$NAME_1corr))
  
  
  ds.final <- merge(dataset, shp.coords, by.x = "State.Orig", by.y = "Region")
  setnames(ds.final, c("Lat", "Long"), c("Lat.Orig", "Long.Orig"))
  ds.final <- merge(ds.final, shp.coords, by.x = "State.Dest", by.y = "Region")
  setnames(ds.final, c("Lat", "Long"), c("Lat.Dest", "Long.Dest"))
  
  p.flow = ggplot() +
    
    # geom_polygon(aes(long, lat, group = group, fill = CAP)) +
    geom_polygon(data = shp.df, aes(long, lat, group = group), color = "lightgrey", fill = "lightgrey") +
    
    # geom_point(data = final.cap.all.test, aes(x = Cent.Long, y = Cent.Lat, size = CAP*2, color = Technology), alpha = NA)+
    # scale_size_continuous(range = c(3,8), name = "Capacity") +
    #
    geom_path(data = shp.df, color="black",aes(group=group, y=lat, x=long)) +
    
    # geom_path(stat = "identity", position = "identity", data = flow.interleave, aes(x = Long, y= Lat, color = FLOW/1000000, size = FLOW/1000000),
    #           arrow = arrow(type = "closed", angle = 25, length = unit(0.05, "inches"), ends = "first")) +
    # 
    geom_segment(stat = "identity", position = "identity", 
                 data = ds.final, 
                 aes(x = Lat.Orig, y= Long.Orig, xend = Lat.Dest, yend = Long.Dest, color = FLOW/1000000, size = FLOW/1000000),
                 arrow = arrow(type = "closed", angle = 30, length = unit(0.1, "inches"), ends = "last")) +
    
    
    
    scale_size_continuous(range = c(0, 2), guide = "none") +
    
    scale_color_gradient2(name = legend.title,
                          midpoint = mean(range(ds.final$FLOW/1000000, na.rm = T)),
                          low = muted("green"), mid = "orange",
                          high = muted("red")) +
    
    # scale_fill_gradient2(low=brewer.pal(3,palette[3])[1], mid = brewer.pal(3,palette[3])[2], high=brewer.pal(3,palette[3])[3],
    #                      guide="colorbar",na.value="darkgrey", midpoint = median(india.df[["CAP"]], na.rm = TRUE))+
    
    coord_map() +
    
    theme_bw() +
    
    theme(title = element_text(size = 10),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text =  element_blank(),
          line = element_blank(),
          panel.border = element_blank(),
          strip.text.x = element_text(size = 8, colour = "black"),
          legend.title=element_text(size=10)) +
    
    ggtitle(paste0(main.title, paste0(year.focus, collapse = ", "))) +
    
    facet_grid(Year~Scenario, space = "free")
  
  return(p.flow)
  
}