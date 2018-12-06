# -------------------------------------------------------------------------------------- /
#                                     NOTES
# -------------------------------------------------------------------------------------- /

# Directly copied over from capacity
# Need to createa a 'new capacity' plot set and difference in new capacity plots

#=================================4. Transmission Flows ==================================

# Extract Single variable from overall list
gdx_flow <- gdx_single_var(gdx_list = gdx_all_var, gdx_var = "totflow")


# -------------------------------------------------------------------------------------- /
#                                 NATIONAL MAPS
# -------------------------------------------------------------------------------------- /

# ---------------------------------------------------------------- /
# Data Prep (reactive values) for national plots and data
# ---------------------------------------------------------------- /

# create list of data tables, hard coded for national focus
simp_list_maps_flow <- reactive({
  
  # Should re-run everytime we 'hit refresh'
  input$Submit
  
  if(regionaggrselection() == "st"){
    
    select_cat_col_cat_ord(cat.color = category.color, cat.order = category.order, st.data = copy(state_data)[,State:=Gams_out,],
                           tech.data = technology, ts = timeslice, yr = years, tech_agg = techaggrselection(),
                           region_agg = "st", years.inc =  c(min(yearselection()):max(yearselection())))
  }else{
    
    select_cat_col_cat_ord(cat.color = category.color, cat.order = category.order, st.data = state_data,
                           tech.data = technology, ts = timeslice, yr = years, tech_agg = techaggrselection(),
                           region_agg = regionaggrselection(), years.inc =  c(min(yearselection()):max(yearselection())))
    
    
  }
  
  
  
})

# ------------------------------------------- /
# National Cumulative Flow Maps by Year
# ------------------------------------------- /

# Simplify Data as necessary
gdx_flow_simp_maps_annual <- reactive({

  # Should create reactive data.table ... hopefully

  # Should re-run everytime we 'hit refresh'
  input$Submit


  data_set <- plot_data_simplification_flow_trans(dataset = gdx_flow, gdx_str_match = gdx.structure, gdx_var = "totflow",
                                       agg_type = "sum", simplify_list = simp_list_maps_flow(), year_vs_ts = "year")

  plot_data_prep_flow(dataset = data_set, gdx_str_match = gdx.structure, gdx_var = "totflow",
                    simplify_list = simp_list_maps_flow(), year.focus = as.numeric(flow_map_year_selected()),
                    region_aggr = ifelse(regionaggrselection() == "st", yes = FALSE, no = TRUE))

})


cumulative_flow_maps_annual_reactive <- reactive({
  
  validate(
    need(try(!(is.null(flow_map_year_selected()))), "\nPlease select a year and hit submit again")
  )
  
  
  plot_flow_data(dataset = gdx_flow_simp_maps_annual(), gdx_str_match = gdx.structure, gdx_var = "totflow", 
                 simplify_list = simp_list_maps_flow(), region_aggr = ifelse(regionaggrselection() == "st", yes = FALSE, no = TRUE), 
                 shp.file = country.shape, year.focus = as.numeric(flow_map_year_selected()), legend.title = "Flow\n(TWh)",
                 main.title = "Transmission Flows Across Regions in Years ")

  
})

output$cumulative_flow_maps_annual <- renderPlot({
  
  cumulative_flow_maps_annual_reactive()
  
})


output$cumulative_flow_maps_annual.ui <- renderUI({
  plotOutput("cumulative_flow_maps_annual", width = paste0(input$width, "%"), height = paste0(input$height, "px"))
})


output$cumulative_flow_maps_annual_data <- downloadHandler(
  filename = function() { paste("gdx_flow_simp_maps_annual", ".csv", sep="") },
  content = function(file) {
    write.csv(gdx_flow_simp_maps_annual(), file, row.names = FALSE)
  }
)

output$cumulative_flow_maps_annual_png <- downloadHandler(
  filename = function() {paste("cumulative_flow_maps_annual", ".png", sep="") },
  content = function(file) {
    ggsave(file, plot = cumulative_flow_maps_annual_reactive(), device = "png",
           width = as.numeric(input$save_width), height = as.numeric(input$save_height), dpi = 300, units = "mm")
  }
)

output$map_years_flow <- renderUI({
  selectInput(inputId = "map_years_flow_in", label = "Years to Plot for Map", 
              choices = simp_list_maps_flow()[["Year.V"]], multiple = TRUE,
              selected = NULL)
})

flow_map_year_selected <- reactive({input$map_years_flow_in})




# -------------------------------------------------------------------------------------- /
#                                    RENDER PLOTS
# -------------------------------------------------------------------------------------- /

# -------------------------- /
# Map Plots
# -------------------------- /

output$flow_maps <- renderUI({
  tagList(
    h3("Maps of Regional Transmission Flows by Scenario and Year"),
    uiOutput("map_years_flow"),
    br(),
    downloadButton('cumulative_flow_maps_annual_png', 'Download Plot'),
    downloadButton('cumulative_flow_maps_annual_data', 'Download Data'),
    uiOutput("cumulative_flow_maps_annual.ui")
  )
})

