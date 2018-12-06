# -------------------------------------------------------------------------------------- /
#                                     NOTES
# -------------------------------------------------------------------------------------- /

# Directly copied over from capacity
# Need to createa a 'new capacity' plot set and difference in new capacity plots

#=================================1.Capacity==================================

# Extract Single variable from overall list
gdx_cap <- gdx_single_var(gdx_list = gdx_all_var, gdx_var = "cap")

# Should net retirements be ignored when calculating new capacity? True = ignore
ign.ret <- TRUE

# -------------------------------------------------------------------------------------- /
#                                 NATIONAL
# -------------------------------------------------------------------------------------- /

# ---------------------------------------------------------------- /
# Data Prep (reactive values) for national plots and data
# ---------------------------------------------------------------- /

# create list of data tables, hard coded for national focus
simp_list_natl <- reactive({
  
  # Should re-run everytime we 'hit refresh'
  input$Submit
  
  select_cat_col_cat_ord(cat.color = category.color, cat.order = category.order, st.data = state_data, 
                         tech.data = technology, ts = timeslice, yr = years, tech_agg = techaggrselection(), 
                         region_agg = "natl", years.inc =  c(min(yearselection()):max(yearselection())))
  
})

# ------------------------------------------- /
# National Cumulative Capacity Stack by Year
# ------------------------------------------- /

# Simplify Data as necessary
gdx_cap_simp_natl_annual <- reactive({
  
  # Should create reactive data.table ... hopefully 
  
  # Should re-run everytime we 'hit refresh'
  input$Submit
  
  plot_data_simplification(dataset = gdx_cap, gdx_str_match = gdx.structure, gdx_var = "cap", 
                           agg_type = "sum", simplify_list = simp_list_natl(), year_vs_ts = "year")
  
})


cumulative_capacity_national_annual_reactive <- reactive({
  
  plot_function_basic_testplotly(dataset = gdx_cap_simp_natl_annual(), gdx_str_match = gdx.structure, gdx_var = "cap", 
                                 x_var = "Year", plot_type = "area", color = "Technology",
                                 group = "Technology", fill = "Technology", 
                                 x_axis = "\nYear", y_axis = "Capacity (TW)\n", 
                                 main_title = "National Capacity by Technology Type and Scenario", 
                                 legend_title = "Technology", 
                                 plot_theme = plot_theme, simplify_list = simp_list_natl())
  
})

output$cumulative_capacity_national_annual <- renderPlotly({
  
  cumulative_capacity_national_annual_reactive()[["plotly"]]
  
})


output$cumulative_capacity_national_annual.ui <- renderUI({
  plotlyOutput("cumulative_capacity_national_annual", width = paste0(input$width, "%"), height = paste0(input$height, "px"))
})

output$cumulative_capacity_national_annual_data <- downloadHandler(
  filename = function() { paste("cumulative_capacity_scenarios_national_annual", ".csv", sep="") },
  content = function(file) {
    write.csv(gdx_cap_simp_natl_annual(), file, row.names = FALSE)
  }
)

output$cumulative_capacity_national_annual_png <- downloadHandler(
  filename = function() {paste("cumulative_capacity_scenarios_national_annual", ".png", sep="") },
  content = function(file) {
    ggsave(file, plot = cumulative_capacity_national_annual_reactive()[["ggplot"]], device = "png",
           width = as.numeric(input$save_width), height = as.numeric(input$save_height), dpi = 300, units = "mm")
  }
)


# ------------------------------------------- /
# National New Capacity Stack by Year
# ------------------------------------------- /

gdx_newcap_simp_natl_annual <- reactive({
  
  # Should create reactive data.table ... hopefully
  
  # Should re-run everytime we 'hit refresh'
  input$Submit
  
  find_yoy_change(dataset = gdx_cap_simp_natl_annual(), gdx_var = "cap", gdx_str_match = gdx.structure, ignore.retire = ign.ret)
  
})


new_capacity_national_annual_reactive <- reactive({
  
  plot_function_basic(dataset = gdx_newcap_simp_natl_annual(), gdx_str_match = gdx.structure, gdx_var = "cap", 
                      x_var = "Year", plot_type = "bar", color = "Technology",
                      group = "Technology", fill = "Technology", 
                      x_axis = "Year",
                      y_axis = "Capacity (TW)", 
                      main_title = "New National Capacity by Technology Type and Scenario", 
                      legend_title = "Technology", 
                      plot_theme = plot_theme, simplify_list = simp_list_natl())
  
})


output$new_capacity_national_annual <- renderPlot({
  
  new_capacity_national_annual_reactive()
  
})


output$new_capacity_national_annual.ui <- renderUI({
  plotOutput("new_capacity_national_annual", width = paste0(input$width, "%"), height = paste0(input$height, "px"))
})

output$new_capacity_national_annual_data <- downloadHandler(
  filename = function() { paste("new_capacity_scenarios_national_annual", ".csv", sep="") },
  content = function(file) {
    fwrite(gdx_newcap_simp_natl_annual(), file, sep = ",", row.names = FALSE)
  }
)

output$new_capacity_national_annual_png <- downloadHandler(
  filename = function() {paste("new_capacity_scenarios_national_annual", ".png", sep="") },
  content = function(file) {
    ggsave(file, plot = new_capacity_national_annual_reactive(), device = "png",
           width = as.numeric(input$save_width), height = as.numeric(input$save_height), dpi = 300, units = "mm")
  }
)

# -------------------------------------------------------------------------------------- /
#                           REGIONAL - Either State or Region
# -------------------------------------------------------------------------------------- /

# ---------------------------------------------------------------- /
# Data Prep (reactive values) for regional plots and data
# ---------------------------------------------------------------- /

# create list of data tables, hard coded for national focus
simp_list_regl <- reactive({
  
  # Should re-run everytime we 'hit refresh'
  input$Submit
  
  select_cat_col_cat_ord(cat.color = category.color, cat.order = category.order, st.data = state_data,
                         tech.data = technology, ts = timeslice, yr = years, tech_agg = techaggrselection(),
                         region_agg = regionaggrselection(), years.inc =  c(min(yearselection()):max(yearselection())))
  
})

# ------------------------------------------- /
# Regional Cumulative Capacity Stack by Year
# ------------------------------------------- /

# Simplify Data as necessary
gdx_cap_simp_regl_annual <- reactive({
  
  # Should re-run everytime we 'hit refresh'
  input$Submit
  
  plot_data_simplification(dataset = gdx_cap, gdx_str_match = gdx.structure, gdx_var = "cap",
                           agg_type = "sum", simplify_list = simp_list_regl(), year_vs_ts = "year")
  
})

cumulative_capacity_regional_annual_reactive <- reactive({
  
  plot_function_basic(dataset = gdx_cap_simp_regl_annual(), gdx_str_match = gdx.structure, gdx_var = "cap",
                      x_var = "Year", plot_type = "area", color = "Technology",
                      group = "Technology", fill = "Technology",
                      x_axis = "Year",
                      y_axis = "Capacity (TW)",
                      main_title = "Regional Capacity by Technology Type, Scenario and Region",
                      legend_title = "Technology",
                      plot_theme = plot_theme, simplify_list = simp_list_regl(), wrap = "State", reverse_wrap = FALSE)
  
})

output$cumulative_capacity_regional_annual <- renderPlot({
  
  cumulative_capacity_regional_annual_reactive()
  
})

output$cumulative_capacity_regional_annual.ui <- renderUI({
  plotOutput("cumulative_capacity_regional_annual", width = paste0(input$width, "%"), height = paste0(input$height, "px"))
})

output$cumulative_capacity_regional_annual_data <- downloadHandler(
  filename = function() { paste("cumulative_capacity_scenarios_regional_annual", ".csv", sep="") },
  content = function(file) {
    fwrite(gdx_cap_simp_regl_annual(), file, sep =",", row.names = FALSE)
  }
)

output$cumulative_capacity_regional_annual_png <- downloadHandler(
  filename = function() {paste("cumulative_capacity_scenarios_regional_annual", ".png", sep="") },
  content = function(file) {
    ggsave(file, plot = cumulative_capacity_regional_annual_reactive(), device = "png",
           width = as.numeric(input$save_width), height = as.numeric(input$save_height), dpi = 300, units = "mm")
  }
)

# ------------------------------------------- /
# Regional New Capacity Stack by Year
# ------------------------------------------- /

gdx_newcap_simp_regl_annual <- reactive({
  
  # Should re-run everytime we 'hit refresh'
  input$Submit
  
  find_yoy_change(dataset = gdx_cap_simp_regl_annual(), gdx_var = "cap", gdx_str_match = gdx.structure, ignore.retire = ign.ret)
  
  
})

new_capacity_regional_annual_reactive <- reactive({
  
  plot_function_basic(dataset = gdx_newcap_simp_regl_annual(), gdx_str_match = gdx.structure, gdx_var = "cap",
                      x_var = "Year", plot_type = "bar", color = "Technology",
                      group = "Technology", fill = "Technology",
                      x_axis = "Time Slice",
                      y_axis = "New Capacity (TW)",
                      main_title = "Regional New Capacity by Technology Type and Scenario",
                      legend_title = "Technology",
                      plot_theme = plot_theme, simplify_list = simp_list_regl(), reverse_wrap = TRUE, wrap = "State")
  
})

output$new_capacity_regional_annual <- renderPlot({
  
  new_capacity_regional_annual_reactive()
  
})

output$new_capacity_regional_annual.ui <- renderUI({
  plotOutput("new_capacity_regional_annual", width = paste0(input$width, "%"), height = paste0(input$height, "px"))
})

output$new_capacity_regional_annual_data <- downloadHandler(
  filename = function() { paste("new_capacity_regional_annual", ".csv", sep="") },
  content = function(file) {
    fwrite(gdx_newcap_simp_regl_annual(), file, sep =",", row.names = FALSE)
  }
)

output$new_capacity_regional_annual_png <- downloadHandler(
  filename = function() {paste("new_capacity_scenarios_regional_annual", ".png", sep="") },
  content = function(file) {
    ggsave(file, plot = new_capacity_regional_annual_reactive(), device = "png",
           width = as.numeric(input$save_width), height = as.numeric(input$save_height), dpi = 300, units = "mm")
  }
)

# -------------------------------------------------------------------------------------- /
#                           Differences - National and State or Region
# -------------------------------------------------------------------------------------- /

# ---------------------------------------------------------------- /
# Data Prep (reactive values) for regional plots and data
# ---------------------------------------------------------------- /

# should be able to reuse simp_list_regl and simp_list_natl (this assumes that those tabs have been run ...)
# Should be able to reuse gdx_cap_simp_regl_annual, gdx_cap_simp_natl_annual, gdx_cap_simp_regl_timeslice and
# gdx_cap_simp_natl_timeslice; again assuming that other tabs are run first

# ------------------------------------------- /
# Difference - National Cumulative Capacity Stack by Year
# ------------------------------------------- /

# Create Difference data
gdx_cap_diff_natl_annual <- reactive({
  
  # Should re-run everytime we 'hit refresh'
  input$Submit
  
  plot_data_comparison_config(dataset = gdx_cap_simp_natl_annual(), gdx_str_match = gdx.structure, gdx_var = "cap", 
                              scenario_baseline = input$ref, scenario_names = scenarioselection(), comp_type = "absolute")
  
})

diff_capacity_national_annual_reactive <- reactive({
  
  plot_function_basic(dataset = gdx_cap_diff_natl_annual(), gdx_str_match = gdx.structure, gdx_var = "cap",
                      x_var = "Year", plot_type = "bar", color = "Technology",
                      group = "Technology", fill = "Technology",
                      x_axis = "Year",
                      y_axis = "Capacity (TW)",
                      main_title = "Difference in National Capacity by Technology Type and Scenario",
                      legend_title = "Technology",
                      plot_theme = plot_theme, simplify_list = simp_list_natl(), reverse_wrap = TRUE, wrap = "State")
  
})

output$diff_capacity_national_annual <- renderPlot({
  
  diff_capacity_national_annual_reactive()
  
})

output$diff_capacity_national_annual.ui <- renderUI({
  plotOutput("diff_capacity_national_annual", width = paste0(input$width, "%"), height = paste0(input$height, "px"))
})

output$diff_capacity_national_annual_data <- downloadHandler(
  filename = function() { paste("difference_capacity_scenarios_national_annual", ".csv", sep="") },
  content = function(file) {
    write.csv(gdx_cap_diff_natl_annual(), file, row.names = FALSE)
  }
)

output$diff_capacity_national_annual_png <- downloadHandler(
  filename = function() {paste("difference_capacity_scenarios_national_annual", ".png", sep="") },
  content = function(file) {
    ggsave(file, plot = diff_capacity_national_annual_reactive(), device = "png",
           width = as.numeric(input$save_width), height = as.numeric(input$save_height), dpi = 300, units = "mm")
  }
)


# ------------------------------------------- /
# Difference - National New Capacity Stack by Year
# ------------------------------------------- /

# Create Difference data
gdx_newcap_diff_natl_annual <- reactive({
  
  # Should re-run everytime we 'hit refresh'
  input$Submit
  
  plot_data_comparison_config(dataset = gdx_newcap_simp_natl_annual(), gdx_str_match = gdx.structure, gdx_var = "cap",
                              scenario_baseline = input$ref, scenario_names = scenarioselection(), comp_type = "absolute")
  
})

diff_newcapacity_national_annual_reactive <- reactive({
  
  plot_function_basic(dataset = gdx_newcap_diff_natl_annual(), gdx_str_match = gdx.structure, gdx_var = "cap",
                      x_var = "Year", plot_type = "bar", color = "Technology",
                      group = "Technology", fill = "Technology",
                      x_axis = "Year",
                      y_axis = "New Capacity (TW)",
                      main_title = "Difference in New National Capacity by Technology Type Across Scenarios",
                      legend_title = "Technology",
                      plot_theme = plot_theme, simplify_list = simp_list_natl(), reverse_wrap = TRUE, wrap = "State")
  
})

output$diff_newcapacity_national_annual <- renderPlot({
  
  diff_newcapacity_national_annual_reactive()
  
})

output$diff_newcapacity_national_annual.ui <- renderUI({
  plotOutput("diff_newcapacity_national_annual", width = paste0(input$width, "%"), height = paste0(input$height, "px"))
})

output$diff_newcapacity_national_annual_data <- downloadHandler(
  filename = function() { paste("difference_new_capacity_scenarios_national_annual", ".csv", sep="") },
  content = function(file) {
    write.csv(gdx_newcap_diff_natl_annual(), file, row.names = FALSE)
  }
)

output$diff_newcapacity_national_annual_png <- downloadHandler(
  filename = function() {paste("difference_new_capacity_scenarios_national_annual", ".png", sep="") },
  content = function(file) {
    ggsave(file, plot = diff_newcapacity_national_annual_reactive(), device = "png",
           width = as.numeric(input$save_width), height = as.numeric(input$save_height), dpi = 300, units = "mm")
  }
)

# ------------------------------------------- /
# Difference - Regional Cumulative Capacity Stack by Year
# ------------------------------------------- /

# Create Difference data
gdx_cap_diff_regl_annual <- reactive({
  
  # Should re-run everytime we 'hit refresh'
  input$Submit
  
  plot_data_comparison_config(dataset = gdx_cap_simp_regl_annual(), gdx_str_match = gdx.structure, gdx_var = "cap", 
                              scenario_baseline = input$ref, scenario_names = scenarioselection(), comp_type = "absolute")
  
})

diff_capacity_regional_annual_reactive <- reactive({
  
  plot_function_basic(dataset = gdx_cap_diff_regl_annual(), gdx_str_match = gdx.structure, gdx_var = "cap",
                      x_var = "Year", plot_type = "bar", color = "Technology",
                      group = "Technology", fill = "Technology",
                      x_axis = "Year",
                      y_axis = "Capacity (TW)",
                      main_title = "Difference in Regional Capacity by Technology Type and Scenario",
                      legend_title = "Technology",
                      plot_theme = plot_theme, simplify_list = simp_list_regl(), wrap = "State", reverse_wrap = FALSE)
  
})

output$diff_capacity_regional_annual <- renderPlot({
  
  diff_capacity_regional_annual_reactive()
  
})

output$diff_capacity_regional_annual.ui <- renderUI({
  plotOutput("diff_capacity_regional_annual", width = paste0(input$width, "%"), height = paste0(input$height, "px"))
})

output$diff_capacity_regional_annual_data <- downloadHandler(
  filename = function() { paste("difference_capacity_scenarios_regional_annual", ".csv", sep="") },
  content = function(file) {
    write.csv(gdx_cap_diff_regl_annual(), file, row.names = FALSE)
  }
)

output$diff_capacity_regional_annual_png <- downloadHandler(
  filename = function() {paste("difference_capacity_scenarios_regional_annual", ".png", sep="") },
  content = function(file) {
    ggsave(file, plot = diff_capacity_regional_annual_reactive(), device = "png",
           width = as.numeric(input$save_width), height = as.numeric(input$save_height), dpi = 300, units = "mm")
  }
)


# ------------------------------------------- /
# Difference - Regional Cumulative Capacity Stack by timeslice
# ------------------------------------------- /

# Create Difference data
gdx_newcap_diff_regl_annual <- reactive({
  
  # Should re-run everytime we 'hit refresh'
  input$Submit
  
  plot_data_comparison_config(dataset = gdx_newcap_simp_regl_annual(), gdx_str_match = gdx.structure, gdx_var = "cap",
                              scenario_baseline = input$ref, scenario_names = scenarioselection(), comp_type = "absolute")
  
})

diff_newcapacity_regional_annual <- reactive({
  
  plot_function_basic(dataset = gdx_newcap_diff_regl_annual(), gdx_str_match = gdx.structure, gdx_var = "cap",
                      x_var = "Year", plot_type = "bar", color = "Technology",
                      group = "Technology", fill = "Technology",
                      x_axis = "Year",
                      y_axis = "New Capacity (TW)",
                      main_title = "Difference in New Regional Capacity by Technology Type and Scenario",
                      legend_title = "Technology",
                      plot_theme = plot_theme, simplify_list = simp_list_regl(), reverse_wrap = TRUE, wrap = "State")
  
})

output$diff_newcapacity_regional_annual <- renderPlot({
  
  diff_newcapacity_regional_annual()
  
})

output$diff_newcapacity_regional_annual.ui <- renderUI({
  plotOutput("diff_newcapacity_regional_annual", width = paste0(input$width, "%"), height = paste0(input$height, "px"))
})

output$diff_newcapacity_regional_annual_data <- downloadHandler(
  filename = function() { paste("difference_new_capacity_scenarios_regional_annual", ".csv", sep="") },
  content = function(file) {
    write.csv(gdx_newcap_diff_regl_annual(), file, row.names = FALSE)
  }
)

output$diff_newcapacity_regional_annual_png <- downloadHandler(
  filename = function() {paste("difference_new_capacity_scenarios_regional_annual", ".png", sep="") },
  content = function(file) {
    ggsave(file, plot = diff_newcapacity_regional_annual(), device = "png",
           width = as.numeric(input$save_width), height = as.numeric(input$save_height), dpi = 300, units = "mm")
  }
)


# -------------------------------------------------------------------------------------- /
#                                 NATIONAL MAPS
# -------------------------------------------------------------------------------------- /

# ---------------------------------------------------------------- /
# Data Prep (reactive values) for national plots and data
# ---------------------------------------------------------------- /

# create list of data tables, hard coded for national focus
simp_list_maps_cap <- reactive({
  
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
# National Cumulative Capacity Maps by Year
# ------------------------------------------- /

# Simplify Data as necessary
gdx_cap_simp_maps_annual <- reactive({
  
  # Should create reactive data.table ... hopefully
  
  # Should re-run everytime we 'hit refresh'
  input$Submit
  
  validate(
    need(try(!(is.null(input$tech_sel_maps))), "Please select a technology to map")
  )
  
  data_set <- plot_data_simplification(dataset = gdx_cap, gdx_str_match = gdx.structure, gdx_var = "cap",
                                       agg_type = "sum", simplify_list = simp_list_maps_cap(), year_vs_ts = "year")
  
  cap_map_data_prep(dataset = data_set, shp.file = country.shape, gdx_str_match = gdx.structure, gdx_var = "cap",
                    simplify_list = simp_list_maps_cap(), year.focus = as.numeric(input$map_years_cap),
                    tech.focus = as.character(input$tech_sel_maps), 
                    region_aggr = ifelse(regionaggrselection() == "st", yes = FALSE, no = TRUE))
  
})

output$tech_select_maps <- renderUI({
  selectInput(inputId = "tech_sel_maps", label = "Select Technology to Map:", 
              choices = simp_list_maps_cap()[["Tech.Re"]][["replacement"]], multiple = FALSE,
              selected = simp_list_maps_cap()[["Tech.Re"]][["replacement"]][1])
})

cumulative_capacity_maps_annual_reactive <- reactive({
  
  validate(
    need(try(!(is.null(input$tech_sel_maps))), "\nPlease select a technology to map")
  )
  
  cap_map_ggplot_plotly(dataset_shp = gdx_cap_simp_maps_annual(), gdx_str_match = gdx.structure, gdx_var = "cap",
                        simplify_list = simp_list_maps_cap(), year.focus = as.numeric(input$map_years_cap),
                        tech.focus = as.character(input$tech_sel_maps), ggplot_vs_plotly = "ggplot", var_name = "Capacity")
  
})

output$cumulative_capacity_maps_annual <- renderPlot({
  
  cumulative_capacity_maps_annual_reactive()
  
})


output$cumulative_capacity_maps_annual.ui <- renderUI({
  plotOutput("cumulative_capacity_maps_annual", width = paste0(input$width, "%"), height = paste0(input$height, "px"))
})



output$cumulative_capacity_maps_annual_data <- downloadHandler(
  filename = function() { paste("gdx_cap_simp_maps_annual", ".csv", sep="") },
  content = function(file) {
    write.csv(gdx_cap_simp_maps_annual(), file, row.names = FALSE)
  }
)

output$cumulative_capacity_maps_annual_png <- downloadHandler(
  filename = function() {paste("cumulative_capacity_maps_annual", ".png", sep="") },
  content = function(file) {
    ggsave(file, plot = cumulative_capacity_maps_annual_reactive(), device = "png",
           width = as.numeric(input$save_width), height = as.numeric(input$save_height), dpi = 300, units = "mm")
  }
)



# -------------------------------------------------------------------------------------- /
#                                    RENDER PLOTS
# -------------------------------------------------------------------------------------- /

# -------------------------- /
# National Plots
# -------------------------- /

output$capacity_nat <- renderUI({
  tagList(
    h3("National Annual Capacity"),
    downloadButton('cumulative_capacity_national_annual_data', 'Download Data'),
    downloadButton('cumulative_capacity_national_annual_png', "Download Plot"),
    uiOutput("cumulative_capacity_national_annual.ui"),
    br(),
    h3("New National Annual Capacity"),
    downloadButton('new_capacity_national_annual_data', 'Download Data'),
    downloadButton('new_capacity_national_annual_png', "Download Plot"),
    uiOutput("new_capacity_national_annual.ui")
  )
})

# -------------------------- /
# Regional Plots
# -------------------------- /

output$capacity_reg <- renderUI({
  tagList(
    h3("Regional Annual Capacity"),
    downloadButton('cumulative_capacity_regional_annual_data', 'Download Data'),
    downloadButton('cumulative_capacity_regional_annual_png', "Download Plot"),
    uiOutput("cumulative_capacity_regional_annual.ui"),
    br(),
    h3("New Regional Annual Capacity"),
    downloadButton('new_capacity_regional_annual_data', 'Download Data'),
    downloadButton('new_capacity_regional_annual_png', "Download Plot"),
    uiOutput("new_capacity_regional_annual.ui")
  )
})

# -------------------------- /
# Difference Plots
# -------------------------- /

output$capacity_diff <- renderUI({
  tagList(
    h3("Difference in National Annual Capacity"),
    downloadButton('diff_capacity_national_annual_data', 'Download Data'),
    downloadButton('diff_capacity_national_annual_png', 'Download Plot'),
    uiOutput("diff_capacity_national_annual.ui"),
    br(),
    h3("Difference in New National Annual Capacity"),
    downloadButton('diff_newcapacity_national_annual_data', 'Download Data'),
    downloadButton('diff_newcapacity_national_annual_png', 'Download Plot'),
    uiOutput("diff_newcapacity_national_annual.ui"),
    br(),
    h3("Difference in Regional Annual Capacity"),
    downloadButton('diff_capacity_regional_annual_data', 'Download Data'),
    downloadButton('diff_capacity_regional_annual_png', 'Download Plot'),
    uiOutput("diff_capacity_regional_annual.ui"),
    br(),
    h3("Difference in New Regional Annual Capacity"),
    downloadButton('diff_newcapacity_regional_annual_data', 'Download Data'),
    downloadButton('diff_newcapacity_regional_annual_png', 'Download Plot'),
    uiOutput("diff_newcapacity_regional_annual.ui")
  )
})

# -------------------------- /
# Difference Plots
# -------------------------- /

output$capacity_maps <- renderUI({
  tagList(
    h3("Maps of National Annual Capacity by Scenario and Year"),
    selectInput("map_years_cap", label = "Years to Plot for Map", choices = c(min(input$year):max(input$year)),selected = max(input$year), multiple = TRUE),
    uiOutput("tech_select_maps"),
    br(),
    downloadButton('cumulative_capacity_maps_annual_data', 'Download Data'),
    downloadButton('cumulative_capacity_maps_annual_png', 'Download Plot'),
    uiOutput("cumulative_capacity_maps_annual.ui")
  )
})

