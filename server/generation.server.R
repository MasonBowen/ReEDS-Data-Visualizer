# -------------------------------------------------------------------------------------- /
#                                     NOTES
# -------------------------------------------------------------------------------------- /

# xvar for plot_function_basic() hard coded
# color, group, fill for plot_function_basic() hard coded
# legend_title '^' hard coded

# Current structure assumes that the national and regional tabs are run before the differences tab

#=================================2.Generation==================================

# Extract Single variable from overall list
gdx_gen <- gdx_single_var(gdx_list = gdx_all_var, gdx_var = "gen")

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
# National Cumulative Generation Stack by Year
# ------------------------------------------- /

# Simplify Data as necessary
gdx_gen_simp_natl_annual <- reactive({
  
  # Should create reactive data.table ... hopefully 
  
  # Should re-run everytime we 'hit refresh'
  input$Submit
  
  plot_data_simplification(dataset = gdx_gen, gdx_str_match = gdx.structure, gdx_var = "gen", 
                           agg_type = "sum", simplify_list = simp_list_natl(), year_vs_ts = "year")
  
})


cumulative_generation_national_annual_reactive <- reactive({
  
  plot_function_basic_testplotly(dataset = gdx_gen_simp_natl_annual(), gdx_str_match = gdx.structure, gdx_var = "gen", 
                      x_var = "Year", plot_type = "area", color = "Technology",
                      group = "Technology", fill = "Technology", 
                      x_axis = "\nYear",
                      y_axis = "Generation (TWh)\n", 
                      main_title = "National Generation by Technology Type and Scenario", 
                      legend_title = "Technology", 
                      plot_theme = plot_theme, simplify_list = simp_list_natl())
  
})

output$cumulative_generation_national_annual <- renderPlotly({
  
  cumulative_generation_national_annual_reactive()[["plotly"]]
  
})


output$cumulative_generation_national_annual.ui <- renderUI({
  plotlyOutput("cumulative_generation_national_annual", width = paste0(input$width, "%"), height = paste0(input$height, "px"))
})

output$cumulative_generation_national_annual_data <- downloadHandler(
  filename = function() { paste("cumulative_generation_scenarios_national_annual", ".csv", sep="") },
  content = function(file) {
    write.csv(gdx_gen_simp_natl_annual(), file, row.names = FALSE)
  }
)

output$cumulative_generation_national_annual_png <- downloadHandler(
  filename = function() {paste("cumulative_generation_scenarios_national_annual", ".png", sep="") },
  content = function(file) {
    ggsave(file, plot = cumulative_generation_national_annual_reactive()[["ggplot"]], device = "png",
           width = as.numeric(input$save_width), height = as.numeric(input$save_height), dpi = 300, units = "mm")
  }
)


# ------------------------------------------- /
# National Cumulative Generation Stack by timeslice
# ------------------------------------------- /

gdx_gen_simp_natl_timeslice <- reactive({
  
  # Should create reactive data.table ... hopefully 
  
  # Should re-run everytime we 'hit refresh'
  input$Submit
  
  plot_data_simplification(dataset = gdx_gen, gdx_str_match = gdx.structure, gdx_var = "gen", 
                           agg_type = "sum", simplify_list = simp_list_natl(), year_vs_ts = "ts")
  
})


cumulative_generation_national_timeslice_reactive <- reactive({
  
  plot_function_basic(dataset = gdx_gen_simp_natl_timeslice(), gdx_str_match = gdx.structure, gdx_var = "gen", 
                      x_var = "Time.Slice", plot_type = "bar", color = "Technology",
                      group = "Technology", fill = "Technology", 
                      x_axis = "Time Slice",
                      y_axis = "Generation (TWh)", 
                      main_title = "National Generation by Technology Type and Scenario", 
                      legend_title = "Technology", 
                      plot_theme = plot_theme, simplify_list = simp_list_natl(), reverse_wrap = TRUE)
  
})


output$cumulative_generation_national_timeslice <- renderPlot({
  
  cumulative_generation_national_timeslice_reactive()
  
})

output$cumulative_generation_national_timeslice.ui <- renderUI({
  plotOutput("cumulative_generation_national_timeslice", width = paste0(input$width, "%"), height = paste0(input$height, "px"))
})

output$cumulative_generation_national_timeslice_data <- downloadHandler(
  filename = function() { paste("cumulative_generation_scenarios_national_timeslice", ".csv", sep="") },
  content = function(file) {
    fwrite(gdx_gen_simp_natl_timeslice(), file, sep = ",", row.names = FALSE)
  }
)

output$cumulative_generation_national_timeslice_png <- downloadHandler(
  filename = function() {paste("cumulative_generation_scenarios_national_timeslice", ".png", sep="") },
  content = function(file) {
    ggsave(file, plot = cumulative_generation_national_timeslice_reactive(), device = "png",
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
# Regional Cumulative Generation Stack by Year
# ------------------------------------------- /

# Simplify Data as necessary
gdx_gen_simp_regl_annual <- reactive({
  
  # Should re-run everytime we 'hit refresh'
  input$Submit
  
  plot_data_simplification(dataset = gdx_gen, gdx_str_match = gdx.structure, gdx_var = "gen",
                           agg_type = "sum", simplify_list = simp_list_regl(), year_vs_ts = "year")
  
})

cumulative_generation_regional_annual_reactive <- reactive({
  
  plot_function_basic(dataset = gdx_gen_simp_regl_annual(), gdx_str_match = gdx.structure, gdx_var = "gen",
                      x_var = "Year", plot_type = "area", color = "Technology",
                      group = "Technology", fill = "Technology",
                      x_axis = "Year",
                      y_axis = "Generation (TWh)",
                      main_title = "National Generation by Technology Type, Scenario and Region",
                      legend_title = "Technology",
                      plot_theme = plot_theme, simplify_list = simp_list_regl(), wrap = "State", reverse_wrap = FALSE)
  
})

output$cumulative_generation_regional_annual <- renderPlot({
  
  cumulative_generation_regional_annual_reactive()
  
})

output$cumulative_generation_regional_annual.ui <- renderUI({
  plotOutput("cumulative_generation_regional_annual", width = paste0(input$width, "%"), height = paste0(input$height, "px"))
})

output$cumulative_generation_regional_annual_data <- downloadHandler(
  filename = function() { paste("cumulative_generation_scenarios_regional_annual", ".csv", sep="") },
  content = function(file) {
    fwrite(gdx_gen_simp_regl_annual(), file, sep =",", row.names = FALSE)
  }
)

output$cumulative_generation_regional_annual_png <- downloadHandler(
  filename = function() {paste("cumulative_generation_scenarios_regional_annual", ".png", sep="") },
  content = function(file) {
    ggsave(file, plot = cumulative_generation_regional_annual_reactive(), device = "png",
           width = as.numeric(input$save_width), height = as.numeric(input$save_height), dpi = 300, units = "mm")
  }
)

# ------------------------------------------- /
# Regional Cumulative Generation Stack by timeslice
# ------------------------------------------- /

gdx_gen_simp_regl_timeslice <- reactive({
  
  # Should re-run everytime we 'hit refresh'
  input$Submit
  
  plot_data_simplification(dataset = gdx_gen, gdx_str_match = gdx.structure, gdx_var = "gen",
                           agg_type = "sum", simplify_list = simp_list_regl(), year_vs_ts = "ts")
  
})

cumulative_generation_regional_timeslice_reactive <- reactive({
  
  plot_function_basic(dataset = gdx_gen_simp_regl_timeslice(), gdx_str_match = gdx.structure, gdx_var = "gen",
                      x_var = "Time.Slice", plot_type = "bar", color = "Technology",
                      group = "Technology", fill = "Technology",
                      x_axis = "Time Slice",
                      y_axis = "Generation (TWh)",
                      main_title = "National Generation by Technology Type and Scenario",
                      legend_title = "Technology",
                      plot_theme = plot_theme, simplify_list = simp_list_regl(), reverse_wrap = TRUE, wrap = "State")
  
})

output$cumulative_generation_regional_timeslice <- renderPlot({
  
  cumulative_generation_regional_timeslice_reactive()
  
})

output$cumulative_generation_regional_timeslice.ui <- renderUI({
  plotOutput("cumulative_generation_regional_timeslice", width = paste0(input$width, "%"), height = paste0(input$height, "px"))
})

output$cumulative_generation_regional_timeslice_data <- downloadHandler(
  filename = function() { paste("cumulative_generation_regional_timeslice", ".csv", sep="") },
  content = function(file) {
    fwrite(gdx_gen_simp_regl_timeslice(), file, sep =",", row.names = FALSE)
  }
)

output$cumulative_generation_regional_timeslice_png <- downloadHandler(
  filename = function() {paste("cumulative_generation_scenarios_regional_timeslice", ".png", sep="") },
  content = function(file) {
    ggsave(file, plot = cumulative_generation_regional_timeslice_reactive(), device = "png",
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
# Should be able to reuse gdx_gen_simp_regl_annual, gdx_gen_simp_natl_annual, gdx_gen_simp_regl_timeslice and
# gdx_gen_simp_natl_timeslice; again assuming that other tabs are run first

# ------------------------------------------- /
# Difference - National Cumulative Generation Stack by Year
# ------------------------------------------- /

# Create Difference data
gdx_gen_diff_natl_annual <- reactive({
  
  # Should re-run everytime we 'hit refresh'
  input$Submit
  
  plot_data_comparison_config(dataset = gdx_gen_simp_natl_annual(), gdx_str_match = gdx.structure, gdx_var = "gen", 
                              scenario_baseline = input$ref, scenario_names = scenarioselection(), comp_type = "absolute")
  
})

diff_generation_national_annual_reactive <- reactive({
  
  plot_function_basic(dataset = gdx_gen_diff_natl_annual(), gdx_str_match = gdx.structure, gdx_var = "gen",
                      x_var = "Year", plot_type = "bar", color = "Technology",
                      group = "Technology", fill = "Technology",
                      x_axis = "Year",
                      y_axis = "Generation (TWh)",
                      main_title = "Difference in National Generation by Technology Type and Scenario",
                      legend_title = "Technology",
                      plot_theme = plot_theme, simplify_list = simp_list_natl(), reverse_wrap = TRUE, wrap = "State")
  
})

output$diff_generation_national_annual <- renderPlot({

  diff_generation_national_annual_reactive()

})

output$diff_generation_national_annual.ui <- renderUI({
  plotOutput("diff_generation_national_annual", width = paste0(input$width, "%"), height = paste0(input$height, "px"))
})

output$diff_generation_national_annual_data <- downloadHandler(
  filename = function() { paste("difference_generation_scenarios_national_annual", ".csv", sep="") },
  content = function(file) {
    write.csv(gdx_gen_diff_natl_annual(), file, row.names = FALSE)
  }
)

output$diff_generation_national_annual_png <- downloadHandler(
  filename = function() {paste("difference_generation_scenarios_national_annual", ".png", sep="") },
  content = function(file) {
    ggsave(file, plot = diff_generation_national_annual_reactive(), device = "png",
           width = as.numeric(input$save_width), height = as.numeric(input$save_height), dpi = 300, units = "mm")
  }
)


# ------------------------------------------- /
# Difference - National Cumulative Generation Stack by timeslice
# ------------------------------------------- /

# Create Difference data
gdx_gen_diff_natl_timeslice <- reactive({
  
  # Should re-run everytime we 'hit refresh'
  input$Submit
  
  plot_data_comparison_config(dataset = gdx_gen_simp_natl_timeslice(), gdx_str_match = gdx.structure, gdx_var = "gen", 
                              scenario_baseline = input$ref, scenario_names = scenarioselection(), comp_type = "absolute")
  
})

diff_generation_national_timeslice_reactive <- reactive({
  
  plot_function_basic(dataset = gdx_gen_diff_natl_timeslice(), gdx_str_match = gdx.structure, gdx_var = "gen",
                      x_var = "Time.Slice", plot_type = "bar", color = "Technology",
                      group = "Technology", fill = "Technology",
                      x_axis = "Time Slice",
                      y_axis = "Generation (TWh)",
                      main_title = "Difference in National Generation by Technology Type and Scenario",
                      legend_title = "Technology",
                      plot_theme = plot_theme, simplify_list = simp_list_natl(), reverse_wrap = TRUE, wrap = "State")
  
})

output$diff_generation_national_timeslice <- renderPlot({
  
  diff_generation_national_timeslice_reactive()
  
})

output$diff_generation_national_timeslice.ui <- renderUI({
  plotOutput("diff_generation_national_timeslice", width = paste0(input$width, "%"), height = paste0(input$height, "px"))
})

output$diff_generation_national_timeslice_data <- downloadHandler(
  filename = function() { paste("difference_generation_scenarios_national_timeslice", ".csv", sep="") },
  content = function(file) {
    write.csv(gdx_gen_diff_natl_timeslice(), file, row.names = FALSE)
  }
)

output$diff_generation_national_timeslice_png <- downloadHandler(
  filename = function() {paste("difference_generation_scenarios_national_timeslice", ".png", sep="") },
  content = function(file) {
    ggsave(file, plot = diff_generation_national_timeslice_reactive(), device = "png",
           width = as.numeric(input$save_width), height = as.numeric(input$save_height), dpi = 300, units = "mm")
  }
)

# ------------------------------------------- /
# Difference - Regional Cumulative Generation Stack by Year
# ------------------------------------------- /

# Create Difference data
gdx_gen_diff_regl_annual <- reactive({
  
  # Should re-run everytime we 'hit refresh'
  input$Submit
  
  plot_data_comparison_config(dataset = gdx_gen_simp_regl_annual(), gdx_str_match = gdx.structure, gdx_var = "gen", 
                              scenario_baseline = input$ref, scenario_names = scenarioselection(), comp_type = "absolute")
  
})

diff_generation_regional_annual_reactive <- reactive({
  
  plot_function_basic(dataset = gdx_gen_diff_regl_annual(), gdx_str_match = gdx.structure, gdx_var = "gen",
                      x_var = "Year", plot_type = "bar", color = "Technology",
                      group = "Technology", fill = "Technology",
                      x_axis = "Year",
                      y_axis = "Generation (TWh)",
                      main_title = "Difference in Regional Generation by Technology Type and Scenario",
                      legend_title = "Technology",
                      plot_theme = plot_theme, simplify_list = simp_list_regl(), wrap = "State", reverse_wrap = FALSE)
  
})

output$diff_generation_regional_annual <- renderPlot({
  
  diff_generation_regional_annual_reactive()
  
})

output$diff_generation_regional_annual.ui <- renderUI({
  plotOutput("diff_generation_regional_annual", width = paste0(input$width, "%"), height = paste0(input$height, "px"))
})

output$diff_generation_regional_annual_data <- downloadHandler(
  filename = function() { paste("difference_generation_scenarios_regional_annual", ".csv", sep="") },
  content = function(file) {
    write.csv(gdx_gen_diff_regl_annual(), file, row.names = FALSE)
  }
)
 
output$diff_generation_regional_annual_png <- downloadHandler(
  filename = function() {paste("difference_generation_scenarios_regional_annual", ".png", sep="") },
  content = function(file) {
    ggsave(file, plot = diff_generation_regional_annual_reactive(), device = "png",
           width = as.numeric(input$save_width), height = as.numeric(input$save_height), dpi = 300, units = "mm")
  }
)


# ------------------------------------------- /
# Difference - Regional Cumulative Generation Stack by timeslice
# ------------------------------------------- /

# Create Difference data
gdx_gen_diff_regl_timeslice <- reactive({
  
  # Should re-run everytime we 'hit refresh'
  input$Submit
  
  plot_data_comparison_config(dataset = gdx_gen_simp_regl_timeslice(), gdx_str_match = gdx.structure, gdx_var = "gen", 
                              scenario_baseline = input$ref, scenario_names = scenarioselection(), comp_type = "absolute")
  
})

diff_generation_regional_timeslice <- reactive({
  
  plot_function_basic(dataset = gdx_gen_diff_regl_timeslice(), gdx_str_match = gdx.structure, gdx_var = "gen",
                      x_var = "Time.Slice", plot_type = "bar", color = "Technology",
                      group = "Technology", fill = "Technology",
                      x_axis = "Time Slice",
                      y_axis = "Generation (TWh)",
                      main_title = "Difference in National Generation by Technology Type and Scenario",
                      legend_title = "Technology",
                      plot_theme = plot_theme, simplify_list = simp_list_regl(), reverse_wrap = TRUE, wrap = "State")
  
})

output$diff_generation_regional_timeslice <- renderPlot({
  
  diff_generation_regional_timeslice()
  
})

output$diff_generation_regional_timeslice.ui <- renderUI({
  plotOutput("diff_generation_regional_timeslice", width = paste0(input$width, "%"), height = paste0(input$height, "px"))
})

output$diff_generation_regional_timeslice_data <- downloadHandler(
  filename = function() { paste("difference_generation_scenarios_regional_timeslice", ".csv", sep="") },
  content = function(file) {
    write.csv(gdx_gen_diff_regl_timeslice(), file, row.names = FALSE)
  }
) 

output$diff_generation_regional_timeslice_png <- downloadHandler(
  filename = function() {paste("difference_generation_scenarios_regional_timeslice", ".png", sep="") },
  content = function(file) {
    ggsave(file, plot = diff_generation_regional_timeslice(), device = "png",
           width = as.numeric(input$save_width), height = as.numeric(input$save_height), dpi = 300, units = "mm")
  }
)

# -------------------------------------------------------------------------------------- /
#                                    RENDER PLOTS
# -------------------------------------------------------------------------------------- /

# -------------------------- /
# National Plots
# -------------------------- /

output$generation_nat <- renderUI({
  tagList(
    h3("National Annual Generation"),
    downloadButton('cumulative_generation_national_annual_data', 'Download Data'),
    downloadButton('cumulative_generation_national_annual_png', "Download Plot"),
    uiOutput("cumulative_generation_national_annual.ui"),
    br(),
    h3("National Generation by Time Slice"),
    downloadButton('cumulative_generation_national_timeslice_data', 'Download Data'),
    downloadButton('cumulative_generation_national_timeslice_png', "Download Plot"),
    uiOutput("cumulative_generation_national_timeslice.ui")
  )
})

# -------------------------- /
# Regional Plots
# -------------------------- /

output$generation_reg <- renderUI({
  tagList(
    h3("Regional Annual Generation"),
    downloadButton('cumulative_generation_regional_annual_data', 'Download Data'),
    downloadButton('cumulative_generation_regional_annual_png', "Download Plot"),
    uiOutput("cumulative_generation_regional_annual.ui"),
    br(),
    h3("Regional Generation by Time Slice"),
    downloadButton('cumulative_generation_regional_timeslice_data', 'Download Data'),
    downloadButton('cumulative_generation_regional_timeslice_png', "Download Plot"),
    uiOutput("cumulative_generation_regional_timeslice.ui")
  )
})

# -------------------------- /
# Difference Plots
# -------------------------- /

output$generation_diff <- renderUI({
  tagList(
    h3("Difference in National Annual Generation"),
    downloadButton('diff_generation_national_annual_data', 'Download Data'),
    downloadButton('diff_generation_national_annual_png', 'Download Plot'),
    uiOutput("diff_generation_national_annual.ui"),
    br(),
    h3("Difference in National Generation by Time Slice"),
    downloadButton('diff_generation_national_timeslice_data', 'Download Data'),
    downloadButton('diff_generation_national_timeslice_png', 'Download Plot'),
    uiOutput("diff_generation_national_timeslice.ui"),
    br(),
    h3("Difference in Regional Annual Generation"),
    downloadButton('diff_generation_regional_annual_data', 'Download Data'),
    downloadButton('diff_generation_regional_annual_png', 'Download Plot'),
    uiOutput("diff_generation_regional_annual.ui"),
    br(),
    h3("Difference in Regional Generation by Time Slice"),
    downloadButton('diff_generation_regional_timeslice_data', 'Download Data'),
    downloadButton('diff_generation_regional_timeslice_png', 'Download Plot'),
    uiOutput("diff_generation_regional_timeslice.ui")
  )
})

