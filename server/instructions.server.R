# -------------------------------------------------------------------------------------- /
#                                     NOTES
# -------------------------------------------------------------------------------------- /

# Directly copied over from capacity
# Need to createa a 'new capacity' plot set

#=================================1.Capacity==================================

# Extract Single variable from overall list
gdx_cap <- gdx_single_var(gdx_list = gdx_all_var, gdx_var = "cap")

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
                         region_agg = "natl", year_bounds = yearselection())
  
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
                           agg_type = aggrstyleselection(), simplify_list = simp_list_natl(), year_vs_ts = "year")
  
})


cumulative_capacity_national_annual_reactive <- reactive({
  
  plot_function_basic_testplotly(dataset = gdx_cap_simp_natl_annual(), gdx_str_match = gdx.structure, gdx_var = "cap", 
                                 x_var = "Year", plot_type = plottypeselection(), color = "Technology",
                                 group = "Technology", fill = "Technology", 
                                 x_axis = ifelse(input$plotxaxis == "Default label", "Year", input$plotxaxis),
                                 y_axis = ifelse(input$plotyaxis == "Default label", "Capacity (TW)", input$plotyaxis), 
                                 main_title = ifelse(input$plottitle == "Default name", "National Capacity by Technology Type and Scenario", input$plotyaxis), 
                                 legend_title = "Technology", 
                                 plot_theme = plot_theme, simplify_list = simp_list_natl())
  
})

output$cumulative_capacity_national_annual <- renderPlotly({
  
  cumulative_capacity_national_annual_reactive()
  
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
    ggsave(file, plot = cumulative_capacity_national_annual_reactive(), device = "png",
           width = as.numeric(input$save_width), height = as.numeric(input$save_height), dpi = 300, units = "mm")
  }
)


# # ------------------------------------------- /
# # National Cumulative Capacity Stack by timeslice
# # ------------------------------------------- /
# 
# gdx_cap_simp_natl_timeslice <- reactive({
#   
#   # Should create reactive data.table ... hopefully 
#   
#   # Should re-run everytime we 'hit refresh'
#   input$Submit
#   
#   plot_data_simplification(dataset = gdx_cap, gdx_str_match = gdx.structure, gdx_var = "cap", 
#                            agg_type = aggrstyleselection(), simplify_list = simp_list_natl(), year_vs_ts = "ts")
#   
# })
# 
# 
# cumulative_capacity_national_timeslice_reactive <- reactive({
#   
#   plot_function_basic(dataset = gdx_cap_simp_natl_timeslice(), gdx_str_match = gdx.structure, gdx_var = "cap", 
#                       x_var = "Time.Slice", plot_type = plottypeselection(), color = "Technology",
#                       group = "Technology", fill = "Technology", 
#                       x_axis = ifelse(input$plotxaxis == "Default label", "Time Slice", input$plotxaxis),
#                       y_axis = ifelse(input$plotyaxis == "Default label", "Capacity (TW)", input$plotyaxis), 
#                       main_title = ifelse(input$plottitle == "Default name", "National Capacity by Technology Type and Scenario", input$plotyaxis), 
#                       legend_title = "Technology", 
#                       plot_theme = plot_theme, simplify_list = simp_list_natl(), reverse_wrap = TRUE)
#   
# })
# 
# 
# output$cumulative_capacity_national_timeslice <- renderPlot({
#   
#   cumulative_capacity_national_timeslice_reactive()
#   
# })
# 
# output$cumulative_capacity_national_timeslice.ui <- renderUI({
#   plotOutput("cumulative_capacity_national_timeslice", width = paste0(input$width, "%"), height = paste0(input$height, "px"))
# })
# 
# output$cumulative_capacity_national_timeslice_data <- downloadHandler(
#   filename = function() { paste("cumulative_capacity_scenarios_national_timeslice", ".csv", sep="") },
#   content = function(file) {
#     fwrite(gdx_cap_simp_natl_timeslice(), file, sep = ",", row.names = FALSE)
#   }
# )
# 
# output$cumulative_capacity_national_timeslice_png <- downloadHandler(
#   filename = function() {paste("cumulative_capacity_scenarios_national_timeslice", ".png", sep="") },
#   content = function(file) {
#     ggsave(file, plot = cumulative_capacity_national_timeslice_reactive(), device = "png",
#            width = as.numeric(input$save_width), height = as.numeric(input$save_height), dpi = 300, units = "mm")
#   }
# )

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
                         region_agg = regionaggrselection(), year_bounds = yearselection())
  
})

# ------------------------------------------- /
# Regional Cumulative Capacity Stack by Year
# ------------------------------------------- /

# Simplify Data as necessary
gdx_cap_simp_regl_annual <- reactive({
  
  # Should re-run everytime we 'hit refresh'
  input$Submit
  
  plot_data_simplification(dataset = gdx_cap, gdx_str_match = gdx.structure, gdx_var = "cap",
                           agg_type = aggrstyleselection(), simplify_list = simp_list_regl(), year_vs_ts = "year")
  
})

cumulative_capacity_regional_annual_reactive <- reactive({
  
  plot_function_basic(dataset = gdx_cap_simp_regl_annual(), gdx_str_match = gdx.structure, gdx_var = "cap",
                      x_var = "Year", plot_type = plottypeselection(), color = "Technology",
                      group = "Technology", fill = "Technology",
                      x_axis = ifelse(input$plotxaxis == "Default label", "Year", input$plotxaxis),
                      y_axis = ifelse(input$plotyaxis == "Default label", "Capacity (TW)", input$plotyaxis),
                      main_title = ifelse(input$plottitle == "Default name", "National Capacity by Technology Type, Scenario and Region", input$plotyaxis),
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

# # ------------------------------------------- /
# # Regional Cumulative Capacity Stack by timeslice
# # ------------------------------------------- /
# 
# gdx_cap_simp_regl_timeslice <- reactive({
#   
#   # Should re-run everytime we 'hit refresh'
#   input$Submit
#   
#   plot_data_simplification(dataset = gdx_cap, gdx_str_match = gdx.structure, gdx_var = "cap",
#                            agg_type = aggrstyleselection(), simplify_list = simp_list_regl(), year_vs_ts = "ts")
#   
# })
# 
# cumulative_capacity_regional_timeslice_reactive <- reactive({
#   
#   plot_function_basic(dataset = gdx_cap_simp_regl_timeslice(), gdx_str_match = gdx.structure, gdx_var = "cap",
#                       x_var = "Time.Slice", plot_type = plottypeselection(), color = "Technology",
#                       group = "Technology", fill = "Technology",
#                       x_axis = ifelse(input$plotxaxis == "Default label", "Time Slice", input$plotxaxis),
#                       y_axis = ifelse(input$plotyaxis == "Default label", "Capacity (TW)", input$plotyaxis),
#                       main_title = ifelse(input$plottitle == "Default name", "National Capacity by Technology Type and Scenario", input$plotyaxis),
#                       legend_title = "Technology",
#                       plot_theme = plot_theme, simplify_list = simp_list_regl(), reverse_wrap = TRUE, wrap = "State")
#   
# })
# 
# output$cumulative_capacity_regional_timeslice <- renderPlot({
#   
#   cumulative_capacity_regional_timeslice_reactive()
#   
# })
# 
# output$cumulative_capacity_regional_timeslice.ui <- renderUI({
#   plotOutput("cumulative_capacity_regional_timeslice", width = paste0(input$width, "%"), height = paste0(input$height, "px"))
# })
# 
# output$cumulative_capacity_regional_timeslice_data <- downloadHandler(
#   filename = function() { paste("cumulative_capacity_regional_timeslice", ".csv", sep="") },
#   content = function(file) {
#     fwrite(gdx_cap_simp_regl_timeslice(), file, sep =",", row.names = FALSE)
#   }
# )
# 
# output$cumulative_capacity_regional_timeslice_png <- downloadHandler(
#   filename = function() {paste("cumulative_capacity_scenarios_regional_timeslice", ".png", sep="") },
#   content = function(file) {
#     ggsave(file, plot = cumulative_capacity_regional_timeslice_reactive(), device = "png",
#            width = as.numeric(input$save_width), height = as.numeric(input$save_height), dpi = 300, units = "mm")
#   }
# )

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
                      x_var = "Year", plot_type = plottypeselection(), color = "Technology",
                      group = "Technology", fill = "Technology",
                      x_axis = ifelse(input$plotxaxis == "Default label", "Year", input$plotxaxis),
                      y_axis = ifelse(input$plotyaxis == "Default label", "Capacity (TW)", input$plotyaxis),
                      main_title = ifelse(input$plottitle == "Default name", "Difference in National Capacity by Technology Type and Scenario", input$plotyaxis),
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


# # ------------------------------------------- /
# # Difference - National Cumulative Capacity Stack by timeslice
# # ------------------------------------------- /
# 
# # Create Difference data
# gdx_cap_diff_natl_timeslice <- reactive({
#   
#   # Should re-run everytime we 'hit refresh'
#   input$Submit
#   
#   plot_data_comparison_config(dataset = gdx_cap_simp_natl_timeslice(), gdx_str_match = gdx.structure, gdx_var = "cap", 
#                               scenario_baseline = input$ref, scenario_names = scenarioselection(), comp_type = "absolute")
#   
# })
# 
# diff_capacity_national_timeslice_reactive <- reactive({
#   
#   plot_function_basic(dataset = gdx_cap_diff_natl_timeslice(), gdx_str_match = gdx.structure, gdx_var = "cap",
#                       x_var = "Time.Slice", plot_type = plottypeselection(), color = "Technology",
#                       group = "Technology", fill = "Technology",
#                       x_axis = ifelse(input$plotxaxis == "Default label", "Time Slice", input$plotxaxis),
#                       y_axis = ifelse(input$plotyaxis == "Default label", "Capacity (TW)", input$plotyaxis),
#                       main_title = ifelse(input$plottitle == "Default name", "Difference in National Capacity by Technology Type and Scenario", input$plotyaxis),
#                       legend_title = "Technology",
#                       plot_theme = plot_theme, simplify_list = simp_list_natl(), reverse_wrap = TRUE, wrap = "State")
#   
# })
# 
# output$diff_capacity_national_timeslice <- renderPlot({
#   
#   diff_capacity_national_timeslice_reactive()
#   
# })
# 
# output$diff_capacity_national_timeslice.ui <- renderUI({
#   plotOutput("diff_capacity_national_timeslice", width = paste0(input$width, "%"), height = paste0(input$height, "px"))
# })
# 
# output$diff_capacity_national_timeslice_data <- downloadHandler(
#   filename = function() { paste("difference_capacity_scenarios_national_timeslice", ".csv", sep="") },
#   content = function(file) {
#     write.csv(gdx_cap_diff_natl_timeslice(), file, row.names = FALSE)
#   }
# )
# 
# output$diff_capacity_national_timeslice_png <- downloadHandler(
#   filename = function() {paste("difference_capacity_scenarios_national_timeslice", ".png", sep="") },
#   content = function(file) {
#     ggsave(file, plot = diff_capacity_national_timeslice_reactive(), device = "png",
#            width = as.numeric(input$save_width), height = as.numeric(input$save_height), dpi = 300, units = "mm")
#   }
# )

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
                      x_var = "Year", plot_type = plottypeselection(), color = "Technology",
                      group = "Technology", fill = "Technology",
                      x_axis = ifelse(input$plotxaxis == "Default label", "Year", input$plotxaxis),
                      y_axis = ifelse(input$plotyaxis == "Default label", "Capacity (TW)", input$plotyaxis),
                      main_title = ifelse(input$plottitle == "Default name", "Difference in Regional Capacity by Technology Type and Scenario", input$plotyaxis),
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


# # ------------------------------------------- /
# # Difference - Regional Cumulative Capacity Stack by timeslice
# # ------------------------------------------- /
# 
# # Create Difference data
# gdx_cap_diff_regl_timeslice <- reactive({
#   
#   # Should re-run everytime we 'hit refresh'
#   input$Submit
#   
#   plot_data_comparison_config(dataset = gdx_cap_simp_regl_timeslice(), gdx_str_match = gdx.structure, gdx_var = "cap", 
#                               scenario_baseline = input$ref, scenario_names = scenarioselection(), comp_type = "absolute")
#   
# })
# 
# diff_capacity_regional_timeslice <- reactive({
#   
#   plot_function_basic(dataset = gdx_cap_diff_regl_timeslice(), gdx_str_match = gdx.structure, gdx_var = "cap",
#                       x_var = "Time.Slice", plot_type = plottypeselection(), color = "Technology",
#                       group = "Technology", fill = "Technology",
#                       x_axis = ifelse(input$plotxaxis == "Default label", "Time Slice", input$plotxaxis),
#                       y_axis = ifelse(input$plotyaxis == "Default label", "Capacity (TW)", input$plotyaxis),
#                       main_title = ifelse(input$plottitle == "Default name", "Difference in National Capacity by Technology Type and Scenario", input$plotyaxis),
#                       legend_title = "Technology",
#                       plot_theme = plot_theme, simplify_list = simp_list_regl(), reverse_wrap = TRUE, wrap = "State")
#   
# })
# 
# output$diff_capacity_regional_timeslice <- renderPlot({
#   
#   diff_capacity_regional_timeslice()
#   
# })
# 
# output$diff_capacity_regional_timeslice.ui <- renderUI({
#   plotOutput("diff_capacity_regional_timeslice", width = paste0(input$width, "%"), height = paste0(input$height, "px"))
# })
# 
# output$diff_capacity_regional_timeslice_data <- downloadHandler(
#   filename = function() { paste("difference_capacity_scenarios_regional_timeslice", ".csv", sep="") },
#   content = function(file) {
#     write.csv(gdx_cap_diff_regl_timeslice(), file, row.names = FALSE)
#   }
# ) 
# 
# output$diff_capacity_regional_timeslice_png <- downloadHandler(
#   filename = function() {paste("difference_capacity_scenarios_regional_timeslice", ".png", sep="") },
#   content = function(file) {
#     ggsave(file, plot = diff_capacity_regional_timeslice(), device = "png",
#            width = as.numeric(input$save_width), height = as.numeric(input$save_height), dpi = 300, units = "mm")
#   }
# )

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
    br()#,
    # h3("National Capacity by Time Slice"),
    # downloadButton('cumulative_capacity_national_timeslice_data', 'Download Data'),
    # downloadButton('cumulative_capacity_national_timeslice_png', "Download Plot"),
    # uiOutput("cumulative_capacity_national_timeslice.ui")
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
    br()#,
    # h3("Regional Capacity by Time Slice"),
    # downloadButton('cumulative_capacity_regional_timeslice_data', 'Download Data'),
    # downloadButton('cumulative_capacity_regional_timeslice_png', "Download Plot"),
    # uiOutput("cumulative_capacity_regional_timeslice.ui")
  )
})

# -------------------------- /
# Difference Plots
# -------------------------- /

output$capacity_diff <- renderUI({
  tagList(
    h3("Difference in National Annual Capacity"),
    downloadButton('diff_capacity_national_annual_data', 'Download Data'),
    downloadButton('diff_capacity_national_annual_png', 'Download Data'),
    uiOutput("diff_capacity_national_annual.ui"),
    br(),
    # h3("Difference in National Capacity by Time Slice"),
    # downloadButton('diff_capacity_national_timeslice_data', 'Download Data'),
    # downloadButton('diff_capacity_national_timeslice_png', 'Download Data'),
    # uiOutput("diff_capacity_national_timeslice.ui"),
    # br(),
    h3("Difference in Regional Annual Capacity"),
    downloadButton('diff_capacity_regional_annual_data', 'Download Data'),
    downloadButton('diff_capacity_regional_annual_png', 'Download Data'),
    uiOutput("diff_capacity_regional_annual.ui"),
    br()#,
    # h3("Difference in Regional Capacity by Time Slice"),
    # downloadButton('diff_capacity_regional_timeslice_data', 'Download Data'),
    # downloadButton('diff_capacity_regional_timeslice_png', 'Download Data'),
    # uiOutput("diff_capacity_regional_timeslice.ui")
  )
})

