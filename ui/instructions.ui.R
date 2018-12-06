tabPanel("Instructions",
         br(),
         h4("General Instructions"),
         p(strong("* Please press 'Submit' button to present the figure each time after changing the inputs.")),
         p("* Please use Plot Width (%) and Plot Height (px) to control the plot output size."),
         p("* Please use Save Plot Width (mm) and Save Plot Height (mm) to control the plot output size when saving images."),
         br(),
         
         h4("Instructions for Tab 1: Capacity by scenario"),
         p("* This session shows the culmulative and new installed capacity by scenarios, available plots include:"),
         p("- area plot for cumulative installed capacity by year (nationally and regionally for annual data"),
         p("- stacked bar plot for cumulative installed capacity by timeslice (regionally and nationally)"),
         p("- stacked bar plot for difference in installed capacity by year and timeslice for regionally and nationally aggregated data"),
         p("- maps of installed capacity by year and technology and regional aggregation selection,\n\t Note: you will likely have to input the desired year and hit submit to refresh the shiny, ignore the initial year"),
         p(strong("PLEASE BE PATIENT WITH THIS MAP AS IT TAKES A BIT TO LOAD, t less than 1 minute")),
         p(strong("   * Select Reference scenario to determine which scenario the other scenarios should be compared against")),
         p("   * 'Scenarios to compare' to control the scenarios for comparison;"),
         p("   * 'Technology aggregation level' to control the technology aggregation; aggregation levels should be determined in the gen_tech_set.csv;"),
         p("   * 'Region selection' to control the region to compare; aggregation levels should be determined in the country_region_state.csv;"),
         p("   * 'Year selection' to control the years present in data;"),
         
         h4("Instructions for Tab 2: Generation by scenario"),
         p("* The same as for capacity, without the maps"),
         
         h4("Instructions for Tab 3: Transmission flow by scenario"),
         p("* There is currently only one tab here, for a national map (aggregated according to the Region selection input),\n\t Note: you will likely have to input the desired year and hit submit to refresh the shiny, ignore the initial year"),
         p(strong("PLEASE BE PATIENT WITH THIS MAP AS IT TAKES A BIT TO LOAD, t less than 1 minute"))
         
         
)
