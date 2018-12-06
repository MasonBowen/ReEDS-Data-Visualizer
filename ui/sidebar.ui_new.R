sidebarPanel(
  submitButton("Submit", icon("refresh")),
  br(),
  
  #year
  sliderInput("year", "Year", min = 2017, max = 2047, step = 2, value = c(2017, 2047), sep =""),
  # br(),
  
  #scenarios
  # To allow develop define the scenario names: developer can defined the scenarios in "scenarios" variable above
  h4("Scenarios"),
  selectInput("ref","Reference scenario", choices = scenarios),  #verbatimTextOutput(scenarios),
  selectInput("choosescenarios", "Scenarios to compare", selected = "All", choices = c("All", scenarios), multiple=TRUE, selectize=TRUE),
  # br(),
  
  #technology
  h4("Technologies"),
  selectInput("techaggr", "Technology aggregation level", selected = "Simplified", choices = c("Complete", "Simplified", "User Defined")),
  # uiOutput("techselection"),
  # br(),
  
  #region
  h4("Regions"),
  selectInput("regionaggr", "Regional aggregation level", selected = "Regional" , choices = c("State","Regional", "National", "User Defined")),
  # uiOutput("regionselection"),
  #br(),
  
  # #water
  # uiOutput("watersidebar"),
  
  # #absolute vs compare
  # h4("Comparison"),
  # selectInput("comp","Absolute VS Difference", choices = c("Absolute value", "Absolute difference", "Percentage value", "Percentage difference")),######################
  # # br(),
  
  # # Aggregation type
  # h4("Aggregation Style"),
  # selectInput("aggtype","Sum Aggregated Data or Take Average?", choices = c("Sum", "Average")),######################
  
  #plot features
  h4("Plot features"),
  # uiOutput("plottypeselection"),
  # textInput("plottitle", label = "Plot title", value = "Default name"),
  # textInput("plotxaxis", label = "Plot x-axis label", value = "Default label"),
  # textInput("plotyaxis", label = "Plot y-axis label", value = "Default label"),
  # selectInput("plottype","Bar or Area Graph", choices = c("Bar", "Area")),
  #figure size
  sliderInput("width", "Plot Width (%)", min = 0, max = 100, value = 100, step = 5),
  sliderInput("height", "Plot Height (px)", min = 300, max = 1500, value = 500, step = 20),
  br(),
  sliderInput("save_width", "Saved Plot Width (mm)", min = 150, max = 500, value = 400, step = 25),
  sliderInput("save_height", "Saved Plot Height (mm)", min = 150, max = 400, value = 300, step = 25),
  br(),
  width = 3
)