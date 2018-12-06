
# Reactive input selection
# 
# output$plottypeselection <- renderUI({
#     plottypeoption <- switch (input$comp,
#                               "Absolute value" = c("stacked","line"), 
#                               "Absolute difference" = c("bar"), 
#                               "Percentage value" = c("bar"), 
#                               "Percentage difference" = NA
#     )
#   selectInput("plottype","Plot type", choices = plottypeoption)
# })

techaggrselection <- reactive({
  
  input$Submit
  
  switch(input$techaggr, 
         "Complete" = "comp",
         "Simplified" = "simp",
         "User Defined" = "userdef"
  )
})

yearselection <- reactive({
  
  input$Submit
  
  as.numeric(input$year)
  
})

# plottypeselection <- reactive({
#   
#   input$Submit
#   
#   switch(input$plottype, 
#          "Bar" = "bar",
#          "Area" = "area"
#   )
# })


# Reactive variables
scenarioselection <- reactive({
  if(input$choosescenarios == "All") {
    scenarios
  } else {
    input$choosescenarios
  }
})

regionaggrselection <- reactive({
  
  input$Submit
  
  switch(input$regionaggr, 
         "State" = "st",
         "Regional" = "reg",
         "National" = "natl",
         "User Defined" = "userdef"
  )
})


# aggrstyleselection <- reactive({
#   
#   input$Submit
#   
#   switch(input$aggtype, 
#          "Sum" = "sum",
#          "Average" = "mean"
#   )
# })
