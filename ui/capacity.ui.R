tabPanel("Capacity",
         br(),
         tabsetPanel(id = "cap",
                     tabPanel("National capacity by scenario",
                              br(),
                              uiOutput("capacity_nat"),
                              br()
                     ),
                     tabPanel("Regional capacity by technology",
                              br(),
                              uiOutput("capacity_reg")
                     ),
                     tabPanel("Difference in capacity by scenario", 
                              br(),
                              # downloadButton('map_capacity_technology_data', 'Download Data'),
                              uiOutput("capacity_diff")
                     ),
                     tabPanel("Maps of Capacity by scenario", 
                              br(),
                              # downloadButton('map_capacity_technology_data', 'Download Data'),
                              uiOutput("capacity_maps")
                     )
                     
         )
)