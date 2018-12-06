tabPanel("Power Flow",
         br(),
         tabsetPanel(id = "flow",
                     tabPanel("Maps of Power Flow by scenario",
                              br(),
                              uiOutput("flow_maps"),
                              br()
                     )#,
                     # tabPanel("Regional capacity by technology",
                     #          br(),
                     #          uiOutput("capacity_reg")
                     # ),
                     # tabPanel("Difference in capacity by scenario", 
                     #          br(),
                     #          # downloadButton('map_capacity_technology_data', 'Download Data'),
                     #          uiOutput("capacity_diff")
                     # ),
                     # tabPanel("Maps of Capacity by scenario", 
                     #          br(),
                     #          # downloadButton('map_capacity_technology_data', 'Download Data'),
                     #          uiOutput("capacity_maps")
                     # )
                     
         )
)