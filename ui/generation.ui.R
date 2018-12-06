tabPanel("Generation",
         br(),
         h4("Instructions"),
         p(strong("* Please press 'Submit' button to present the figure each time after changing the inputs.")),
         p(strong("* All the plot features are the same as 'Capacity' Tab, please find the specific instructions there.")),
         p("* To show default figure name, please type 'Default name' in the Plot title field."),
         p("* Please use Plot Width (%) and Plot Height (px) to control the plot output size."),
         br(),
         tabsetPanel(id = "gen",
                     tabPanel("National generation by scenario",
                              br(),
                              uiOutput("generation_nat"),
                              br()
                     ),
                     tabPanel("Regional generation by scenario",
                              br(),
                              uiOutput("generation_reg")
                     ),
                     tabPanel("Difference in generation by scenario",
                              br(),
                              uiOutput("generation_diff")
                     )
         )
)