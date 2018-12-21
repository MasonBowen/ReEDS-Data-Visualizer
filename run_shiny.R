# Created by ReEDS-India team 10/2018 - 12/2018 (Amy Rose, David Palchak, Ilya Chernyakhovskiy, Thomas Bowen)

# Welcome to the ReEDS Interactive Visualization Tool!
# Please address all concerns/comments/suggestions to Thomas Bowen

# For the set up, please refer to the README.txt file in the same directory as this script


# Notes to users: 

#         1) You may need to change the 'base.directory' variable below to point R to the proper folder(s)
#               The base directory should have 4 subfolders: '/basic data/', '/functions/', '/server/' and '/ui/'
# 
#         2) You may also need to go into the 'start_gdxr.R' file and change the 'gams.folder' location so that it 
#             points to your installation of gams



# --------------------------------------------- /
# Install and load necessary packages
# --------------------------------------------- /

# We will be using pacman to install and load all necessary packages, so we need to make sure it's installed
if (!require("pacman")){
  install.packages("pacman")
}

# # Because of multiple packages' dependency on rJava, and its notorious fickleness, we will install and load separately
# if (!require("rJava")){
#   
#   # point r to your installation of java CHANGE THE DIRECTORY!!!
#   Sys.setenv(JAVA_HOME='C:\\Your\\Java\\Directory')
#   
#   # install and load
#   install.packages("rJava")
#   library(rJava)
#   
#   
# }

# Load rest of packages
pacman::p_load(data.table,           # Used to manipulate data frames and create data.table objects
               
               tidyverse,            # Used to manipulate objects (primarily just ggplot)
               
               qdap,                 # Used for its mgsub fucntion which allows for a gsub with multiple pattern-replacement pairs, 
                                     #      could be replaced with stringr::str_replace_all(string, list) if causing issues
               
               plotly,               # Used for creating html-power interactive plots
               
               htmlwidgets,          # Used for manipulating html objects and (one day) saving interactive 
                                     # plots from the tool as html files
               
               fastmatch,            # Used for fast matching, primarily '%fin%' as replacement to '%in%'
               
               parallel,             # Used for parallel computing NOTE!!! this is the most likely of the packages to cause issues
                                     # if you have issues regarding this one just comment it out and set 'parallel' to FALSE
                                     # in the gdx_extract_all() function below 
               
               processx,             # oof, no idea, I think it's extraneous but will check it out later
               
               units,                # needed for 'sf' package, when the pop up comes for installing with compilation hit no.
               
               sf,                   # all of the following packages are used to manipulate shape files and create country maps
               raster, 
               sp, 
               rgeos,
               maptools,
               scales,
               rgdal,
               
               gdxrrw,               # needed to interact with GAMS .gdx files
               
               shiny,                # Used to actually render the shiny output
               shinythemes)


# --------------------------------------------- /
# point to appropriate directories
# --------------------------------------------- /

# Set working directory as needed
setwd("/../Users/tbowen/Documents/Github/InteractiveDataVisualizer")

gams.directory <- "/../../../../../../Program Files (x86)/GAMS/win64" # should point to your installation of GAMS

base.directory <- "ReEDS - Visualization Tool"         # should point to the folder where all this data is stored (relative to current directory)

functions.directory <- file.path(base.directory, "functions") # Should point to folder with all non-shiny scripts

basicdata.directory <- file.path(base.directory, "basic data") # Should point to folder with all the csv data

server.directory <- file.path(base.directory, "server") # Should point to folder with all shiny-server scripts

ui.directory <- file.path(base.directory, "ui") # Should point to folder with all shiny-ui scripts

data.folder <- "../capexp-india/ReEDS-2.0/gdxfiles" # should point to location of '.gdx' files

scenarios <- gsub(c(list.files(data.folder, pattern = "\\.gdx$")), # here you can select which scenarios you are interested in
                  pattern = "\\.gdx$", replacement = "", 
                  perl = T)[c(1,4,5)]  ### NOTE: the indexing here right on the end might need to change

shape.file.loc <- file.path(basicdata.directory, "mapping/IND_adm1gadmFinal_wKashWhole_WGS84.shp") # This should point directly to .shp file
# NOTE for now this is specific to India in that it excludes "Andaman_Nicobar" and "Lakshadweep" if you want to change this navigate
# to the 'mapping_data_and_functions.R' script

# --------------------------------------------- /
# Load in necessary functions, .RDS objects and .csv files
# --------------------------------------------- /

source(file.path(functions.directory, "read_basic_gdx_info.R"))
# This script executs 4 main tasks:
# 
#       1) runs a script 'create_gdx_str_list.R' which creates a list (gdx.structure) with the naming conventions we 
#           expect from GAMS and the naming convention we want to replace it with, this list is saved 
#           to the 'basic data/' folder after it's created so that you can examine it (with the 'readRDS()' function);
# 
#       2) runs a script 'gdxr_functions.R' which loads all functions needed by 'gdx_extract_all' 
#           function (below) to interact with the gdx files directly;
# 
#       3) runs a script 'start_gdxr.R' which will actually start up the gdxrrw package,
#             NOTE: you might have to change a directory or two here depending on where your GAMS folder is 
#             so that R can find your copy of GAMS;
# 
#       4) loads 2 functions which are used to interact with the .gdx files once they are loaded into R


###
source(file.path(functions.directory, "gdx_extract_all.R"))
# This script loads a function 'gdx_extract_all()' which will extract ALL of the data present in the 'gdx.structure' 
#     list (loaded above) from ALL of the given gdx files in the scenarios variable


###
source(file.path(functions.directory, "read_basic_nongdx_info.R"))
# This script loads the csv's which contain data on the states and technology types, the years and the 
#     color coordination for the data and plots. 
#     NOTE: These csv's will allow non-R users to manipulate how the data is rearranged and aggregated
#           for the technologies and state data, there is a column for user defined aggregations


###
source(file.path(functions.directory, "mapping_data.R"))
# This script reads in the shape file and formats it a bit


###
source(file.path(basicdata.directory, "plot_parameters.R"))
# This script loads some basic plotting parameters (as a list), 
# so far this is all R files, but my intention is to change this over to a csv for non-r-user manipulation


###
source(file.path(functions.directory, "plot_data_simplification.R"))
# This script loads in a series of functions all of which are used to process the data before plotting
# see actual script for more description


###
source(file.path(functions.directory, "plot_functions.R"))
# This script loads in a series of functions for plotting
# see actual script for more description


###
source(file.path(functions.directory, "sub_select_csv.R"))
# This script loads a function 'select_cat_col_cat_ord()' which takes the csv files loaded above and user-inputs from the shiny
#     to create a list of data.tables with the original values and the new desired values. This lsit is then used by the functions 
#     loaded in the "plot_data_simplification.R" to process/simplify the data


# --------------------------------------------- /
# Load in gdx files
# --------------------------------------------- /

# The below variable contains all the gdx data specified by the 'gdx.structure' list loaded above
# This can take a bit based on the number of scenarios being loaded in
# If you are feeling brave, set parallel = TRUE to activate parallel computing to speed up loading of data
# 5 scenarios = ~40 seconds with parallel = FALSE and ~15 seconds with parallel = TRUE

# NOTE IF YOU RUN parallel = TRUE, YOU WILL NEED TO MANUALLY CHANGE several directories in ''gdx_extract_all.R'

gdx_all_var <- gdx_extract_all(scenario = scenarios, parallel = TRUE)


# --------------------------------------------- /
# Load in shiny ui-server structure and run app
# --------------------------------------------- /

# The ui (User Interface) create the skeleton for the shiny but does not actually perform any real action
ui <- fluidPage(theme = shinytheme("readable"),
                
                tags$head(
                  tags$style(HTML("
                                  .shiny-output-error-validation {
                                  color: red;
                                  font-weight:bold;
                                  font-size:14px;
                                  }
                                  "))
                  ),
                
                titlePanel("ReEDS Interactive Scenario Comparison Dashboard"),
                sidebarLayout(
                  source(file.path(ui.directory, "sidebar.ui_new.R"), local = TRUE)$value, # creates a sidebar visible from all tabs
                  
                  mainPanel(
                    tabsetPanel(type = "tabs", id = "name",
                                source(file.path(ui.directory, "instructions.ui.R"),  local = TRUE)$value, # Instructions tab
                                source(file.path(ui.directory, "capacity.ui.R"),  local = TRUE)$value, # Capacity tab
                                source(file.path(ui.directory, "generation.ui.R"),  local = TRUE)$value, # Generation tab
                                source(file.path(ui.directory, "flow.ui.R"),  local = TRUE)$value # Transmission flow tab
                    ), width = 9
                  )
                )
                
)

# The server is the actual workhorse of the shiny, runs all the functions etc.
server <- function(input, output) {
  source(file.path(server.directory, "reactivedef.server.R"),  local = TRUE)$value # controls the sidebar
  source(file.path(server.directory, "capacity.server.R"),  local = TRUE)$value # controls the capacity tab
  source(file.path(server.directory, "generation.server.R"),  local = TRUE)$value # controls the generation tab
  source(file.path(server.directory, "flow.server.R"),  local = TRUE)$value # controls the transmission flow tab
}


# runs the app
shinyApp(ui = ui, server = server)
