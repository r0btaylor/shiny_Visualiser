# To run this script, select ">Run App" (Top right of this pane)

# Script will install all packages before loading visualizer application

# ** This may take some time on first run **
# Subsequent runs will be quicker as no package installation will be required

if(!require("shiny")) {
  install.packages("shiny")
  library(shiny)
} 
runApp("hwiseVis")