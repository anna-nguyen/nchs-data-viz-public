#################################
# Load directories
#################################
renv::restore()

library(tidyverse)
library(ggplot2)
library(stringr)
library(tidycensus)
library(here)
library(lubridate)
library(haven)
library(NatParksPalettes)
library(shiny)
library(gridExtra)
library(plotly)
library(shinyWidgets)


# Spatial packages
library(ncdf4)
library(rgeos)
library(raster)
library(tigris)
library(sf)

#################################
# Set up data directories
#################################
data_dir = here::here("data/")
