rm(list=ls())

library(pacman)


p_load("shiny","shinythemes","foreign","data.table","leaflet","leaflet.extras",
       "zoo","ggplot2", "riem")
p_load_gh('ramnathv/rCharts')

source("data_read_functions.R")
source("get_external_data.R")


df=NULL