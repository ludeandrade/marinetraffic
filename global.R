#### --- global

# instal packages
requiredPackages <- c(
  "shiny",
  "shiny.semantic",
  "semantic.dashboard",
  "tidyr",
  "geosphere",
  "leaflet",
  "lubridate",
  "plotly",
  "testthat"
)

newPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]

if (length(newPackages)) {
  install.packages(newPackages)
}

# load libraries
library(shiny)
library(shiny.semantic)
library(tidyr)
library(geosphere)
library(leaflet)
library(lubridate)
library(plotly)
library(testthat)

# data
# rawData <- read.csv("Data/rawData.csv")
# 
# data <- rawData %>% mutate(SHIPNAME=toupper(SHIPNAME), ship_type=toupper(ship_type))
# 
# data$DATETIME <- data$DATETIME %>% 
#                  lapply(sub, pattern = "T", replacement = " ") %>% 
#                  lapply(sub, pattern = "Z", replacement = "") %>% 
#                  unlist() %>% 
#                  as_datetime()
# 
# data$SHIPNAME[data$SHIPNAME==". PRINCE OF WAVES"] <- "PRINCE OF WAVES"
# data$SHIPNAME[data$SHIPNAME==".WLA-311"] <- "WLA-311"

# palette
darkGrey <- "#3c3c3c" 
rgbDarkGrey <- "rgba(60, 60, 60, 0.5)"

lightGrey <- "#f6f6f6" 
rgbLightGrey <- "rgba(240, 240, 240, 0.5)"


# ui layout grid
mainGrid <- grid_template(
  default = list(
    areas = rbind(
      c("mapa",  "sidebar"),
      c("kpi",   "sidebar"),
      c("barra", "sidebar")
    ),
    rows_height = c( "4fr",
                     "1fr",
                     "2fr"),
    cols_width = c("4fr", "1fr")
  )
)

# sidebar column subgrid
subGridSidebar <- grid_template(
  default = list(
    areas = cbind(c("top", 
                    "middle",
                    "bottom")),
    rows_height = c("4fr", "4fr", "1fr"),
    cols_width = c( "100%")
  )
)

# kpi line subgrid
subGridKpi <- grid_template(
  default = list(
    areas = rbind(c("left", "right")),
    rows_height = c("100%"),
    cols_width = c("1fr", "1fr")
  )
)

#module
source("module.R")



