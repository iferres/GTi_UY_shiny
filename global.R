library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(sf)
library(googlesheets4)
library(magrittr)
library(stringi)
library(leaflet)
library(leaflet.minicharts)
library(reshape2)
library(scales)
library(ggsci)
library(RColorBrewer)

departamentos <- st_read("data/Departamentos.shp")
# Compute centroid
cent <- st_centroid(departamentos)$geometry
cent <- do.call(rbind, cent)
# fix Montevideo
cent[9, 1] <- cent[9, 1] - 0.3
cent[9, 2] <- cent[9, 2] - 0.1
# fix Tacuarembo
cent[2, 2] <- cent[2,2] + 0.2



# x <- by(x, x$`Fecha de diagnóstico`, function(y){
#   tpl <- tapply(y$conteo_variante, y$`Variante por PCR`, max)
#   nas <- is.na(tpl)
#   if (any(nas)){
#     tpl[nas] <- 0L
#   }
#   perc <- proportions(tpl) * 100
#   y$porcentaje <- perc[match(y$`Variante por PCR`, names(perc))]
#   y
# }) %>%
#   do.call(rbind, .)




