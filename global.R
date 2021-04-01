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

departamentos <- st_read("data/Departamentos.shp")
# Compute centroid
cent <- st_centroid(departamentos)$geometry
cent <- do.call(rbind, cent)

gs4_auth(cache = ".secrets/", email = TRUE)
# Sys.sleep(3)
x <- read_sheet("1sYmvW0xiN86s_PuGtRwLSLGOT-XQr2SzVeITdjSQNvA", skip = 1)
gs4_deauth()

# x <- readRDS("data/toy.RDS")

# Remove "Descartada".
torm <- which(x$Estado == "Descartada")
if (length(torm)){
  x <- x[-torm, ]
}

# Remove without Departamento info.
torm <- which(is.na(x$Departamento))
if (length(torm)){
  x <- x[-torm, ]
}

# Remove tildes and match names with shp file
x$Departamento <- stri_trans_general(x$Departamento, "Latin-ASCII")
x$Departamento <- sapply(x$Departamento,  grep, departamentos$admlnm, value = TRUE, ignore.case = T)

# Factorize variants and remove NAs
x$`Variante por PCR` <- factor(x$`Variante por PCR`, levels = c("No-VOC", "P.1/B.1.351", "B.1.1.7"))
torm <- which(is.na(x$`Variante por PCR`))
if (length(torm)){
  x <- x[-torm, ]
}

# Sort by Fecha de diagnóstico
x$`Fecha de diagnóstico` <- as.Date(x$`Fecha de diagnóstico`)
x <- x[order(x$`Fecha de diagnóstico`), ]

# Add count per Departamento
x <- by(x, x$`Variante por PCR`, function(y){
  y$conteo_variante <- seq_len(nrow(y))
  y
}) %>%
  do.call(rbind, .)

# Sort by Fecha de diagnóstico
x <- x[order(x$`Fecha de diagnóstico`), ]

x$Año <- strftime(x$`Fecha de diagnóstico`, format = "%Y")
x$Semana <- strftime(x$`Fecha de diagnóstico`, format = "%V")
x$`Semana Epidemiológica` <- cut.Date(x$`Fecha de diagnóstico`, breaks = "1 week", labels = F)

torm <- which(is.na(x$`Fecha de diagnóstico`))
if (length(torm)){
  x <- x[-torm, ]
}

totales <- sapply(departamentos$admlnm, function(y) {
  wh <- which(x$Departamento == y)
  if (length(wh)){
    table(x$`Variante por PCR`[wh])
  } else {
    table(factor(NA_character_, levels = c("No-VOC", "P.1/B.1.351", "B.1.1.7")))
  }
})
totales <- as.data.frame(t(totales))
totales$Total <- rowSums(totales)

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




