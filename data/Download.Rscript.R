#! /usr/bin/env Rscript

# Script to download data with crontab
# crontab -e
# 5 * * * * Rscript ~/GTi_UY_shiny/data/Download.Rscript.R

library(googlesheets4)
library(stringi)
library(sf)

readdata <- function(times = 3){
  on.exit( gs4_deauth() )
  gs4_auth(path = "~/.secrets/vigilanciagenomicauy-e474fd4f07d4.json", use_oob = TRUE)
  x <- try(read_sheet("1CP4TG44tYZ860SynTpwlTq1LtAdsq9KF9LcyrsMtrP4", skip = 1, sheet = "Hoja 1"))
  times <- times - 1L
  while (times>0 & class(x)=="try-error"){
    gs4_auth(cache = "~/.secrets/vigilanciagenomicauy-e474fd4f07d4.json", use_oob = TRUE)
    x <- try(read_sheet("1CP4TG44tYZ860SynTpwlTq1LtAdsq9KF9LcyrsMtrP4", skip = 1, sheet = "Hoja 1"))
    times <- times - 1L
  }
  x
}


x <- readdata(times = 3)
gs4_deauth()

departamentos <- st_read("~/GTi_UY_shiny/data/Departamentos.shp")


## setear linaje_nov21 como Linaje.poreCov para mostrar lo más actualizado
x$Linaje.poreCov <- x$linaje_nov21



################
## PARSE DATA ##
################

# Muestras procesadas
NIDS <- length(unique(x$`ID Consorcio`))
NSEQ <- length(which(x$Secuenciación == "Si"))



# x <- readRDS("data/toy.RDS")

# Remove "Descartada".
torm <- which(x$`Status qPCR` != "Finalizada")
if (length(torm)){
  x <- x[-torm, ]
}

# Remove without Departamento info.
# torm <- which(is.na(x$Departamento))
# if (length(torm)){
#   x <- x[-torm, ]
# }

torm <- which(x$`PCR variantes` == "No")
if (length(torm)){
  x <- x[-torm, ]
}

# Remove tildes and match names with shp file
x$Departamento <- stri_trans_general(x$Departamento, "Latin-ASCII")
x$Departamento <- sapply(x$Departamento,  grep, departamentos$admlnm, value = TRUE, ignore.case = T)
x$Departamento <- factor(x$Departamento, levels = departamentos$admlnm)

# Factorize variants and remove NAs
x$`Variante por PCR` <- factor(x$`Variante por PCR`, levels = c("No-VOC", "P.1/B.1.351", "B.1.1.7", "B.1.617.2"))
torm <- which(is.na(x$`Variante por PCR`))
if (length(torm)){
  x <- x[-torm, ]
}

# Sort by Fecha de hisopado/diagnóstico
# x$`Fecha de diagnóstico` <- as.Date(x$`Fecha de diagnóstico`)
# x$`Fecha de hisopado` <- as.Date(x$`Fecha de hisopado`)
x$`Fecha de diagnóstico2` <- apply(x, 1, function(y){
  fh <- y[["Fecha de hisopado"]]
  fd <- y[["Fecha de diagnóstico"]]
  if (!is.na(fh)){
    res <- fh
  } else if (!is.na(fd)) {
    res <- fd
  } else {
    res <- fh
  }
  res
}) %>% 
  as.POSIXct(origin="1970-01-01") %>%
  as.Date()

x <- x[order(x$`Fecha de diagnóstico2`), ]

# Add count per Departamento
x <- by(x, x$`Variante por PCR`, function(y){
  y$conteo_variante <- seq_len(nrow(y))
  y
}) %>%
  do.call(rbind, .)

# Sort by Fecha de diagnóstico
x <- x[order(x$`Fecha de diagnóstico2`), ]

x$Año <- strftime(x$`Fecha de diagnóstico2`, format = "%Y")
x$Semana_diagnostico <- strftime(x$`Fecha de diagnóstico2`, format = "%V")
x$`Semana Epidemiológica` <- cut(x$`Fecha de diagnóstico2`, breaks = "1 week", labels = F)
x$Semana <- as.Date(x$Semana)

torm <- which(is.na(x$`Fecha de diagnóstico2`))
if (length(torm)){
  x <- x[-torm, ]
}

totalesV <- table(x$Departamento, x$`Variante por PCR`)
class(totalesV) <- "matrix"
totalesV <- as.data.frame(totalesV)
totalesV$Total <- rowSums(totalesV)

#Only which pass QC
xS <- x[which(x$QC_FINAL %in% c("PASS_0-10", "WARNING_10-30")), ]
xS$Linaje.poreCov <- factor(xS$Linaje.poreCov)
totalesS <- table(xS$Departamento, xS$Linaje.poreCov)
class(totalesS) <- "matrix"
totalesS <- as.data.frame(totalesS)
totalesS$Total <- rowSums(totalesS)
# remove_modal_spinner()

saveRDS(x, "~/Downloads/x.rds")
saveRDS(NIDS, "~/Downloads/NIDS.rds")
saveRDS(NSEQ, "~/Downloads/NSEQ.rds")
saveRDS(totalesV, "~/Downloads/totalesV.rds")
saveRDS(xS, "~/Downloads/xS.rds")
saveRDS(totalesS, "~/Downloads/totalesS.rds")
