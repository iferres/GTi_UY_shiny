#! /usr/bin/env Rscript

# Script to download data with crontab
# crontab -e
# 5 * * * * Rscript ~/GTi_UY_shiny/data/Download.Rscript.R

library(googlesheets4)

readdata <- function(times = 3){
  on.exit( gs4_deauth() )
  gs4_auth(path = "../.secrets/vigilanciagenomicauy-e474fd4f07d4.json", use_oob = TRUE)
  x <- try(read_sheet("1CP4TG44tYZ860SynTpwlTq1LtAdsq9KF9LcyrsMtrP4", skip = 1, sheet = "Hoja 1"))
  times <- times - 1L
  while (times>0 & class(x)=="try-error"){
    gs4_auth(cache = "../.secrets/vigilanciagenomicauy-e474fd4f07d4.json", use_oob = TRUE)
    x <- try(read_sheet("1CP4TG44tYZ860SynTpwlTq1LtAdsq9KF9LcyrsMtrP4", skip = 1, sheet = "Hoja 1"))
    times <- times - 1L
  }
  x
}


x <- readdata(times = 3)
gs4_deauth()

saveRDS(x, "~/drive_data.rds")