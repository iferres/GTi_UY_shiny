#!/usr/bin/env bash

mkdir -p /export/home/$USER/shinylog 
mkdir -p /export/home/$USER/shinybookmark
singularity run \ 
--bind GTi_UY_shiny:/srv/shiny-server \ 
--bind /export/home/$USER/shinylog:/var/log/shiny-server/ \
--bind /export/home/$USER/shinybookmark:/var/lib/shiny-server/ \
--bind /export/home/$USER/Downloads \
--bind shiny-server.conf:/etc/shiny-server/shiny-server.conf \
/export/home/$USER/gti.sif
