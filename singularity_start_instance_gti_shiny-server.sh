#!/usr/bin/env bash

mkdir -p ~/Downloads
mkdir -p /tmp/shinylog 
mkdir -p /tmp/shinybookmark
singularity instance start \
--bind shiny:/srv/shiny-server \
--bind /tmp/shinylog:/var/log/shiny-server/ \
--bind /tmp/shinybookmark:/var/lib/shiny-server/ \
--bind /home/$USER/Downloads \
--bind shiny-server.conf:/etc/shiny-server/shiny-server.conf \
/home/$USER/gti.sif gti
