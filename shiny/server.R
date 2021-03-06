

shinyServer(function(input, output, session){
  
  #######################
  ## READ LATEST DATA ##
  ######################
  
  x <- readRDS("~/Downloads/x.rds")
  NIDS <- readRDS("~/Downloads/NIDS.rds")
  NSEQ <- readRDS("~/Downloads/NSEQ.rds")
  totalesV <- readRDS("~/Downloads/totalesV.rds")
  xS <- readRDS("~/Downloads/xS.rds")
  totalesS <- readRDS("~/Downloads/totalesS.rds")
  
  # readdata <- function(times = 3){
  #   on.exit( gs4_deauth() )
  #   gs4_auth(path = "../.secrets/vigilanciagenomicauy-e474fd4f07d4.json", use_oob = TRUE)
  #   x <- try(read_sheet("1CP4TG44tYZ860SynTpwlTq1LtAdsq9KF9LcyrsMtrP4", skip = 1, sheet = "Hoja 1"))
  #   times <- times - 1L
  #   while (times>0 & class(x)=="try-error"){
  #     gs4_auth(cache = "../.secrets/vigilanciagenomicauy-e474fd4f07d4.json", use_oob = TRUE)
  #     x <- try(read_sheet("1CP4TG44tYZ860SynTpwlTq1LtAdsq9KF9LcyrsMtrP4", skip = 1, sheet = "Hoja 1"))
  #     times <- times - 1L
  #   }
  #   x
  # }
  # 
  # # show_modal_spinner(text = "Descargando datos...")
  # 
  # x <- readdata(times = 3)
  # gs4_deauth()
  
  
  
  
  #########
  ## UIs ##
  #########
  output$fechas <- renderUI({
    dateRangeInput(
      "fechas",
      label = "Zoom intervalo de fechas:",
      min = min(x$`Fecha de diagnóstico2`),
      max = max(x$`Fecha de diagnóstico2`),
      start = "2021-02-24",
      end = max(x$`Fecha de diagnóstico2`),
      format = "dd-mm-yyyy",
      language = "es",
      width = "300px"
    )
  })
  
  output$departamentos <- renderUI({
    pickerInput(
      "departamentos",
      label = "Departamentos",
      choices = c("Todos", as.character(unique(x$Departamento))), 
      selected = "Todos",
      multiple = FALSE,
      width = "300px"
    )
  })
  
  
  ################
  ## SHINY CODE ##
  ################
  
  X <- reactiveVal(x)
  #XS <- reactiveVal(xS)
  win <- reactiveVal(7)
  winS <- reactiveVal(7)
  colorbyV <- reactiveVal("Variante")
  colorbyS <- reactiveVal("Linaje")
  scaleV <- reactiveVal("Conteo")
  scaleS <- reactiveVal("Conteo")
  
  
  observeEvent(input$color_byV, {
    colorbyV(input$color_byV)
  })
  
  observeEvent(input$color_byS, {
    colorbyS(input$color_byS)
  })
  
  observeEvent(input$scale_V, {
    scaleV(input$scale_V)
  })
  
  observeEvent(input$scale_S, {
    scaleS(input$scale_S)
  })
  
  observeEvent({
    input$fechas 
    input$departamentos
  }, {
    if (input$departamentos == "Todos"){
      X(x[which( x$`Fecha de diagnóstico2` >= input$fechas[1] &
                   x$`Fecha de diagnóstico2` <= input$fechas[2] ), , drop = FALSE])
    }else{
      X(x[which( x$`Fecha de diagnóstico2` >= input$fechas[1] &
                   x$`Fecha de diagnóstico2` <= input$fechas[2] &
                   x$Departamento == input$departamentos), , drop = FALSE])
    }
  })
  
  # observeEvent(input$fechas, {
  #   X(x[which( x$`Fecha de diagnóstico2`>= input$fechas[1] & 
  #                x$`Fecha de diagnóstico2`<= input$fechas[2] ), , drop = FALSE])
  # })
  
  observeEvent(input$window, {
    win(input$window)
  })
  
  ## VALUE BOXES 
  output$num_qpcr <- renderValueBox({
    valueBox(NIDS, 
             subtitle = "Número de muestras ingresadas", 
             color = "blue")
  })
  output$num_seq <- renderValueBox({
    valueBox(NSEQ,
             subtitle = "Número de muestras secuenciadas",
             color = "blue")
  })
  output$last_update <- renderValueBox({
    valueBox(format(max(x$`Fecha de diagnóstico2`), format="%d/%m/%Y"),
             subtitle = "Fecha de diagóstico más reciente", 
             color = "blue")
  })
  
  
  ## PALETTE
  vars <- as.character(unique(xS$Linaje.poreCov))
  vcls <- colorRampPalette(c(pal_npg()(10), pal_igv()(10)[-2]))(length(vars))
  pal <- setNames(vcls, vars)
  pal <- c(pal, "P.1/B.1.351" = pal[["P.1"]], "No-VOC"="#00A087FF", 
           "B.1.1.7/BA.1" = pal[["BA.1"]], 
           "P.1/B.1.351/BA.2" = pal[["BA.2"]])
  pal["<NA>"] <- "grey"
  
  # vocs <- c("P.1", "P.1/B.1.351", "P.2", "B.1.1.7", "B.1.617.2", "No-VOC")
  # vocls <- c("#DC0000FF", "#DC0000FF", "#E64B35FF", "#F39B7FFF", "#DC0050FF", "#00A087FF")
  # novoc <- as.character(unique(xS$Linaje.poreCov)[!unique(xS$Linaje.poreCov) %in% vocs])
  # nwpl <- c(pal_npg()(10), pal_igv()(10)[-2])
  # novcls <- nwpl[-which(nwpl %in% vocls)][seq_along(novoc)]
  # vars <- c(vocs, novoc)
  # pal <- setNames(c(vocls, novcls), vars)
  
  ## LEAFLET MAP  
  output$mapV <- renderLeaflet({
    
    remove_start_up()
    
    leaflet(departamentos, options = leafletOptions(zoomControl=FALSE, minZoom = 7, maxZoom = 7)) %>%
      addPolygons(color = "black",
                  fillColor = colorNumeric("Greens", domain = NULL)(totalesV$Total),
                  weight = 1, 
                  smoothFactor = 0.5,
                  opacity = 1.0, 
                  fillOpacity = 0.5,
                  label = paste0(rownames(totalesV), ": ", totalesV$Total)) %>% # popup dep name
      addMinicharts(lng = cent[, 1], 
                    lat = cent[, 2],
                    colorPalette = unname(pal[c("No-VOC", "P.1/B.1.351", "B.1.1.7", "B.1.617.2",
                                                "B.1.1.7/BA.1",
                                                "P.1/B.1.351/BA.2")]),
                    chartdata = totalesV[,c("No-VOC", "P.1/B.1.351", "B.1.1.7", "B.1.617.2",
                                            "B.1.1.7/BA.1",
                                            "P.1/B.1.351/BA.2")],
                    type = "pie", 
                    showLabels = TRUE, 
                    height = 50, 
                    width = 50)
  })
  
  
  output$barV <- renderPlotly({
    df <- as.data.frame(table(x$Semana, x$`Variante por PCR`, x$Departamento, x$Sexo))
    names(df) <- c("Semana", "Variante", "Departamento", "Sexo", "Conteo")
    # df$Departamento <- factor(df$Departamento, levels = departamentos$admlnm)
    
    ln <- length(levels(df[[colorbyV()]]))
    if (ln>10){
      pals <- colorRampPalette(pal_npg()(10))(ln)
    }else if(colorbyV() == "Variante"){
      pals <-pal[c("No-VOC", "P.1/B.1.351", "B.1.1.7", "B.1.617.2",
                   "B.1.1.7/BA.1",
                   "P.1/B.1.351/BA.2")]
    }else{
      pals <- pal_npg()(ln)
    }
    
    if (scaleV() == "Conteo"){
      
      g6 <- ggplot(df, aes(x=Semana, fill = !! sym(colorbyV()))) + 
        stat_summary(aes(y = Conteo), fun = "sum", geom = "bar", position = "stack") + 
        scale_fill_manual(values = pals) + 
        theme_bw() +
        theme(axis.text.x = element_text(angle = 35, vjust = 0.5))
      
    } else {
      
      g6 <- ggplot(df, aes(x=Semana, fill = !! sym(colorbyV()))) + 
        stat_summary(aes(y = Conteo), fun = "sum", geom = "bar", position = "fill") + 
        scale_fill_manual(values = pals) + 
        theme_bw() +
        theme(axis.text.x = element_text(angle = 35, vjust = 0.5)) + 
        scale_y_continuous(labels = scales::percent) + 
        ylab("Porcentaje")
      
    }
    
    
    
    ggplotly(g6)
    
  })
  
  # torm <- which(is.na(x$Edad))
  # if (length(torm)){
  #   x <- x[-torm, ]
  # }
  
  ## SCATTER DENSITY ##
  output$scat <- renderPlotly({
    g3 <- ggplot(X(), aes(x=`Fecha de diagnóstico2`, y = Edad, color = `Variante por PCR`)) + 
      geom_point() + 
      ylab("Edad") + 
      scale_color_manual(values = pal[levels(x$`Variante por PCR`)], drop = FALSE, guide = FALSE) +
      theme_bw()
    g4 <- ggplot(X(), aes(x = `Fecha de diagnóstico2`)) + 
      geom_density(aes(fill = `Variante por PCR`), alpha = 0.5) + 
      scale_fill_manual(values = pal[levels(x$`Variante por PCR`)], drop = FALSE, guide = FALSE) +
      theme_void()
    g5 <- ggplot(X(), aes(x = Edad)) + 
      geom_density(aes(fill = `Variante por PCR`), alpha = 0.5) + 
      scale_fill_manual(values = pal[levels(x$`Variante por PCR`)], drop = FALSE, guide = FALSE) +
      theme_void() + 
      coord_flip() 
    subplot(ggplotly(g4), plotly_empty(),
            ggplotly(g3) , ggplotly(g5), 
            nrows = 2, 
            shareX = T, 
            shareY = T, 
            heights = c(0.2, 0.8), 
            widths = c(0.9, 0.1),
            margin = 0) %>% 
      hide_legend() %>% 
      layout(xaxis=list(title="Fecha de diagnóstico"))
  })
  
  
  ## STEP PLOT
  output$cumulative <- renderPlotly({
    g1 <- ggplot(X(), aes(x=`Fecha de diagnóstico2`, color=`Variante por PCR`, y = conteo_variante)) + 
      geom_step() + scale_color_manual(values = pal[levels(x$`Variante por PCR`)], drop = F) + 
      theme_bw() + 
      xlab("Fecha de diagnóstico") + 
      ylab("Conteo")
    ggplotly(g1) %>% 
      hide_legend()
  })
  
  ## AREA PLOT
  # output$area <- renderPlotly({
  #   g2 <- by(X()[c("Departamento", "Variante por PCR", "Fecha de diagnóstico2", "conteo_variante")], X()$`Fecha de diagnóstico2`, function(y){
  #     max <- by(y, y$`Variante por PCR`, function(z) {
  #       wma <- which.max(z$conteo_variante)
  #       z <- z[wma,]
  #     }) %>% do.call(rbind, .)
  #     lvl <- levels(max$`Variante por PCR`)
  #     wlv <- which(! lvl %in% max$`Variante por PCR`)
  #     ln <- length(wlv)
  #     if (ln){
  #       ad <- list(Departamento = rep(NA_character_, ln), 
  #                  `Variante por PCR` = lvl[wlv], 
  #                  `Fecha de diagnóstico2` = rep(max$`Fecha de diagnóstico2`[1], ln),
  #                  conteo_variante = rep(0L, ln))
  #       max <- rbind(max, ad)
  #     }
  #     max$porcentaje <- proportions(max$conteo_variante) * 100
  #     max
  #   }) %>%
  #     do.call(rbind, .) %>% 
  #     ggplot(aes(x = `Fecha de diagnóstico2`, 
  #                y = porcentaje, 
  #                fill = `Variante por PCR`, 
  #                group = `Variante por PCR`)) + 
  #     geom_area() + 
  #     theme_bw()
  # 
  #   ggplotly(g2)
  # })
  
  output$area <- renderPlotly({
    ww <- cut(X()$`Fecha de diagnóstico2`, breaks = paste(win(), "days"), labels = FALSE)
    g2 <- X() %>% by(ww, function(y) y ) %>% 
      lapply(function(y){
        data.frame(
          as.list(proportions(table(y$`Variante por PCR`)) * 100),
          list(Day = min(y$`Fecha de diagnóstico2`))
        )
      }) %>%
      do.call(rbind, .)  %>%
      setNames(c("No-VOC", "P.1/B.1.351", "B.1.1.7", "B.1.617.2",
                 "B.1.1.7/BA.1",
                 "P.1/B.1.351/BA.2" ,"Day")) %>%
      melt(id.vars = "Day", measure.vars = c("No-VOC", "P.1/B.1.351", "B.1.1.7", "B.1.617.2",
                                             "B.1.1.7/BA.1",
                                             "P.1/B.1.351/BA.2")) %>%
      ggplot(aes(x = Day,
                 y = value,
                 fill = variable,
                 group = variable)) +
      geom_area() +
      scale_fill_manual(values = pal[levels(x$`Variante por PCR`)], drop = F) +
      theme_bw() + 
      xlab("Fecha de diagnóstico") + 
      ylab("Porcentaje")
    
      ggplotly(g2) %>% 
      hide_legend()
  })
  
  
  ## HEATMAP
  # hm <- by(x, x$`Fecha de diagnóstico2`, function(y){
  #   lst <- list(
  #     Menores = which(y$Edad < 18),
  #     Adultos = which(y$Edad >= 18 & y$Edad < 60),
  #     Mayores = which(y$Edad >= 60)
  #   )
  #   lp <- lapply(lst, function(z){
  #     tb <- table(y$`Variante por PCR`[z])
  #     RatioVOC <- (tb[["P.1/B.1.351"]] + tb[["B.1.1.7"]]) / sum(tb) 
  #     if (is.nan(RatioVOC)) RatioVOC <- NA_real_ else RatioVOC
  #   }) 
  # }) %>% sapply(I) %>% as.data.frame()
  # hm[] <- lapply(hm, unlist)
  # # hm <- melt(as.matrix(hm))
  # # hm$Var2 <- as.Date(as.character(hm$Var2))
  # # ggplot(hm, aes(x = Var2, y = Var1)) + geom_tile(aes(fill = value))
  # heat <- heatmaply(hm, Rowv = F, Colv = F, colors = colorRampPalette(c("chartreuse4", "yellow", "firebrick3"))(50))
  # # output$heat <- 
  
  
  # Scatter density
  # x2 <- x[-which(is.na(x$`Fecha de diagnóstico2`) | is.na(x$Edad))]
  
  
  ## LEAFLET MAP  
  output$mapS <- renderLeaflet({
    leaflet(departamentos, options = leafletOptions(zoomControl=FALSE, minZoom = 7, maxZoom = 7)) %>%
      addPolygons(color = "black",
                  fillColor = colorNumeric("Greens", domain = NULL)(totalesS$Total),
                  weight = 1, 
                  smoothFactor = 0.5,
                  opacity = 1.0, 
                  fillOpacity = 0.5,
                  label = paste0(rownames(totalesS), ": ", totalesS$Total)) %>% # popup dep name
      addMinicharts(lng = cent[, 1], 
                    lat = cent[, 2],
                    colorPalette = unname(pal[colnames(totalesS)[-dim(totalesS)[2]]]),
                    chartdata = totalesS[,-dim(totalesS)[2]],
                    type = "pie", 
                    showLabels = TRUE, 
                    height = 50, 
                    width = 50)
  })
  
  
  output$barS <- renderPlotly({
    df <- as.data.frame(table(xS$Semana, xS$Linaje.poreCov, xS$Departamento, xS$Sexo))
    names(df) <- c("Semana", "Linaje", "Departamento", "Sexo", "Conteo")
    # df$Departamento <- factor(df$Departamento, levels = departamentos$admlnm)
    
    ln <- length(levels(df[[colorbyS()]]))
    if(colorbyS() == "Linaje"){
      pals <- pal[colnames(totalesS)[-dim(totalesS)[2]]]
    }else if(ln>10){
      pals <- colorRampPalette(pal_npg()(10))(ln)
    }else{
      pals <- pal_npg()(ln)
    }
    
    if (scaleS() == "Conteo"){
      
      g7 <- ggplot(df, aes(x=Semana, fill = !! sym(colorbyS()))) + 
        stat_summary(aes(y = Conteo), fun = "sum", geom = "bar", position = "stack") + 
        scale_fill_manual(values = pals) + 
        theme_bw() +
        theme(axis.text.x = element_text(angle = 35, vjust = 0.5)) 
      
    } else {
      
      g7 <- ggplot(df, aes(x=Semana, fill = !! sym(colorbyS()))) + 
        stat_summary(aes(y = Conteo), fun = "sum", geom = "bar", position = "fill") + 
        scale_fill_manual(values = pals) + 
        theme_bw() +
        theme(axis.text.x = element_text(angle = 35, vjust = 0.5)) + 
        scale_y_continuous(labels = scales::percent) + 
        ylab("Porcentaje")
      
    }
    
    
    ggplotly(g7)
    
  })
  
  
  observeEvent(input$windowS, {
    winS(input$windowS)
  })
  
  
  output$areaS <- renderPlotly({
    ww <- cut(xS$`Fecha de diagnóstico2`, breaks = paste(winS(), "days"), labels = FALSE)
    g8 <- xS %>% by(ww, function(y) y ) %>%
      lapply(function(y){
        data.frame(
          as.list(proportions(table(factor(y$Linaje.poreCov, levels = names(pal)))) * 100),
          list(Day = min(y$`Fecha de diagnóstico2`))
        )
      }) %>%
      do.call(rbind, .)  %>%
      melt(id.vars="Day") %>%
      ggplot(aes(x = Day,
                 y = value,
                 fill = variable,
                 group = variable)) +
      geom_area() +
      scale_fill_manual(values = pal, drop = F) +
      theme_bw() + 
      xlab("Fecha de diagnóstico") + 
      ylab("Porcentaje")
    
    ggplotly(g8) %>% 
      hide_legend()
  })
  
  output$seqdata <- renderDT({
    datatable(xS[, c("ID Consorcio", "Ct", "Departamento", "Sexo", "QC_FINAL", "Mutaciones", "Deleciones", "Linaje.poreCov")])
  })
    
  
})
