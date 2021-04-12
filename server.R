shinyServer(function(input, output, session){
  
  X <- reactiveVal(x)
  win <- reactiveVal(7)
  colorbyV <- reactiveVal("Variante")
  colorbyS <- reactiveVal("Linaje")
  
  
  observeEvent(input$color_byV, {
    colorbyV(input$color_byV)
  })
  
  observeEvent(input$color_byS, {
    colorbyS(input$color_byS)
  })
  
  observeEvent(c(input$fechas, input$departamentos), {
    if (input$departamentos == "Todos"){
      X(x[which( x$`Fecha de diagnóstico`>= as.Date(input$fechas[1]) &
                   x$`Fecha de diagnóstico`<= as.Date(input$fechas[2]) ), , drop = FALSE])
    }else{
      X(x[which( x$`Fecha de diagnóstico`>= as.Date(input$fechas[1]) &
                   x$`Fecha de diagnóstico`<= as.Date(input$fechas[2]) &
                   x$Departamento == input$departamentos), , drop = FALSE])
    }
  })
  
  # observeEvent(input$fechas, {
  #   X(x[which( x$`Fecha de diagnóstico`>= input$fechas[1] & 
  #                x$`Fecha de diagnóstico`<= input$fechas[2] ), , drop = FALSE])
  # })
  
  observeEvent(input$window, {
    win(input$window)
  })
  
  
  output$num_qpcr <- renderValueBox({
    valueBox(sum(totalesV$Total), 
             subtitle = "Número de muestras por qPCR", 
             color = "green")
  })
  output$num_seq <- renderValueBox({
    valueBox(sum(totalesS$Total),
             subtitle = "Número de muestras secuenciadas",
             color = "green")
  })
  
  
  ## LEAFLET MAP  
  output$mapV <- renderLeaflet({
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
                    colorPalette = hue_pal()(3),
                    chartdata = totalesV[,c("No-VOC", "P.1/B.1.351", "B.1.1.7")],
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
      pals <- hue_pal()(3)
    }else{
      pals <- pal_npg()(ln)
    }
    
    g6 <- ggplot(df, aes(x=Semana, fill = !! sym(colorbyV()))) + 
      stat_summary(aes(y = Conteo), fun = "sum", geom = "bar", position = "stack") + 
      scale_fill_manual(values = pals) + 
      theme_bw()
    
    ggplotly(g6)
    
  })
  
  # torm <- which(is.na(x$Edad))
  # if (length(torm)){
  #   x <- x[-torm, ]
  # }
  
  ## SCATTER DENSITY ##
  output$scat <- renderPlotly({
    g3 <- ggplot(X(), aes(x=`Fecha de diagnóstico`, y = Edad, color = `Variante por PCR`)) + 
      geom_point() + 
      ylab("Edad") + 
      scale_color_discrete(drop = FALSE, guide = FALSE) +
      theme_bw()
    g4 <- ggplot(X(), aes(x = `Fecha de diagnóstico`)) + 
      geom_density(aes(fill = `Variante por PCR`), alpha = 0.5) + 
      scale_fill_discrete(drop = FALSE, guide = FALSE) +
      theme_void()
    g5 <- ggplot(X(), aes(x = Edad)) + 
      geom_density(aes(fill = `Variante por PCR`), alpha = 0.5) + 
      scale_fill_discrete(drop = FALSE, guide = FALSE) +
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
      hide_legend()
  })
  
  
  ## STEP PLOT
  output$cumulative <- renderPlotly({
    g1 <- ggplot(X(), aes(x=`Fecha de diagnóstico`, color=`Variante por PCR`, y = conteo_variante)) + 
      geom_step() + scale_color_discrete(drop = F) + theme_bw()
    ggplotly(g1) %>% 
      hide_legend()
  })
  
  ## AREA PLOT
  # output$area <- renderPlotly({
  #   g2 <- by(X()[c("Departamento", "Variante por PCR", "Fecha de diagnóstico", "conteo_variante")], X()$`Fecha de diagnóstico`, function(y){
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
  #                  `Fecha de diagnóstico` = rep(max$`Fecha de diagnóstico`[1], ln),
  #                  conteo_variante = rep(0L, ln))
  #       max <- rbind(max, ad)
  #     }
  #     max$porcentaje <- proportions(max$conteo_variante) * 100
  #     max
  #   }) %>%
  #     do.call(rbind, .) %>% 
  #     ggplot(aes(x = `Fecha de diagnóstico`, 
  #                y = porcentaje, 
  #                fill = `Variante por PCR`, 
  #                group = `Variante por PCR`)) + 
  #     geom_area() + 
  #     theme_bw()
  # 
  #   ggplotly(g2)
  # })
  
  output$area <- renderPlotly({
    ww <- cut.Date(X()$`Fecha de diagnóstico`, breaks = paste(win(), "days"), labels = FALSE)
    g2 <- X() %>% by(ww, function(y) y ) %>% 
      lapply(function(y){
        data.frame(
          as.list(proportions(table(y$`Variante por PCR`)) * 100),
          list(Day = min(y$`Fecha de diagnóstico`))
        )
      }) %>%
      do.call(rbind, .)  %>%
      melt(id.vars = "Day", measure.vars = c("No.VOC", "P.1.B.1.351", "B.1.1.7")) %>%
      ggplot(aes(x = Day,
                 y = value,
                 fill = variable,
                 group = variable)) +
      geom_area() +
      scale_fill_discrete(drop = F) +
      theme_bw()
    
      ggplotly(g2) %>% 
      hide_legend()
  })
  
  
  ## HEATMAP
  # hm <- by(x, x$`Fecha de diagnóstico`, function(y){
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
  # x2 <- x[-which(is.na(x$`Fecha de diagnóstico`) | is.na(x$Edad))]
  
  
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
                    colorPalette = hue_pal()(length(levels(xS$Linaje.poreCov))),
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
    if (ln>10){
      pals <- colorRampPalette(pal_npg()(10))(ln)
    }else if(colorbyS() == "Linaje"){
      pals <- hue_pal()(length(levels(xS$Linaje.poreCov)))
    }else{
      pals <- pal_npg()(ln)
    }
    
    g7 <- ggplot(df, aes(x=Semana, fill = !! sym(colorbyS()))) + 
      stat_summary(aes(y = Conteo), fun = "sum", geom = "bar", position = "stack") + 
      scale_fill_manual(values = pals) + 
      theme_bw()
    
    ggplotly(g7)
    
  })
    
  
  
  
})