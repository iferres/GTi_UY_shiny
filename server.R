shinyServer(function(input, output, session){
  
  X <- reactiveVal(x)
  win <- reactiveVal(7)
  
  observeEvent(c(input$fechas, input$departamentos), {
    if (input$departamentos == "Todos"){
      X(x[which( x$`Fecha de diagnóstico`>= input$fechas[1] &
                   x$`Fecha de diagnóstico`<= input$fechas[2] ), , drop = FALSE])
    }else{
      X(x[which( x$`Fecha de diagnóstico`>= input$fechas[1] &
                   x$`Fecha de diagnóstico`<= input$fechas[2] &
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
    valueBox(sum(totales$Total), 
             subtitle = "Número de muestras por qPCR", 
             color = "green")
  })
  output$num_seq <- renderValueBox({
    valueBox(table(x$Secuenciación)[["Si"]], 
             subtitle = "Número de muestras secuenciadas",
             color = "green")
  })
  
  ## LEAFLET MAP  
  output$map <- renderLeaflet({
    leaflet(departamentos) %>%
      addPolygons(color = "black",
                  fillColor = colorNumeric("Greens", domain = NULL)(totales$Total),
                  weight = 1, 
                  smoothFactor = 0.5,
                  opacity = 1.0, 
                  fillOpacity = 0.5, 
                  label = ~admlnm) %>% # popup dep name
      addMinicharts(lng = cent[, 1], 
                    lat = cent[, 2],
                    chartdata = totales[,c("No-VOC", "P.1/B.1.351", "B.1.1.7")],
                    type = "pie", showLabels = TRUE, height = 50, width = 50)
  })
  
  # torm <- which(is.na(x$Edad))
  # if (length(torm)){
  #   x <- x[-torm, ]
  # }
  
  ## STEP PLOT
  output$cumulative <- renderPlotly({
    g1 <- ggplot(X(), aes(x=`Fecha de diagnóstico`, color=`Variante por PCR`, y = conteo_variante)) + 
      geom_step() + scale_color_discrete(drop = F) + theme_bw()
    ggplotly(g1)
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
      theme_bw()
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
  
  output$scat <- renderPlotly({
    g3 <- ggplot(X(), aes(x=`Fecha de diagnóstico`, y = Edad, color = `Variante por PCR`)) + 
      geom_point() + 
      theme(legend.position = "bottom")
    g4 <- ggplot(X(), aes(x = `Fecha de diagnóstico`)) + 
      geom_density(aes(fill = `Variante por PCR`), alpha = 0.5) + 
      theme_void() + 
      theme(legend.position = "none")
    g5 <- ggplot(X(), aes(x = Edad)) + 
      geom_density(aes(fill = `Variante por PCR`), alpha = 0.5) + 
      theme_void() + 
      theme(legend.position = "none") + 
      coord_flip()
    subplot(g4, plotly_empty(),
            g3 , g5, 
            nrows = 2, 
            heights = c(0.2, 0.8), 
            widths = c(0.8, 0.2),
            margin = 0)
  })
    
    
  
  
  
})