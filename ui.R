ui <- dashboardPage(
  skin = "green",
  
  header = dashboardHeader(
    title = "GTI - Vigilancia SARS-Cov-2"
  ),
  
  sidebar = dashboardSidebar(
    
    pickerInput(
      "departamentos",
      label = "Departamentos",
      choices = c("Todos", unique(x$Departamento)), 
      selected = "Todos",
      multiple = FALSE
    ),
    
    
    dateRangeInput(
      "fechas", 
      label = "Zoom intervalo de fechas:", 
      min = min(x$`Fecha de diagnóstico`), 
      max = max(x$`Fecha de diagnóstico`),
      start = as.Date("2021-02-24"), 
      end = max(x$`Fecha de diagnóstico`),
      format = "dd-mm-yyyy",
      language = "es"
    ),
    
    sliderInput(
      "window",
      label = "Ventana de promedios:",
      min = 1,
      max = 7,
      value = 7,
      step = 1, 
      round = TRUE
    )
  ),
  
  body = dashboardBody(
    
    fluidRow(
      valueBoxOutput("num_qpcr"),
      valueBoxOutput("num_seq")
    ),
    
    fluidRow(
      tabBox(
        width = 12,
        
        ## PCR ##
        tabPanel(
          title = "Variantes por PCR",
          fluidRow(
            column(
              width = 6,
              leafletOutput(
                "map", 
                # width = "50%", 
                height = "600px"
              )
            ),
            
            column(
              width = 6,
              plotlyOutput("scat", height = "600px")
            )
          ),
          
          fluidRow(
            plotlyOutput("cumulative")
          ),
          
          fluidRow(
            plotlyOutput("area")
          )
        ),
        
        
        ## SEQ ##
        tabPanel(
          title = "Secuenciación"
        )
      )
    )
  )
)