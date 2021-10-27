ui <- dashboardPage(
  skin = "green",
  
  header = dashboardHeader(
    title = "GTI - Vigilancia SARS-Cov-2"
  ),
  
  sidebar = dashboardSidebar(disable = TRUE),
  
  body = dashboardBody(
    
    busy_start_up(
      loader = spin_epic("orbit", color = "green"),
      text = "Descargando y procesando datos...",
      timeout = 5000,
      mode = "auto"
    ),
    
    fluidRow(
      valueBoxOutput("num_qpcr"),
      valueBoxOutput("num_seq"),
      valueBoxOutput("last_update")
    ),
    
    fluidRow(
      tabBox(
        width = 12,
        
        ## PCR ##
        tabPanel(
          title = "Variantes por PCR",
          
          fluidRow(
            box(
              title = "Totales",
              width = 12, 
              fluidRow(
                column(
                  width = 6,
                  leafletOutput(
                    "mapV", 
                    # width = "50%", 
                    height = "600px"
                  )
                ),
                
                column(
                  width = 6, 
                  fluidRow(
                    pickerInput(
                      "color_byV",
                      label = "Colorear por:", 
                      choices = c("Variante", "Departamento", "Sexo"),
                      selected = "Variante",
                      width = "200px"
                    ),
                    radioButtons(
                      "scale_V",
                      label = "Escala",
                      choices = c("Conteo", "Porcentaje"), 
                      selected = "Conteo",
                      inline = TRUE,
                      width = "200px"
                    )
                  ),
                  plotlyOutput("barV", height = "500px")
                )
                
              ) 
            )
          ),
          
          fluidRow(
            
            box(
              title = "Serie de datos temporales",
              width = 12,
              fluidRow(
                column(
                  width = 6, 
                  uiOutput("departamentos")
                ),
                column(
                  width = 6,
                  uiOutput("fechas")
                )
              ),
              fluidRow(
                plotlyOutput("scat")#, height = "600px")
              ),
              
              fluidRow(
                column(
                  width = 11,
                  plotlyOutput("cumulative")
                )
              ),
              
              fluidRow(
                sliderInput(
                  "window",
                  label = "Ventana de promedios:",
                  min = 1,
                  max = 7,
                  value = 7,
                  step = 1, 
                  round = TRUE, 
                  width = "300px"
                ),
                column(
                  width = 11,
                  plotlyOutput("area")
                )
              )
            ) 
          ),
          
        ),
        
        
        ## SEQ ##
        tabPanel(
          title = "Secuenciación",
          fluidRow(
            box(
              title = "Totales Secuenciación",
              width = 12,
              fluidRow(
                column(
                  width = 6,
                  leafletOutput(
                    "mapS",
                    height = "600px"
                  )
                ),
                column(
                  width = 6,
                  fluidRow(
                    pickerInput(
                      "color_byS",
                      label = "Colorear por:", 
                      choices = c("Linaje", "Departamento", "Sexo"),
                      selected = "Linaje",
                      width = "200px"
                    ),
                    radioButtons(
                      "scale_S",
                      label = "Escala",
                      choices = c("Conteo", "Porcentaje"), 
                      selected = "Conteo",
                      inline = TRUE, 
                      width = "200px"
                    )
                  ),
                  plotlyOutput("barS", height = "600px")
                )
              )
              
            )
          ), 
          fluidRow(
            sliderInput(
              "windowS",
              label = "Ventana de promedios:",
              min = 1,
              max = 7,
              value = 7,
              step = 1, 
              round = TRUE, 
              width = "300px"
            ),
            column(
              width = 11,
              plotlyOutput("areaS")
            )
          ),
          fluidRow(
            DTOutput("seqdata")
          )
        )
      )
    )
  )
)