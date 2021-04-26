ui <- dashboardPage(
  skin = "green",
  
  header = dashboardHeader(
    title = "GTI - Vigilancia SARS-Cov-2"
  ),
  
  sidebar = dashboardSidebar(disable = TRUE),
  
  body = dashboardBody(
    
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
                  pickerInput(
                    "color_byV",
                    label = "Colorear por:", 
                    choices = c("Variante", "Departamento", "Sexo"),
                    selected = "Variante",
                    width = "300px"
                  ),
                  plotlyOutput("barV", height = "500px")
                )
                
              ) 
            )
          ),
          
          fluidRow(
            
            box(
              title = "asdf",
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
                  pickerInput(
                    "color_byS",
                    label = "Colorear por:", 
                    choices = c("Linaje", "Departamento", "Sexo"),
                    selected = "Linaje",
                    width = "300px"
                  ),
                  plotlyOutput("barS", height = "600px")
                )
              )
              
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