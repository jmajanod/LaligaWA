#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(readxl)

source("CargaResultados.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Laliga: historico del partido"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel("Seleccionar equipos:",
                     selectInput("ht",
                                 "Equipo Local",
                                 choices = nombres, 
                                 selected = "Ath Madrid"
                     ),      
                     selectInput("at",
                                "Equipo Visitante",
                                 choices = nombres,
                                selected = "Barcelona"
                     ),
                     checkboxGroupInput(
                         "ver", "Seleccionar Informe:",
                         choices = list("Relacion Fuerzas",
                                        "Resultados ultimas temporadas",
                                        "Partidos del local en ultimas temporadas",
                                        "Partidos del visitante en ultimas temporadas"),
                         selected= "Relacion Fuerzas"
                     )
                     
                     
        ),

        mainPanel(
            # FUERZA
            conditionalPanel( 
                 condition = "input.ver.includes('Relacion Fuerzas')",
                    "Fuerza entre local y visitante (%)",
                     textOutput("fuerza"), "\n\n\n"),
            
            
            conditionalPanel( 
                condition = "input.ver.includes('Resultados ultimas temporadas')",
                "Resultados entre ambos en las ultimas temporadas:",
                tableOutput("ultimospartidos"), "\n\n\n"),
            
            conditionalPanel( 
                condition = "input.ver.includes('Partidos del local en ultimas temporadas')",
            "Resultados en casa del equipo local",
            tableOutput("res_casa"), "\n\n\n"),
            
            conditionalPanel( 
                condition = "input.ver.includes('Partidos del visitante en ultimas temporadas')",
            "Resultados fuera del equipo visitante",
            tableOutput("res_fuera"), "\n\n\n"),
            
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    rv <- reactiveValues(ht = "Ath Madrid")
    rv$ht <- reactive(input$ht)
    rv$at <- reactive(input$at)
    
    output$fuerza <- renderText({
        options(digits = 3)
        as.numeric(Pesos$peso[Pesos$equipos == rv$ht()]/Pesos$peso[Pesos$equipos == rv$at()])
    })
    output$ultimospartidos <- renderTable({
        resultados %>% filter(HomeTeam == rv$ht() & AwayTeam == rv$at()) %>% arrange(desc(as.integer(Temporada)))
    })
    output$res_casa <- renderTable({
        resultados %>% filter(HomeTeam == rv$ht()) %>% select(HomeTeam, FTR, Temporada) %>% mutate(Temporada = as.integer(Temporada)) %>%  
            arrange(desc(Temporada)) %>%                     
            group_by(HomeTeam, Temporada) %>%
            summarize(g = sum(FTR=="H"),
                      e = sum(FTR == "D"),
                      p = sum(FTR == "A"))
    })
    output$res_fuera <- renderTable({
        resultados %>% filter(AwayTeam == rv$at()) %>% select(AwayTeam, FTR, Temporada) %>% mutate(Temporada = as.integer(Temporada)) %>%
            group_by(AwayTeam, Temporada) %>% 
            arrange(desc(Temporada)) %>%
            summarize(g = sum(FTR=="A"),
                      e = sum(FTR == "D"),
                      p = sum(FTR == "H"))
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
