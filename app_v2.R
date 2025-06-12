library(shiny)
library(shinydashboard)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(shinyalert)
library(shinyauthr)
library(shinyBS)

library(IPDfromKM)
library(flexsurv)


ui <- dashboardPage(skin = "black",
  dashboardHeader(disable = FALSE,title = "Sobrevida"),
  dashboardSidebar(collapsed = FALSE,disable=FALSE,
    h5("Selecione um arquivo com tempo e sobrevida"),
    radioButtons("format","Formato do arquivo",choices = c("Excel","csv"),selected = "csv"),
    conditionalPanel(condition = "input.format=='csv'",
      checkboxInput("header", "Rótulo das variáveis", TRUE),
      radioButtons("sep","Separador",choices=c("Comma"=",","Semicolon"=";","Tab"="\t"),selected=";")
    ),
    fileInput("file", label = NULL,accept = c(".csv",".xlsx",".xls"),
      buttonLabel="Buscar",placeholder = "Nenhum arquivo selecionado"),
    br()
    
  ),
  dashboardBody(
    br(),
    tabsetPanel(
      tabPanel("Tratamento",
        fluidPage(
          h3("Reconstrução de dados de sobrevida a partir de dados agregados"),
          column(4,
            div(style = "overflow:scroll;", dataTableOutput("table", height = 500,width = 100))
          ),
          column(8,
            h4("Definir um vetor contendo os tempos de coleta de dados dispostos no gráfico"),
            textInput("trisk",label = NULL),
            br(),
            h4("Definir um vetor contendo os números de pessoas sob risco em cada tempo"),
            textInput("nrisk",label = NULL),
            br(),
            actionButton("run_codes","Gerar dados individualizados"),
            br(),
            br(),
            downloadButton("save", "Salvar arquivo", class = "butt")
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  database <- reactive({
    req(input$file)
    df=read.csv(input$file$datapath,sep = input$sep, header = input$header)
    
  })
  
  output$table <- DT::renderDataTable({
    req(input$file)
    
    
    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = TRUE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       buttons = c('copy', 'csv', 'excel', 'pdf'))
    
    table=datatable(database(), options = my.options,rownames = F, width = '100%', extensions = 'Buttons')
    table=table %>% 
      formatStyle( 0, target= 'row',color = 'black', lineHeight='20%')
    print(table)
  })
  
  fit_data <- reactive({
    req(input$run_codes)
    
    #trisk2 <- c(0,12,18,24,36,48,60,72,84,96)
    #nrisk2 <- c(103,90,80,72,49,39,29,21,14,2)
    
    trisk2 <- as.numeric(c(unlist(strsplit(c(input$trisk),","))))
    nrisk2 <- as.numeric(c(unlist(strsplit(c(input$nrisk),","))))

    pre <- preprocess(dat=database(),
                      trisk=trisk2,
                      nrisk=nrisk2,
                      totalpts=NULL,
                      maxy=100)

  })
  
  output$save <- downloadHandler(
    
    filename = function() {
      paste0("IPD_OS_ANA.csv")
    },
    
    content = function(file) {
      
      est <- getIPD(fit_data(),
                    #armID=0,#Havendo mais de um braço de tratamento, indicar
                    tot.events=NULL)
      ipd <- est[["IPD"]]
      
      # db=database()
      
      write.csv(ipd,file, row.names = FALSE)
      
      # switch (input$format6,
      #         ".csv" = write.csv2(df, file,row.names = FALSE),
      #         ".xlsx" = write.xlsx(df,file)
      # )
    }
  )
  
}

shinyApp(ui = ui, server = server)
