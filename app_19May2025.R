library(shiny)
library(shinydashboard)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(shinyalert)
library(shinyauthr)
library(shinyBS)
library(shinyFeedback)
library(rintrojs)
library(fresh)

library(IPDfromKM)
library(flexsurv)

library(DT)
library(tidyverse)


#source("/home/urstudio/Pessoal/Ivan/theme.R")

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#00205C",
    
  ),
  adminlte_sidebar(
    width = "200px",
    dark_bg = "#FFF",
    dark_hover_bg = "#1B4E77",
    dark_color = "#1B4E77",
    
  ),
  adminlte_global(
    content_bg = "white",
    box_bg = "white", 
    info_box_bg = "white"
  )
)

options(spinner.color = "grey", spinner.color.background = "#ffffff", spinner.size = 2, shiny.reactlog=TRUE)


ui <- dashboardPage(skin = "black",
        dashboardHeader(disable = TRUE,title = "Sobrevida"),
        dashboardSidebar(collapsed = TRUE,disable=TRUE),
        dashboardBody(
          introjsUI(),
          useShinyFeedback(),
          use_theme(mytheme),
          tags$head(tags$style(HTML(
                        "
            .card-counter.purple{background-color: #A02B93;color: #FFF;}
            .card-counter.info2{background-color: #0B76A0;color: #FFF;}
            .box.box-primary > .box-header {background-color: #00205C !important;color: white !important;}
            .box.box-primary {border-top-color: #00205C !important;}
            .irs-single, .irs-from, .irs-to, .irs-min, .irs-max {background-color: #00205C !important;color: white !important;font-weight: bold;}
            .irs-bar {background-color: #00205C !important;border-top: 1px solid #00205C !important;border-bottom: 1px solid #00205C !important;}
            .irs-slider {background-color: #00205C !important;border: 1px solid #001a47 !important;}
            .small-box {border-radius: 10px;margin-bottom: 10px !important;}
            .navbar-default .navbar-brand {color:white; min-width: 0px;}
            .navbar-default .navbar-brand:hover {color:white;}
            .navbar { background-color:#00205C;}
            .navbar-default .navbar-nav > li > a {color:white; min-width: 250px;font-size:16px}
            .navbar-default .navbar-nav > .active > a,
            .navbar-default .navbar-nav > .active > a:focus,
            .navbar-default .navbar-nav > .active > a:hover {color:#00205C;background-color:#CDD2DD;font-weight: bold;}
            .navbar-default .navbar-nav > li > a:hover {color:white;background-color:#1F3161;text-decoration}
            .btn-lang {color: #263238;background-color: white;border-color: white;}
            .plots-title{margin-left: 30px; margin-right: 30px; text-align: center}
            
            "
          ))),
          fluidRow(
            style = "background-color: #15385A; height: 60px; display: flex; justify-content: flex-end; align-items: center; padding-right: 0px;",
            actionButton(
              inputId = "help",
              label = icon("info-circle", class = "fa-2x"),
              style = "background: transparent; border: none; color: #ffffff; cursor: pointer;"
            )
          ),
          fluidPage(
            br(),
            introBox(
              htmlOutput("title"),
              data.step = 1,
              data.intro = "Esta é uma ferramenta calculadora de dados de sobrevida."
            ),
            br(),
            tabsetPanel(
              tabPanel(
                introBox(
                  "Tratamento",
                  data.step = 2,
                  data.intro = "Esta seção é utilizada para simular os dados individualizados (IPD) a partir de dados agregados obtidos de gráficos curvas de Kaplan-Meier."
                ),
                br(),
                htmlOutput("title1",align="left"),
                htmlOutput("description1",align="left"),
                hr(),
                column(width = 3,
                  introBox(
                    box(width = 12,height = 600,
                      h5("Selecione um arquivo com tempo e sobrevida"),
                      introBox(
                        radioButtons("format","Formato do arquivo",choices = c("Excel","csv"),selected = "csv"),
                        data.step = 6,
                        data.intro = "Para o arquivos do formato csv, você precisa configurar se a primeira linha 
                        contém o título das variáveis, marcando a opção 'Rótulo de variáveis' e o tipo de separador 
                        dos dados (vírgula, ponto e vírgula ou tab)."
                      ),
                      conditionalPanel(condition = "input.format=='csv'",
                                       checkboxInput("header", "Rótulo das variáveis", TRUE),
                                       radioButtons("sep","Separador",choices=c("Vírgula"=",","Ponto e vírgula"=";","Tab"="\t"),selected=";")
                      ),
                      fileInput("file", label = NULL,accept = c(".csv",".xlsx",".xls"),
                                buttonLabel="Buscar",placeholder = "Nenhum arquivo selecionado"),
                      introBox(
                        uiOutput("var_select_ui"),
                        data.step = 7,
                        data.intro = "Selecione as respectivas variáveis de tempo e sobrevida do seu banco de dados."
                      ),
                      introBox(
                        actionButton("preview","Pré-visualizar dados"),
                        data.step = 8,
                        data.intro = "Após carregar o arquivo, é possível checar se está sendo lido corretamente 
                        clicando na opção 'Pré-visualizar dados'."
                      )
                    ),
                    data.step = 5,
                    data.intro = "Nesta opção você carrega o arquivo contendo dados com o tempo e sobrevida. 
                    Este arquivo pode ser do formato Excel (xlsx) ou csv. Importante notar que estas configurações 
                    são essenciais para que o arquivo seja carregado corretamente."
                  )
                ),
                column(width = 9,
                  box(width = 12,height = 600,
                    fluidPage(
                      column(4,
                        introBox(
                          div(style = "overflow-y:scroll;", dataTableOutput("table", height = 550,width = 250)),
                          data.step = 9,
                          data.intro = "Aqui você irá pré-visualizar os dados após carregados."
                        )
                      ),
                      column(8,
                          h4("Possui vetor de tempos e número de pessoas sob risco em cada tempo?"),
                        introBox(
                          radioButtons("n_t_risk",label = NULL,choices = c("Sim","Não"),selected = "Sim",inline = T),
                          data.step = 10,
                          data.intro = "Selecione se possui informações sobre o vetor de tempos e número de pessoas sob risco em cada tempo. 
                          Caso contrário, deverá informar o número total de pessoa sob risco e o algorítmo irá fornecer uma aproximação baseada 
                          nos dados carregados."
                        ),
                        conditionalPanel(condition = "input.n_t_risk=='Sim'",
                          h4("Definir um vetor contendo os tempos de coleta de dados dispostos no gráfico"),
                          textInput("trisk",label = NULL,value = NULL,placeholder = "Informe o vetor de tempos de coleta separados por vírgula"),
                          br(),
                          h4("Definir um vetor contendo os números de pessoas sob risco em cada tempo"),
                          textInput("nrisk",label = NULL,value = NULL,placeholder = "Informe o vetor de número de pessoas sob risco no tempo separados por vírgula"),
                        ),
                        conditionalPanel(condition = "input.n_t_risk=='Não'",
                          h4("Informar o número inicial de pacientes"),
                          numericInput("totalpts",label = NULL,value = NULL)
                        ),
                        br(),
                        introBox(
                          actionButton("run_codes","Gerar dados individualizados"),
                          data.step = 11,
                          data.intro = "Após carregar os dados e checar que está carregado corretamente. E,
                          após informar os vetores com o tempo e o número de pessoas sob risco, clicar 
                          neste botão para gerar os dados individualizados."
                        ),
                        br(),
                        br(),
                        introBox(
                          downloadButton("save", "Salvar arquivo", class = "butt"),
                          data.step = 12,
                          data.intro = "Use este botão para baixar os dados individualizados gerados. 
                          Este será um arquivo do formato csv."
                        )
                      )
                    )
                  )
                )
              ),
              tabPanel(
                introBox(
                  "Ajuste",
                  data.step = 3,
                  data.intro = "Esta seção é utilizada realizar o ajuste de distribuições de sobrevivência paramétricas."
                ),
                br(),
                htmlOutput("title2",align="left"),
                htmlOutput("description2",align="left"),
                hr(),
                column(width = 3,
                  box(width = 12,
                    radioButtons("manter","Carregar o arquivo da aba anterior",choices = c("Sim","Não"),inline = T,selected = "Não"),
                    conditionalPanel(condition = "input.manter=='Não'",
                       h5("Selecione o arquivo contendo os dados individualizados"),
                       radioButtons("format2","Formato do arquivo",choices = c("Excel","csv"),selected = "csv"),
                       conditionalPanel(condition = "input.format2=='csv'",
                                        checkboxInput("header2", "Rótulo das variáveis", TRUE),
                                        radioButtons("sep2","Separador",choices=c("Vírgula"=",","Ponto e vírgula"=";","Tab"="\t"),selected=";")
                       ),
                       fileInput("file2", label = NULL,accept = c(".csv",".xlsx",".xls"),
                                 buttonLabel="Buscar",placeholder = "Nenhum arquivo selecionado")
                    )
                  )
                ),
                column(width = 9,
                  box(width = 12,
                    tabsetPanel(
                      tabPanel("Dados e curva",
                        fluidPage(
                          column(width = 4,
                            div(style = "overflow-y:scroll;", dataTableOutput("table2", height = 360,width = 100))
                          ),
                          column(width = 8,
                            h3("Curva de Kaplan-Meier"),
                            withSpinner(plotOutput("plot1", width = 'auto', height = 350), type = 2)
                          )
                        )
                      ),
                      tabPanel("Parâmetros ajustados",
                        fluidPage(
                          div(style = "overflow:scroll;", dataTableOutput("table3", height = 340,width = 100))
                        )
                      ),
                      tabPanel("Qualidade do ajuste",
                        fluidPage(
                          div(style = "overflow:scroll;", dataTableOutput("table4", height = 340,width = 100))
                        )
                      ),
                      tabPanel("Dados para MC multivariado",
                        fluidPage(
                          column(width = 6,
                             div(style = "overflow:scroll;", dataTableOutput("table5.1", height = 340,width = 100))
                          ),
                          column(width = 6,
                             div(style = "overflow:scroll;", dataTableOutput("table5.2", height = 340,width = 100))
                          )
                        )
                      )
                    )
                  )
                )
              ),
              tabPanel(
                introBox(
                  "Ajuda",
                  data.step = 4,
                  data.intro = "Nesta seção contém maiores detalhes sobre a metodologia da ferramenta."
                ),
              )
            )
          ),
       )
)





server <- shinyServer(function(input, output, session) {
  
  hintjs(session, options = list("hintButtonLabel"="Espero que este tutorial seja útil"),
         events = list("onhintclose"=I('alert("Wasn\'t that hint helpful")')))
  
  observeEvent(input$help,
               introjs(session, options = list("nextLabel"="Próximo",
                                               "prevLabel"="Anterior",
                                               "skipLabel"="Fechar"),
                       events = list("oncomplete"=I('alert("Tutorial concluído")')))
  )
  
  observe({
    # Pega os vetores como strings
    trisk_vals <- strsplit(input$trisk, ",")[[1]]
    nrisk_vals <- strsplit(input$nrisk, ",")[[1]]
    
    # Remove espaços e transforma em numérico
    trisk_clean <- as.numeric(trimws(trisk_vals))
    nrisk_clean <- as.numeric(trimws(nrisk_vals))
    
    # Checa se os comprimentos são iguais
    valid <- length(trisk_clean) == length(nrisk_clean)
    
    # Exibe feedback (caso use shinyFeedback)
    feedbackWarning(
      inputId = "trisk",
      show = !valid,
      text = "O vetor de tempos deve ter o mesmo tamanho do vetor de número de pessoas em risco."
    )
    
    feedbackWarning(
      inputId = "nrisk",
      show = !valid,
      text = "O vetor de número de pessoas em risco deve ter o mesmo tamanho do vetor de tempos."
    )
    
    # Alternativamente, pode desabilitar o botão de gerar dados:
    shinyjs::toggleState("run_codes", condition = valid)
  })
  
  observeEvent(input$run_codes, {
    # Definindo o caminho do arquivo
    file_path <- tempfile(fileext = ".csv")
    
    # Barra de progresso
    withProgress(message = "Processando...", value = 0, {
      for (i in 1:10) {
        Sys.sleep(0.01)
        incProgress(0.1)
      }
      
      # Criação do arquivo
      write.csv(fit_data(), file_path, row.names = FALSE)
    })
    
    # Atualiza o botão de download
    shinyjs::show("save")
    
    # Handler para o download
    output$save <- downloadHandler(
      filename = function() {
        "dados_IPD.csv"
      },
      content = function(file) {
        file.copy(file_path, file)
      }
    )
  })
  
  output$title <- renderUI({
    
    htmlText = paste("<div style='font-weight: bold;font-size: 34px;color: #414141;text-indent: 0.1em;'>","Ferramenta para cálculos de sobrevida","</div>")
    HTML(htmlText)
  })
  
  output$description <- renderUI({
    htmlText = paste("<div style='font-size: 18px;color: #414141;'>",
                     "Descrição...",
                     "</div>")
    
    HTML(htmlText)
    
  })
  
  output$title1 <- renderUI({
    
    htmlText = paste("<div style='font-weight: bold;font-size: 24px;color: #414141;'>","Reconstrução de dados de sobrevida a partir de dados agregados","</div>")
    HTML(htmlText)
  })
  
  output$description1 <- renderUI({
    htmlText = paste("<div style='font-size: 18px;color: #414141;'>",
                     "O método a seguir é baseado nos trabalhos de Guyot et al. (2012) e Liu et al. (2021), onde são propostos algoritmos e pacotes na linguagem R que simulam os dados individualizados (IPD) a partir de dados agregados obtidos de gráficos curvas de Kaplan-Meier.",
                     "</div>")
    
    HTML(htmlText)
    
  })
  
  output$title2 <- renderUI({
    
    htmlText = paste("<div style='font-weight: bold;font-size: 24px;color: #414141;'>","Ajuste de curvas de sobrevida com dados individualizados (IPD)","</div>")
    HTML(htmlText)
  })
  
  output$description2 <- renderUI({
    htmlText = paste("<div style='font-size: 18px;color: #414141;'>",
                     "O método a seguir é baseado no pacote estatístico flexsurv, desenvolvido por Jackson (2016), onde é possível realizar o ajuste de distribuições de sobrevivência paramétricas, incluindo: Exponencial, Weibull, Log-normal, Log-logística, Gompertz e Gama generalizada.",
                     "</div>")
    
    HTML(htmlText)
    
  })
  
  database <- reactive({
    req(input$file)
    df=read.csv(input$file$datapath,sep = input$sep, header = input$header) #%>%
      #rename("tempo"=1,"sobrevida"=2)
    # 
    # if(any(str_detect(df$tempo,","))==TRUE){
    #   df=df %>% 
    #     mutate(tempo=scan(text=tempo, dec=","))
    # } else{
    #   df=df %>% 
    #     mutate(tempo=as.numeric(tempo))
    # }
    # 
    # 
    # if(any(str_detect(df$sobrevida,","))==TRUE){
    #   df=df %>% 
    #     mutate(sobrevida=scan(text=sobrevida, dec=","))
    # } else{
    #   df=df %>% 
    #     mutate(sobrevida=as.numeric(sobrevida))
    # }
    # 
    # if(max(df$sobrevida,na.rm=T)<=1){
    #   df$sobrevida=df$sobrevida*100
    # }
      
    #mutate(tempo=case_when(str_detect(tempo,",") ~ scan(text=tempo, dec=","),
    #                         TRUE ~ as.numeric(tempo)))# %>% 
      # mutate(status=case_when(str_detect(sobrevida,",") ~ scan(text=sobrevida, dec=","),
      #                            TRUE ~ as.numeric(sobrevida)))
    
  })
  
  output$var_select_ui <- renderUI({
    req(database())
    cols <- names(database())
    tagList(
      selectInput("var1", "Selecione a variável tempo:", choices = cols,selected = cols[1]),
      selectInput("var2", "Selecione a variável sobrevida:", choices = cols,selected = cols[2])
    )
  })
  
  database2 <- reactive({
    
    if(!is.null(input$file) & input$manter=="Sim"){
      req(input$run_codes)
      df=fit_data()
    } else{
      req(input$file2)
      df=read.csv(input$file2$datapath,sep = input$sep2, header = input$header2) %>% 
        rename("time"=1,"status"=2,"treat"=3)
      
      if(any(str_detect(df$time,","))==TRUE){
        df=df %>% 
          mutate(time=scan(text=time, dec=","))
      } else{
        df=df %>% 
          mutate(time=as.numeric(time))
      }
      
    }
    
    
  })
  
  output$plot1 <- renderPlot({
    km <- survfit(Surv(time, status)~1,data=database2())
    plot(km, xmax= 100, xlab="Tempo", ylab="Sobrevida")
    
  })
  
  output$table <- DT::renderDataTable({
    req(input$file)
    req(input$preview)
    
    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = FALSE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       buttons = c('copy', 'csv', 'excel', 'pdf'))
    
    df=data.frame(tempo=database()[,input$var1],sobrevida=database()[,input$var2])
    
    if(any(str_detect(df$tempo,","))==TRUE){
      df=df %>%
        mutate(tempo=scan(text=tempo, dec=","))
    } else{
      df=df %>%
        mutate(tempo=as.numeric(tempo))
    }


    if(any(str_detect(df$sobrevida,","))==TRUE){
      df=df %>%
        mutate(sobrevida=scan(text=sobrevida, dec=","))
    } else{
      df=df %>%
        mutate(sobrevida=as.numeric(sobrevida))
    }

    if(max(df$sobrevida,na.rm=T)<=1){
      df$sobrevida=df$sobrevida*100
    }
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #15385A;}"
    header.names <- c("Tempo","Sobrevida")
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(lapply(header.names, th, style = "text-align: center; border-right: 1px solid white; border-bottom: 1px solid white"))
      )
    ))
    
    table=datatable(df, options = my.options,container = my.container, rownames = F, width = '100%', extensions = 'Buttons')
    table=table %>% 
      formatStyle( 0, target= 'row',color = 'black', lineHeight='100%') %>% 
      formatStyle(columns = c(1:2),
                  width = '100px',
                  fontFamily = "Calibri",
                  fontSize = "14px",
                  borderRightWidth = "1px",
                  borderRightStyle = "solid",
                  borderRightColor = "white",
                  borderBottomColor = "#ffffff",
                  borderBottomStyle = "solid",
                  borderBottomWidth = "1px",
                  borderCollapse = "collapse",
                  verticalAlign = "middle",
                  textAlign = "center",
                  wordWrap = "break-word") %>% 
      formatRound(c(names(df)[1],names(df)[2]),digits = 2,mark = ".")
    print(table)
  })
  
  output$table2 <- DT::renderDataTable({
    #req(input$file2)
    
    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = FALSE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       buttons = c('copy', 'csv', 'excel', 'pdf'))
    
    table=datatable(database2(), options = my.options,rownames = F, width = '100%', extensions = 'Buttons')
    table=table %>% 
      formatStyle( 0, target= 'row',color = 'black', lineHeight='100%')
    print(table)
  })
  
  output$table3 <- DT::renderDataTable({
    #req(input$file2)
    
    # Exponential <- flexsurvreg(Surv(time, status)~1,data=database2(), dist="exp")
    # Weibull <- flexsurvreg(Surv(time, status)~1,data=database2(), dist="weibullPH")
    # Lognormal <- flexsurvreg(Surv(time, status)~1,data=database2(), dist="lnorm")
    # Gengamma <- flexsurvreg(Surv(time, status)~1,data=database2(), dist="gengamma")
    # Loglogistic <- flexsurvreg(Surv(time, status)~1,data=database2(), dist="llogis")
    # Gompertz <- flexsurvreg(Surv(time, status)~1,data=database2(), dist="gompertz")
    
    p_reg_exp <- data.frame(exp()[["res"]])
    p_reg_exp$Model <- c("Exponential")
    p_reg_wei <- data.frame(weibull()[["res"]])
    p_reg_wei$Model <- c("Weibull")
    p_reg_logl <- data.frame(loglogistic()[["res"]])
    p_reg_logl$Model <- c("Loglogistica")
    p_reg_gomp <- data.frame(gompertz()[["res"]])
    p_reg_gomp$Model <- c("Gompertz")
    p_reg_lnorm <- data.frame(lognormal()[["res"]])
    p_reg_lnorm$Model <- c("Lognormal")
    p_reg_gam_gen <- data.frame(gengamma()[["res"]])
    p_reg_gam_gen$Model <- c("Gama generalizada")
    
    Parametros <- rbind(p_reg_exp, p_reg_lnorm, p_reg_logl, p_reg_wei, p_reg_gomp,p_reg_gam_gen)
    Parametros$Parametros<-rownames(Parametros)
    Parametros<-Parametros %>% 
      relocate(Parametros, .before = est)
    
    
    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = FALSE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       buttons = c('copy', 'csv', 'excel', 'pdf'))
    
    table=datatable(Parametros, options = my.options,rownames = F, width = '100%', extensions = 'Buttons')
    table=table %>% 
      formatStyle( 0, target= 'row',color = 'black', lineHeight='100%')
    print(table)
  })
  
  output$table4 <- DT::renderDataTable({
    #req(input$file2)
    
    AIC = AIC(exp(), weibull(), loglogistic(), gompertz(),
              lognormal(), gengamma())
    
    BIC = rbind(BIC(exp()),BIC(weibull()),BIC(loglogistic()),
                BIC(gompertz()),BIC(lognormal()), BIC(gengamma()))
    
    Ajuste <- cbind(Distr=c("Exponential","Weibull","Loglogistic","Gompertz",
                            "Lognormal","Gengamma"),AIC,BIC)
    
    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = FALSE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       buttons = c('copy', 'csv', 'excel', 'pdf'))
    
    table=datatable(Ajuste, options = my.options,rownames = F, width = '100%', extensions = 'Buttons')
    table=table %>% 
      formatStyle( 0, target= 'row',color = 'black', lineHeight='100%')
    print(table)
  })
  
  output$table5.1 <- DT::renderDataTable({
    #req(input$file2)
    
    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = FALSE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       buttons = c('copy', 'csv', 'excel', 'pdf'))
    
    table=datatable(vcov(gompertz()), options = my.options,rownames = F, width = '100%', extensions = 'Buttons')
    table=table %>% 
      formatStyle( 0, target= 'row',color = 'black', lineHeight='100%')
    print(table)
  })
  
  output$table5.2 <- DT::renderDataTable({
    #req(input$file2)
    
    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = FALSE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       buttons = c('copy', 'csv', 'excel', 'pdf'))
    
    table=datatable(t(chol(vcov(gompertz()))), options = my.options,rownames = F, width = '100%', extensions = 'Buttons')
    table=table %>% 
      formatStyle( 0, target= 'row',color = 'black', lineHeight='100%')
    print(table)
  })
  
  fit_data <- reactive({
    req(input$run_codes)
    
    #trisk2 <- c(0,12,18,24,36,48,60,72,84,96)
    #nrisk2 <- c(103,90,80,72,49,39,29,21,14,2)
    
    df=data.frame(tempo=database()[,input$var1],sobrevida=database()[,input$var2])
    
    if(any(str_detect(df$tempo,","))==TRUE){
      df=df %>%
        mutate(tempo=scan(text=tempo, dec=","))
    } else{
      df=df %>%
        mutate(tempo=as.numeric(tempo))
    }
    
    
    if(any(str_detect(df$sobrevida,","))==TRUE){
      df=df %>%
        mutate(sobrevida=scan(text=sobrevida, dec=","))
    } else{
      df=df %>%
        mutate(sobrevida=as.numeric(sobrevida))
    }
    
    if(max(df$sobrevida,na.rm=T)<=1){
      df$sobrevida=df$sobrevida*100
    }
    
    if(input$n_t_risk=="Sim"){
      trisk2 <- as.numeric(c(unlist(strsplit(c(input$trisk),","))))
      nrisk2 <- as.numeric(c(unlist(strsplit(c(input$nrisk),","))))
      
      pre <- preprocess(dat=df,
                        trisk=trisk2,
                        nrisk=nrisk2,
                        totalpts=NULL,
                        maxy=100)
    } else{
      
      pre <- preprocess(dat=df,
                        totalpts=as.numeric(input$totalpts),
                        maxy=100)
    }
    
    
    est <- getIPD(pre,
                  #armID=0,#Havendo mais de um braço de tratamento, indicar
                  tot.events=NULL)
    ipd <- est[["IPD"]]

  })
  
  exp <- reactive({
    #req(input$file2)
    
    Exponential <- flexsurvreg(Surv(time, status)~1,data=database2(), dist="exp")
  })
  
  weibull <- reactive({
    #req(input$file2)
    
    Weibull <- flexsurvreg(Surv(time, status)~1,data=database2(), dist="weibullPH")
  })
  
  lognormal <- reactive({
    #req(input$file2)
    
    Lognormal <- flexsurvreg(Surv(time, status)~1,data=database2(), dist="lnorm")
  })
  
  gengamma <- reactive({
    #req(input$file2)
    
    Gengamma <- flexsurvreg(Surv(time, status)~1,data=database2(), dist="gengamma")
  })
  
  loglogistic <- reactive({
    #req(input$file2)
    
    Loglogistic <- flexsurvreg(Surv(time, status)~1,data=database2(), dist="llogis")
  })
  
  gompertz <- reactive({
    #req(input$file2)
    
    Gompertz <- flexsurvreg(Surv(time, status)~1,data=database2(), dist="gompertz")
  })
  
  # output$save <- downloadHandler(
  #   
  #   filename = function() {
  #     paste0("file.csv")
  #   },
  #   
  #   content = function(file) {
  #     
  #     write.csv(fit_data(),file, row.names = FALSE)
  #     
  #     # switch (input$format6,
  #     #         ".csv" = write.csv2(df, file,row.names = FALSE),
  #     #         ".xlsx" = write.xlsx(df,file)
  #     # )
  #   }
  # )
  
})

shinyApp(ui = ui, server = server)
