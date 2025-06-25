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
library(plotly)

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
        dashboardHeader(disable = TRUE,title = "AjustAI"),
        dashboardSidebar(collapsed = TRUE,disable=TRUE),
        dashboardBody(
          introjsUI(),
          useShinyFeedback(),
          useShinyjs(), 
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
            .panel-default {border: none !important;box-shadow: none !important;}
            .panel-default > .panel-heading {background-color: transparent !important;border: none !important;padding-left: 0;font-weight: bold;font-size: 16px;}
            .panel-default > .panel-collapse > .panel-body {border: none !important;background-color: transparent !important;padding-left: 0;}
            "
          ))),
          tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "https://cdn.datatables.net/buttons/2.4.1/css/buttons.dataTables.min.css"),
            tags$script(src = "https://cdn.datatables.net/buttons/2.4.1/js/dataTables.buttons.min.js"),
            tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jszip/3.1.3/jszip.min.js"),
            tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/pdfmake/0.1.53/pdfmake.min.js"),
            tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/pdfmake/0.1.53/vfs_fonts.js"),
            tags$script(src = "https://cdn.datatables.net/buttons/2.4.1/js/buttons.html5.min.js"),
            tags$script(src = "https://cdn.datatables.net/buttons/2.4.1/js/buttons.print.min.js")
          ),
          fluidRow(
            style = "background-color: #00205C; height: 60px; display: flex; justify-content: flex-end; align-items: center; padding-right: 0px;",
            # actionButton(
            #   inputId = "help",
            #   label = icon("info-circle", class = "fa-2x"),
            #   style = "background: transparent; border: none; color: #ffffff; cursor: pointer;"
            # )
          ),
          fluidPage(
            br(),
            introBox(
              id="step1_1",
              uiOutput("logo")
            ),
            br(),
            tabsetPanel(id="abas",
              tabPanel(
                introBox(
                  id="step1_2",
                  "Dados"
                ),
                br(),
                fluidPage(
                  column(width = 10,
                    fluidRow(
                      htmlOutput("title1",align="left"),
                      htmlOutput("description1",align="left")
                    )
                  ),
                  column(width = 2,
                    fluidRow(
                      style = "background-color: transparent; height: 60px; display: flex; justify-content: flex-end; align-items: center; padding-right: 0px;margin-top:-260px;margin-right:-50px",
                      actionButton(
                        inputId = "help",
                        label = icon("info-circle", class = "fa-2x"),
                        style = "background: transparent; border: none; color: #ffffff; cursor: pointer;"
                      )
                    ),
                  )
                ),
                br(),
                column(width = 3,
                  introBox(
                    id="step1_5",
                    box(width = 12,height = 600,title = "Configurações",solidHeader = TRUE,status = "primary",
                      h5("Selecione um arquivo com tempo e sobrevida"),
                      introBox(
                        id="step1_6",
                        radioButtons("format","Formato do arquivo",choices = c("Excel (.xls, .xslx)","csv"),selected = "csv")
                      ),
                      conditionalPanel(condition = "input.format=='csv'",
                                       checkboxInput("header", "Rótulo das variáveis", TRUE),
                                       radioButtons("sep","Separador",choices=c("Vírgula"=",","Ponto e vírgula"=";","Tab"="\t"),selected=";")
                      ),
                      fileInput("file", label = NULL,accept = c(".csv",".xlsx",".xls"),
                                buttonLabel="Buscar",placeholder = "Nenhum arquivo selecionado"),
                      introBox(
                        id="step1_7",
                        uiOutput("var_select_ui")
                      ),
                      introBox(
                        id="step1_8",
                        actionButton("preview","Pré-visualizar dados")
                      )
                    )
                  )
                ),
                column(width = 9,
                  box(width = 12,height = 600,title = "Resultados",solidHeader = TRUE,status = "primary",
                    fluidPage(
                      column(4,
                        introBox(
                          id="step1_9",
                          div(style = "overflow-y:scroll;", dataTableOutput("table", height = 550,width = 250))
                        )
                      ),
                      column(8,
                          h4("Possui vetor de tempos e número de pessoas sob risco em cada tempo?"),
                        introBox(
                          id="step1_10",
                          radioButtons("n_t_risk",label = NULL,choices = c("Sim","Não"),selected = "Sim",inline = T)
                        ),
                        conditionalPanel(condition = "input.n_t_risk=='Sim'",
                          h4("Informar um conjunto de dados contendo os tempos de coleta de dados dispostos no gráfico"),
                          textInput("trisk",label = NULL,value = NULL,placeholder = "Informe o vetor de tempos de coleta separados por vírgula"),
                          br(),
                          h4("Informar um conjunto de dados contendo os números de pessoas sob risco em cada tempo"),
                          textInput("nrisk",label = NULL,value = NULL,placeholder = "Informe o vetor de número de pessoas sob risco no tempo separados por vírgula"),
                        ),
                        conditionalPanel(condition = "input.n_t_risk=='Não'",
                          h4("Informar o número inicial de pacientes"),
                          numericInput("totalpts",label = NULL,value = NULL)
                        ),
                        br(),
                        introBox(
                          id="step1_11",
                          actionButton("run_codes","Gerar dados individualizados")
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
                  id="step1_3",
                  "Ajuste"
                ),
                br(),
                fluidPage(
                  column(width = 10,
                    introBox(
                      id="step2_1",
                      fluidRow(
                        htmlOutput("title2",align="left"),
                        htmlOutput("description2",align="left")
                      )
                    )
                  ),
                  column(width = 2,
                    fluidRow(
                      style = "background-color: transparent; height: 60px; display: flex; justify-content: flex-end; align-items: center; padding-right: 0px;margin-top:-260px;margin-right:-50px",
                      actionButton(
                        inputId = "help2",
                        label = icon("info-circle", class = "fa-2x"),
                        style = "background: transparent; border: none; color: #ffffff; cursor: pointer;"
                      )
                    ),
                  )
                ),
                br(),
                column(width = 3,
                  introBox(
                    id="step2_2",
                    box(width = 12,height = 670,title = "Configurações",solidHeader = TRUE,status = "primary",
                      introBox(
                        id="step2_3",
                        radioButtons("manter","Carregar o arquivo da aba anterior",choices = c("Sim","Não"),inline = T,selected = "Não")
                      ),
                      conditionalPanel(condition = "input.manter=='Não'",
                         h5("Selecione o arquivo contendo os dados individualizados"),
                         introBox(
                           id="step2_4",
                           radioButtons("format2","Formato do arquivo",choices = c("Excel (.xls, .xslx)","csv"),selected = "csv")
                         ),
                         conditionalPanel(condition = "input.format2=='csv'",
                                          checkboxInput("header2", "Rótulo das variáveis", TRUE),
                                          radioButtons("sep2","Separador",choices=c("Vírgula"=",","Ponto e vírgula"=";","Tab"="\t"),selected=";")
                         ),
                         fileInput("file2", label = NULL,accept = c(".csv",".xlsx",".xls"),
                                   buttonLabel="Buscar",placeholder = "Nenhum arquivo selecionado"),
                         introBox(
                           id="step2_5",
                           uiOutput("var_select_ui2")
                         )
                      ),
                      column(width = 4,
                        introBox(
                          id="step2_6",
                          actionButton("preview2","Visualizar dados")
                        )
                      ),
                      column(width = 2),
                      column(width = 4,
                        introBox(
                          id="step2_7",
                          actionButton("run_codes2","Executar análises")
                        )
                      )
                    )
                  )
                ),
                column(width = 9,
                  box(width = 12,height = 670,title = "Resultados",solidHeader = TRUE,status = "primary",
                    tabsetPanel(
                      tabPanel(
                        introBox(
                          id="step2_8",
                          "Dados e curva"
                        ),
                        fluidPage(
                          column(width = 4,
                            br(),
                            introBox(
                              id="step2_12",
                              div(style = "overflow-y:scroll;", dataTableOutput("table2", height = 500,width = 250))
                            )
                          ),
                          column(width = 8,
                            h3("Curva de Kaplan-Meier"),
                            fluidRow(
                              column(6,
                                introBox(
                                  id="step2_13",
                                  radioButtons("add_curves","Adicionar curvas estimadas",choices=c("Sim","Não"),selected = "Não",inline = T)
                                )
                              ),
                              conditionalPanel(condition = "input.add_curves=='Sim'",
                                column(6,
                                  numericInput("horizonte","Horizonte",value = 100)
                                )
                              )
                            ),
                            br(),
                            fluidRow(
                              withSpinner(plotlyOutput("plot1", width = 'auto', height = 400), type = 2)
                            )
                          )
                        )
                      ),
                      tabPanel(
                        introBox(
                          id="step2_9",
                          "Parâmetros ajustados"
                        ),
                        fluidPage(
                          br(),
                          h4("Estimativas dos parâmetros para modelos de sobrevivência por tipo de distribuição."),
                          br(),
                          dataTableOutput("table3", height = 500,width = "100%")
                        )
                      ),
                      tabPanel(
                        introBox(
                          id="step2_10",
                          "Qualidade do ajuste"
                        ),
                        fluidPage(
                          br(),
                          h4("Qualidade de ajuste dos modelos de sobrevivência ajustados."),
                          dataTableOutput("table4", height = 500,width = "100%")
                        )
                      ),
                      tabPanel(
                        introBox(
                          id="step2_11",
                          "Dados para MC multivariado"
                        ),
                        fluidPage(
                          h4("Selecione o modelo para ajustar o MC multivariado"),
                          selectInput("model",label = NULL,choices=c("Exponential", "Weibull", "Loglogistic",
                                                                 "Gompertz","Lognormal", "Gengamma")),
                          br(),
                          column(width = 6,
                             h4(htmlOutput("title_table5.1")),
                             dataTableOutput("table5.1", height = 200,width = "100%")
                          ),
                          column(width = 6,
                             h4(htmlOutput("title_table5.2")),
                             dataTableOutput("table5.2", height = 200,width = "100%")
                          )
                        )
                      )
                    )
                  )
                )
              ),
              tabPanel(
                introBox(
                  id="step1_4",
                  "Ajuda"
                ),
                br(),
                fluidRow(
                  htmlOutput("title3",align="left"),
                  htmlOutput("description3",align="left"),
                  br(),
                  br(),
                  htmlOutput("subtitle3",align="left"),
                  br(),
                  bsCollapse(
                    id = "detalhes",
                    bsCollapsePanel(
                      title = htmlOutput("q1"),
                      htmlOutput("a1")
                    ),
                    bsCollapsePanel(
                      title = htmlOutput("q2"),
                      htmlOutput("a2")
                    ),
                    bsCollapsePanel(
                      title = htmlOutput("q3"),
                      htmlOutput("a3")
                    ),
                    bsCollapsePanel(
                      title = htmlOutput("q4"),
                      htmlOutput("a4")
                    ),
                    bsCollapsePanel(
                      title = htmlOutput("q5"),
                      htmlOutput("a5")
                    ),
                    bsCollapsePanel(
                      title = htmlOutput("q6"),
                      htmlOutput("a6")
                    ),
                    bsCollapsePanel(
                      title = htmlOutput("q7"),
                      htmlOutput("a7")
                    ),
                    bsCollapsePanel(
                      title = htmlOutput("q8"),
                      htmlOutput("a8")
                    )
                  )
                )
              )
            )
          )
       )
)





server <- shinyServer(function(input, output, session) {
  
  hintjs(session, options = list("hintButtonLabel"="Espero que este tutorial seja útil"),
         events = list("onhintclose"=I('alert("Wasn\'t that hint helpful")')))
  
  observeEvent(input$help, {
    introjs(session, options = list(
      nextLabel = "Próximo",
      prevLabel = "Anterior",
      skipLabel = "Fechar",
      steps = list(
        list(element = "#step1_1", intro = "Ajustaí é uma ferramenta calculadora de dados de sobrevida."),
        list(element = "#step1_2", intro = "Esta seção é utilizada para simular os dados individualizados (IPD) a partir de dados agregados obtidos de gráficos curvas de Kaplan-Meier."),
        list(element = "#step1_3", intro = "Esta seção é utilizada realizar o ajuste de distribuições de sobrevivência paramétricas."),
        list(element = "#step1_4", intro = "Nesta seção contém maiores detalhes sobre a metodologia da ferramenta."),
        list(element = "#step1_5", intro = "Nesta opção você carrega o arquivo contendo dados com o tempo e sobrevida. 
                    Este arquivo pode ser do formato Excel (xlsx) ou csv. Importante notar que estas configurações 
                    são essenciais para que o arquivo seja carregado corretamente."),
        list(element = "#step1_6", intro = "Para o arquivos do formato csv, você precisa configurar se a primeira linha 
                        contém o título das variáveis, marcando a opção 'Rótulo de variáveis' e o tipo de separador 
                        dos dados (vírgula, ponto e vírgula ou tab)."),
        list(element = "#step1_7", intro = "Selecione as respectivas variáveis de tempo e sobrevida do seu banco de dados."),
        list(element = "#step1_8", intro = "Após carregar o arquivo, é possível checar se está sendo lido corretamente 
                        clicando na opção 'Pré-visualizar dados'."),
        list(element = "#step1_9", intro = "Aqui você irá pré-visualizar os dados após carregados."),
        list(element = "#step1_10", intro = "Selecione se possui informações sobre o número de pessoas sob risco em cada tempo. 
                          Caso contrário, deverá informar o número total de pessoa sob risco e o algorítmo irá fornecer uma aproximação baseada 
                          nos dados carregados."),
        list(element = "#step1_11", intro = "Após carregar os dados e checar que está carregado corretamente. E,
                          após informar os vetores com o tempo e o número de pessoas sob risco, clicar 
                          neste botão para gerar os dados individualizados.")
      )
    ))
  })
  
  observeEvent(input$help2, {
    introjs(session, options = list(
      nextLabel = "Próximo",
      prevLabel = "Anterior",
      skipLabel = "Fechar",
      steps = list(
        list(element = "#step2_1", intro = "Essa seção permite realizar os ajustes das curvas a partir dos dados individualizados carregados."),
        list(element = "#step2_2", intro = "Nesta opção você carrega o arquivo contendo dados com o tempo e sobrevida. 
                    Este arquivo pode ser do formato Excel (xlsx) ou csv. Importante notar que estas configurações 
                    são essenciais para que o arquivo seja carregado corretamente."),
        list(element = "#step2_3", intro = "Se deseja utilizar os dados gerados pela seção anterior, marque a opção 'Sim', caso contrário, marque a opção 'Não' e carregue um arquivo individualizado."),
        list(element = "#step2_4", intro = "Para o arquivos do formato csv, você precisa configurar se a primeira linha 
                        contém o título das variáveis, marcando a opção 'Rótulo de variáveis' e o tipo de separador 
                        dos dados (vírgula, ponto e vírgula ou tab)."),
        list(element = "#step2_5", intro = "Selecione as respectivas variáveis de tempo e sobrevida do seu banco de dados."),
        list(element = "#step2_6", intro = "Após carregar o arquivo, é possível checar se está sendo lido corretamente 
                        clicando na opção 'Pré-visualizar dados'."),
        list(element = "#step2_7", intro = "Clique neste botão para executar as análises. Só é ativado após clicar para pré-visualizar os dados."),
        list(element = "#step2_8", intro = "Seção de visualização dos dados e curva Kaplan-Meier e as curvas estimadas dos modelos paramétricos."),
        list(element = "#step2_9", intro = "Seção de visualização dos parâmetros dos modelos ajustados."),
        list(element = "#step2_10", intro = "Seção de visualização da qualidade de ajuste dos modelos ajustados."),
        list(element = "#step2_11", intro = "Seção de visualização dos parâmetros de Monte Carlo Multivariado."),
        list(element = "#step2_12", intro = "Aqui você irá pré-visualizar os dados após carregados."),
        list(element = "#step2_13", intro = "Esta opção te permite adicionar as curvas dos modelos ajustados no gráfico de Kaplan-Meier. É possível customizar o horizonte de tempo para as curvas.")
      )
    ))
  })
  
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
  
  disable("run_codes2")
  
  observeEvent(input$preview2, {
    enable("run_codes2")  # Ativa o botão quando preview2 for clicado
  })
  
  observeEvent(input$run_codes2,{
    AIC = AIC(exp(), weibull(), loglogistic(), gompertz(),
              lognormal(), gengamma())
    
    BIC = rbind(BIC(exp()),BIC(weibull()),BIC(loglogistic()),
                BIC(gompertz()),BIC(lognormal()), BIC(gengamma()))
    
    Ajuste <- cbind(Distr=c("Exponential","Weibull","Loglogistic","Gompertz",
                            "Lognormal","Gengamma"),AIC,BIC) %>% 
      arrange(AIC)
    
    updateSelectInput(
      session,
      "model",
      label = NULL,
      selected = Ajuste$Distr[1]
    )
    
    
  })
  
  output$logo <- renderUI({
    x <- "logo.png"
    tags$img(src = x, height = "100px")#, width = "600px")
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
  
  output$title3 <- renderUI({
    
    htmlText = paste("<div style='font-weight: bold;font-size: 24px;color: #414141;margin-left: 30px;'>","Sobre Ajustaí","</div>")
    HTML(htmlText)
  })
  
  output$description3 <- renderUI({
    htmlText = paste("<div style='font-size: 18px;color: #414141;margin-left: 30px;margin-right: 30px;text-align:justify'>",
                     "Ajustaí é uma ferramenta online desenvolvida para facilitar a análise de sobrevida 
                     sem a necessidade de conhecimentos em programação. Por meio do carregamento de 
                     arquivos em formato .csv ou .xlsx, é possível gerar curvas de Kaplan-Meier 
                     e ajustar modelos paramétricos clássicos (Exponencial, Weibull, Log-normal, Log-Logística, Gamma Generalizada e Gompertz). 
                     Esta ferramenta permite visualizar os dados carregados, estimativas gráficas das curvas 
                     de sobrevida, avaliação da qualidade do ajuste dos modelos e extração dos parâmetros 
                     estimados para uso em outras aplicações.",
                     "</div>")
    
    HTML(htmlText)
    
  })
  
  output$subtitle3 <- renderUI({
    
    htmlText = paste("<div style='font-weight: bold;font-size: 24px;color: #414141;margin-left: 30px;'>","Perguntas frequentes","</div>")
    HTML(htmlText)
  })
  
  output$q1 <- renderUI({
    
    htmlText = paste("<div style='font-weight: bold;font-size: 18px;color: #414141;margin-left: 50px;'>","•	Que tipo de arquivo posso carregar (formato, separador, estrutura)?","</div>")
    HTML(htmlText)
  })
  
  output$q2 <- renderUI({
    
    htmlText = paste("<div style='font-weight: bold;font-size: 18px;color: #414141;margin-left: 50px;'>","•	Quais colunas devem estar presentes no meu arquivo de entrada?","</div>")
    HTML(htmlText)
  })
  
  output$q3 <- renderUI({
    
    htmlText = paste("<div style='font-weight: bold;font-size: 18px;color: #414141;margin-left: 50px;'>","•	O que significa 'vetor de tempos de coleta'?","</div>")
    HTML(htmlText)
  })
  
  output$q4 <- renderUI({
    
    htmlText = paste("<div style='font-weight: bold;font-size: 18px;color: #414141;margin-left: 50px;'>","•	Como devo informar o número de pessoas sob risco em cada tempo?","</div>")
    HTML(htmlText)
  })
  
  output$q5 <- renderUI({
    
    htmlText = paste("<div style='font-weight: bold;font-size: 18px;color: #414141;margin-left: 50px;'>","•	O que acontece se eu não tiver os vetores de tempo e risco?","</div>")
    HTML(htmlText)
  })
  
  output$q6 <- renderUI({
    
    htmlText = paste("<div style='font-weight: bold;font-size: 18px;color: #414141;margin-left: 50px;'>","•	A ferramenta aceita dados com censura explícita?","</div>")
    HTML(htmlText)
  })
  
  output$q7 <- renderUI({
    
    htmlText = paste("<div style='font-weight: bold;font-size: 18px;color: #414141;margin-left: 50px;'>","•	Posso exportar os resultados da análise?","</div>")
    HTML(htmlText)
  })
  
  output$q8 <- renderUI({
    
    htmlText = paste("<div style='font-weight: bold;font-size: 18px;color: #414141;margin-left: 50px;'>","•	A ferramenta ajusta modelos paramétricos automaticamente?","</div>")
    HTML(htmlText)
  })
  
  output$a1 <- renderUI({
    
    htmlText = paste("<div style='font-size: 16px;color: #414141;margin-left: 50px;'>",
                     "Você pode carregar arquivos nos formatos .csv ou .xlsx. Para arquivos .csv, é 
                     possível configurar o separador como vírgula (,), ponto e vírgula (;) ou tabulação 
                     (\t). A ferramenta permite indicar se a primeira linha do arquivo contém os nomes 
                     das variáveis (cabeçalho). A estrutura deve seguir um formato tabular (colunas com 
                     variáveis e linhas com observações).","</div>")
    HTML(htmlText)
  })
  
  output$a2 <- renderUI({
    
    htmlText = paste("<div style='font-size: 16px;color: #414141;margin-left: 50px;'>",
                     "Seu arquivo deve conter, no mínimo uma coluna com os tempos de sobrevivência ou coleta 
                     e uma coluna indicando o status do evento (1 para evento ocorrido, 0 para censura).","</div>")
    HTML(htmlText)
  })
  
  output$a3 <- renderUI({
    
    htmlText = paste("<div style='font-size: 16px;color: #414141;margin-left: 50px;'>",
                     "É a sequência dos tempos nos quais foram registrados eventos (morte, falha, etc.) 
                     ou censuras. Em dados individuais, cada linha traz o tempo de cada sujeito. 
                     Em dados agregados, são os pontos de tempo onde se observou o número de eventos 
                     e o número sob risco.","</div>")
    HTML(htmlText)
  })
  
  output$a4 <- renderUI({
    
    htmlText = paste("<div style='font-size: 16px;color: #414141;margin-left: 50px;'>",
                     "Se você estiver utilizando dados agregados, inclua uma coluna com os números sob 
                     risco correspondentes a cada tempo. Em dados individuais, essa informação será inferida 
                     automaticamente pela ferramenta com base no tempo de entrada e censura/evento de cada sujeito.","</div>")
    HTML(htmlText)
  })
  
  output$a5 <- renderUI({
    
    htmlText = paste("<div style='font-size: 16px;color: #414141;margin-left: 50px;'>",
                     "Se você não informar os vetores de tempo ou o número sob risco, deverá fornecer ao menos 
                     o número total de indivíduos e então a ferramenta irá calcular uma aproximação dos vetores 
                     de tempo e do status do evento.","</div>")
    HTML(htmlText)
  })
  
  output$a6 <- renderUI({
    
    htmlText = paste("<div style='font-size: 16px;color: #414141;margin-left: 50px;'>",
                     "Sim. A ferramenta aceita censura à direita (mais comum). Para isso, 
                     você deve usar uma coluna de status com 1 para eventos observados (ex: morte) 
                     e 0 para censura (ex: paciente ainda vivo ao final do acompanhamento).","</div>")
    HTML(htmlText)
  })
  
  output$a7 <- renderUI({
    
    htmlText = paste("<div style='font-size: 16px;color: #414141;margin-left: 50px;'>",
                     "Sim. A ferramenta permite copiar os dados ou exportar os parâmetros dos modelos 
                     ajustados e as tabelas de resumo em formato .csv, .xlsx e .pdf.","</div>")
    HTML(htmlText)
  })
  
  output$a8 <- renderUI({
    
    htmlText = paste("<div style='font-size: 16px;color: #414141;margin-left: 50px;'>",
                     "Sim. Após carregar os dados, você poderá escolher quais modelos paramétricos 
                     ajustar. A ferramenta estima os parâmetros automaticamente e apresenta curvas 
                     e métricas de qualidade do ajuste (como AIC e BIC).","</div>")
    HTML(htmlText)
  })
  
  output$title_table5.1 <- renderUI({
    HTML(paste("Matriz de covariância do modelo"),input$model)
  })
  
  output$title_table5.2 <- renderUI({
    HTML(paste("Decomposição de Choleski do modelo"),input$model)
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
  
  database2 <- reactive({
    
    if(!is.null(input$file) & input$manter=="Sim"){
      req(input$run_codes)
      df=fit_data()
    } else{
      req(input$file2)
      df=read.csv(input$file2$datapath,sep = input$sep2, header = input$header2)
      
    }
    
    
  })
  
  output$var_select_ui <- renderUI({
    req(database())
    cols <- names(database())
    tagList(
      selectInput("var1", "Selecione a variável tempo:", choices = cols,selected = cols[1]),
      selectInput("var2", "Selecione a variável sobrevida:", choices = cols,selected = cols[2])
    )
  })
  
  output$var_select_ui2 <- renderUI({
    req(database2())
    cols <- names(database2())
    tagList(
      selectInput("var3", "Selecione a variável tempo", choices = cols,selected = cols[1]),
      selectInput("var4", "Selecione a variável status", choices = cols,selected = cols[2])
    )
  })
  
  output$plot1 <- renderPlotly({
    req(input$run_codes2)
    
    if(!is.null(input$file) & input$manter=="Sim"){
      df=database2()
    } else{
      df=data.frame(time=database2()[,input$var3],status=database2()[,input$var4])
      
      if(any(str_detect(df$time,","))==TRUE){
        df=df %>%
          mutate(time=scan(text=time, dec=","))
      } else{
        df=df %>%
          mutate(time=as.numeric(time))
      }
    }
    
    km <- survfit(Surv(time, status)~1,data=df)
    #plot(km, xmax= 100, xlab="Tempo", ylab="Sobrevida")
    
    exp <- flexsurvreg(Surv(time, status)~1,data=df, dist="exp")
    weibull <- flexsurvreg(Surv(time, status)~1,data=df, dist="weibullPH")
    lognormal <- flexsurvreg(Surv(time, status)~1,data=df, dist="lnorm")
    gengamma <- flexsurvreg(Surv(time, status)~1,data=df, dist="gengamma")
    loglogistic <- flexsurvreg(Surv(time, status)~1,data=df, dist="llogis")
    gompertz <- flexsurvreg(Surv(time, status)~1,data=df, dist="gompertz")
    
    data=data.frame(time=km$time,surv=km$surv,li=km$lower,ls=km$upper)
    
    if(input$add_curves=="Não"){
      plot_ly(data, x = ~time, y = ~surv, name = 'Sobrevida', type = 'scatter', mode = 'lines',line = list(color = 'black')) %>% 
        add_trace(y = ~li, name = 'LI IC95%', mode = 'lines',
                  line = list(color = 'grey', dash = 'dash')) %>%
        add_trace(y = ~ls, name = 'LS IC95%', mode = 'lines',
                  line = list(color = 'grey', dash = 'dash')) %>%
        layout(
          yaxis = list(
            title = 'Probabilidade de Sobrevida',
            tickformat = ".2f",   # Uma casa decimal
            showline = TRUE,      # Mostra a linha do eixo Y
            linecolor = 'black',  # Cor da linha do eixo Y
            linewidth = 1         # Espessura da linha
          ),
          xaxis = list(
            title = 'Tempo',
            showline = F,
            linecolor = 'black',
            linewidth = 1
          )
        )
    } else{
      
      data2=data.frame(time=data.frame(summary(exp, type = "survival", t = seq(0, input$horizonte, by = 1)))$time,
                       exp=data.frame(summary(exp, type = "survival", t = seq(0, input$horizonte, by = 1)))$est,
                       weibull=data.frame(summary(weibull, type = "survival", t = seq(0, input$horizonte, by = 1)))$est,
                       loglogist=data.frame(summary(loglogistic, type = "survival", t = seq(0, input$horizonte, by = 1)))$est,
                       gompertz=data.frame(summary(gompertz, type = "survival", t = seq(0, input$horizonte, by = 1)))$est,
                       lognormal=data.frame(summary(lognormal, type = "survival", t = seq(0, input$horizonte, by = 1)))$est,
                       gengamma=data.frame(summary(gengamma, type = "survival", t = seq(0, input$horizonte, by = 1)))$est)
      
      plot_ly(data, x = ~time, y = ~surv, name = 'Sobrevida', type = 'scatter', mode = 'lines',line = list(color = 'black')) %>% 
        add_trace(y = ~li, name = 'LI IC95%', mode = 'lines',
                  line = list(color = 'grey', dash = 'dash')) %>%
        add_trace(y = ~ls, name = 'LS IC95%', mode = 'lines',
                  line = list(color = 'grey', dash = 'dash')) %>%
        layout(
          yaxis = list(
            title = 'Probabilidade de Sobrevida',
            tickformat = ".2f",   # Uma casa decimal
            showline = TRUE,      # Mostra a linha do eixo Y
            linecolor = 'black',  # Cor da linha do eixo Y
            linewidth = 1         # Espessura da linha
          ),
          xaxis = list(
            title = 'Tempo',
            showline = F,
            linecolor = 'black',
            linewidth = 1
          )
        ) %>% 
        add_trace(data = data2, x = ~time, y = ~exp, name = 'Exponential',
                  type = 'scatter', mode = 'lines',
                  line = list(color = 'blue')) %>%
        add_trace(data = data2, x = ~time, y = ~weibull, name = 'Weibull',
                  type = 'scatter', mode = 'lines',
                  line = list(color = 'green')) %>%
        add_trace(data = data2, x = ~time, y = ~loglogist, name = 'LogLogistic',
                  type = 'scatter', mode = 'lines',
                  line = list(color = 'red')) %>%
        add_trace(data = data2, x = ~time, y = ~gompertz, name = 'Gompertz',
                  type = 'scatter', mode = 'lines',
                  line = list(color = 'purple')) %>%
        add_trace(data = data2, x = ~time, y = ~lognormal, name = 'LogNormal',
                  type = 'scatter', mode = 'lines',
                  line = list(color = '#83CBEB')) %>%
        add_trace(data = data2, x = ~time, y = ~gengamma, name = 'Gengamma',
                  type = 'scatter', mode = 'lines',
                  line = list(color = 'orange'))
      
    }
    
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
    
    table=datatable(df, options = my.options,container = my.container, rownames = F, width = '100%', extensions = 'Buttons') %>% 
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
      formatRound(c(names(df)[1],names(df)[2]),digits = 2,mark = ".",dec.mark = ",")
    print(table)
  })
  
  output$table2 <- DT::renderDataTable({
    req(input$preview2)
    
    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = FALSE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       buttons = c('copy', 'csv', 'excel', 'pdf'))
    
    if(!is.null(input$file) & input$manter=="Sim"){
      df=database2() %>% 
        dplyr::select(-treat)
    } else{
      df=data.frame(time=database2()[,input$var3],status=database2()[,input$var4])
      
      if(any(str_detect(df$time,","))==TRUE){
        df=df %>%
          mutate(time=scan(text=time, dec=","))
      } else{
        df=df %>%
          mutate(time=as.numeric(time))
      }
    }
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #15385A;}"
    header.names <- c("Tempo","Status")
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(lapply(header.names, th, style = "text-align: center; border-right: 1px solid white; border-bottom: 1px solid white"))
      )
    ))
    
    table=datatable(df, options = my.options,container = my.container, rownames = F, width = '100%', extensions = 'Buttons') %>% 
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
      formatRound(c(names(df)[1]),digits = 2,mark = ".",dec.mark = ",")
    
    print(table)
  })
  
  output$table3 <- DT::renderDataTable({
    req(input$run_codes2)
    
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
      relocate(Parametros, .before = est) %>% 
      relocate(Model, .before = Parametros) %>% 
      relocate(est, .after = est) %>% 
      relocate(se, .after = est)
    
    
    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = FALSE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       dom = 'Bfrtip',
                       buttons = list(
                         list(
                           extend = 'copy',
                           text = 'Copiar'
                         ),
                         list(
                           extend = 'csv',
                           filename = 'parâmetros',
                           text = 'Exportar CSV'
                         ),
                         list(
                           extend = 'excel',
                           filename = 'parâmetros',
                           text = 'Exportar Excel'
                         ),
                         list(
                           extend = 'pdf',
                           filename = 'parâmetros',
                           text = 'Exportar PDF'
                         )
                       ))
    
    
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #15385A;}"
    header.names <- c("Modelo","Parâmetros","Estatística","Erro padrão","LI IC95%","LS IC95%")
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(lapply(header.names, th, style = "text-align: center; border-right: 1px solid white; border-bottom: 1px solid white"))
      )
    ))
    
    table=datatable(Parametros, options = my.options,container = my.container,rownames = F, width = '100%', extensions = 'Buttons') %>% 
      formatStyle( 0, target= 'row',color = 'black', lineHeight='100%') %>% 
      formatStyle(columns = c(1),
                  width = '200px',
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
      formatStyle(columns = c(2),
                  width = '150px',
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
      formatStyle(columns = c(3:6),
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
      formatRound(c(names(Parametros)[3],names(Parametros)[4],names(Parametros)[5],names(Parametros)[6]),digits = 6,mark = ".",dec.mark = ",")
    
    
    print(table)
  })
  
  output$table4 <- DT::renderDataTable({
    req(input$run_codes2)
    
    AIC = AIC(exp(), weibull(), loglogistic(), gompertz(),
              lognormal(), gengamma())
    
    BIC = rbind(BIC(exp()),BIC(weibull()),BIC(loglogistic()),
                BIC(gompertz()),BIC(lognormal()), BIC(gengamma()))
    
    Ajuste <- cbind(Distr=c("Exponential","Weibull","Loglogistic","Gompertz",
                            "Lognormal","Gengamma"),AIC,BIC)
    
    Ajuste$df=NULL
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #15385A;}"
    header.names <- c("Distribuição","AIC","BIC")
    
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(lapply(header.names, th, style = "text-align: center; border-right: 1px solid white; border-bottom: 1px solid white"))
      )
    ))
    
    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = FALSE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       dom = 'Bfrtip',
                       buttons = list(
                         list(
                           extend = 'copy',
                           text = 'Copiar'
                         ),
                         list(
                           extend = 'csv',
                           filename = 'qualidade do ajuste',
                           text = 'Exportar CSV'
                         ),
                         list(
                           extend = 'excel',
                           filename = 'qualidade do ajuste',
                           text = 'Exportar Excel'
                         ),
                         list(
                           extend = 'pdf',
                           filename = 'qualidade do ajuste',
                           text = 'Exportar PDF'
                         )
                       ))
    
    table=datatable(Ajuste, options = my.options,container = my.container,rownames = F, width = '100%', extensions = 'Buttons') %>% 
      formatStyle( 0, target= 'row',color = 'black', lineHeight='100%') %>% 
      formatStyle(columns = c(1),
                  width = '200px',
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
      formatStyle(columns = c(2:3),
                  width = '150px',
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
      formatRound(c(names(Ajuste)[2],names(Ajuste)[3]),digits = 4,mark = ".",dec.mark = ",")
    
    print(table)
  })
  
  output$table5.1 <- DT::renderDataTable({
    req(input$run_codes2)
    
    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = FALSE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       dom = 'Bfrtip',
                       buttons = list(
                         list(
                           extend = 'copy',
                           text = 'Copiar'
                         ),
                         list(
                           extend = 'csv',
                           filename = 'matrix covariância',
                           text = 'Exportar CSV'
                         ),
                         list(
                           extend = 'excel',
                           filename = 'matrix covariância',
                           text = 'Exportar Excel'
                         ),
                         list(
                           extend = 'pdf',
                           filename = 'matrix covariância',
                           text = 'Exportar PDF'
                         )
                       ))
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #15385A;}"
    
    if(input$model=="Exponential"){
      df=data.frame(vcov(exp()))
      header.names <- c("Scale")
      ncol=1
      
    } else if(input$model=="Weibull"){
      df=vcov(weibull())
      header.names <- c("Shape","Scale")
      ncol=2
      
    } else if(input$model=="Loglogistic"){
      df=vcov(loglogistic())
      header.names <- c("Shape","Scale")
      ncol=2
      
    } else if(input$model=="Gompertz"){
      df=vcov(gompertz())
      header.names <- c("Shape","Scale")
      ncol=2
      
    } else if(input$model=="Lognormal"){
      df=vcov(lognormal())
      header.names <- c("MeanLog","SDLog")
      ncol=2
      
    } else if(input$model=="Gengamma"){
      df=vcov(gengamma())
      header.names <- c("Mu","Sigma","Q")
      ncol=3
    }
    
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(lapply(header.names, th, style = "text-align: center; border-right: 1px solid white; border-bottom: 1px solid white"))
      )
    ))
    
    
    table=datatable(df, options = my.options,container = my.container,rownames = F, width = '100%', extensions = 'Buttons') %>% 
      formatStyle( 0, target= 'row',color = 'black', lineHeight='100%') %>% 
      formatStyle(columns = c(1:ncol),
                  width = '150px',
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
      formatRound(columns = c(1:ncol),digits = 4,mark = ".",dec.mark = ",")
    
    print(table)
  })
  
  output$table5.2 <- DT::renderDataTable({
    req(input$run_codes2)
    
    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = FALSE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       dom = 'Bfrtip',
                       buttons = list(
                         list(
                           extend = 'copy',
                           text = 'Copiar'
                         ),
                         list(
                           extend = 'csv',
                           filename = 'matrix covariância',
                           text = 'Exportar CSV'
                         ),
                         list(
                           extend = 'excel',
                           filename = 'matrix covariância',
                           text = 'Exportar Excel'
                         ),
                         list(
                           extend = 'pdf',
                           filename = 'matrix covariância',
                           text = 'Exportar PDF'
                         )
                       ))
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #15385A;}"
    
    if(input$model=="Exponential"){
      df=data.frame(t(chol(vcov(exp()))))
      header.names <- c("Scale")
      ncol=1
      
    } else if(input$model=="Weibull"){
      df=t(chol(vcov(weibull())))
      header.names <- c("Shape","Scale")
      ncol=2
      
    } else if(input$model=="Loglogistic"){
      df=t(chol(vcov(loglogistic())))
      header.names <- c("Shape","Scale")
      ncol=2
      
    } else if(input$model=="Gompertz"){
      df=t(chol(vcov(gompertz())))
      header.names <- c("Shape","Scale")
      ncol=2
      
    } else if(input$model=="Lognormal"){
      df=t(chol(vcov(lognormal())))
      header.names <- c("MeanLog","SDLog")
      ncol=2
      
    } else if(input$model=="Gengamma"){
      df=t(chol(vcov(gengamma())))
      header.names <- c("Mu","Sigma","Q")
      ncol=3
    }
    
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(lapply(header.names, th, style = "text-align: center; border-right: 1px solid white; border-bottom: 1px solid white"))
      )
    ))
    
    
    table=datatable(df, options = my.options,container = my.container,rownames = F, width = '100%', extensions = 'Buttons') %>% 
      formatStyle( 0, target= 'row',color = 'black', lineHeight='100%') %>% 
      formatStyle(columns = c(1:ncol),
                  width = '150px',
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
      formatRound(columns = c(1:ncol),digits = 4,mark = ".",dec.mark = ",")
    
    print(table)
  })
  
  fit_data <- reactive({
    req(input$run_codes)
    
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
    if(!is.null(input$file) & input$manter=="Sim"){
      df=database2()
    } else{
      df=data.frame(time=database2()[,input$var3],status=database2()[,input$var4])
      
      if(any(str_detect(df$time,","))==TRUE){
        df=df %>%
          mutate(time=scan(text=time, dec=","))
      } else{
        df=df %>%
          mutate(time=as.numeric(time))
      }
    }
    
    
    Exponential <- flexsurvreg(Surv(time, status)~1,data=df, dist="exp")
  })
  
  weibull <- reactive({
    if(!is.null(input$file) & input$manter=="Sim"){
      df=database2()
    } else{
      df=data.frame(time=database2()[,input$var3],status=database2()[,input$var4])
      
      if(any(str_detect(df$time,","))==TRUE){
        df=df %>%
          mutate(time=scan(text=time, dec=","))
      } else{
        df=df %>%
          mutate(time=as.numeric(time))
      }
    }
    
    Weibull <- flexsurvreg(Surv(time, status)~1,data=df, dist="weibullPH")
  })
  
  lognormal <- reactive({
    if(!is.null(input$file) & input$manter=="Sim"){
      df=database2()
    } else{
      df=data.frame(time=database2()[,input$var3],status=database2()[,input$var4])
      
      if(any(str_detect(df$time,","))==TRUE){
        df=df %>%
          mutate(time=scan(text=time, dec=","))
      } else{
        df=df %>%
          mutate(time=as.numeric(time))
      }
    }
    
    Lognormal <- flexsurvreg(Surv(time, status)~1,data=df, dist="lnorm")
  })
  
  gengamma <- reactive({
    if(!is.null(input$file) & input$manter=="Sim"){
      df=database2()
    } else{
      df=data.frame(time=database2()[,input$var3],status=database2()[,input$var4])
      
      if(any(str_detect(df$time,","))==TRUE){
        df=df %>%
          mutate(time=scan(text=time, dec=","))
      } else{
        df=df %>%
          mutate(time=as.numeric(time))
      }
    }
    
    Gengamma <- flexsurvreg(Surv(time, status)~1,data=df, dist="gengamma")
  })
  
  loglogistic <- reactive({
    if(!is.null(input$file) & input$manter=="Sim"){
      df=database2()
    } else{
      df=data.frame(time=database2()[,input$var3],status=database2()[,input$var4])
      
      if(any(str_detect(df$time,","))==TRUE){
        df=df %>%
          mutate(time=scan(text=time, dec=","))
      } else{
        df=df %>%
          mutate(time=as.numeric(time))
      }
    }
    
    Loglogistic <- flexsurvreg(Surv(time, status)~1,data=df, dist="llogis")
  })
  
  gompertz <- reactive({
    if(!is.null(input$file) & input$manter=="Sim"){
      df=database2()
    } else{
      df=data.frame(time=database2()[,input$var3],status=database2()[,input$var4])
      
      if(any(str_detect(df$time,","))==TRUE){
        df=df %>%
          mutate(time=scan(text=time, dec=","))
      } else{
        df=df %>%
          mutate(time=as.numeric(time))
      }
    }
    
    Gompertz <- flexsurvreg(Surv(time, status)~1,data=df, dist="gompertz")
  })
  
})

shinyApp(ui = ui, server = server)
