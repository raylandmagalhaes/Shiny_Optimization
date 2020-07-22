library(reembolsos)
library(tidyverse)
library(plotly)
library(shinydashboard)
library(DT)

load("camara")


shinyApp(
  
  ui= dashboardPage(
    
    dashboardHeader(title = "Pedidos de reembolso dos deputados federais brasileiros"
    ),
    
    dashboardSidebar( sidebarMenu(
      # menuItem("Barplot", tabName = "bar", icon = icon("bar-chart", lib = "font-awesome")),
      # menuItem("Tabela", tabName = "tabela", icon = icon("table",lib = "font-awesome")),
      menuItem("Reembolsos", tabName = "dash", icon = icon("search dollar",lib = "font-awesome"))
    )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "dash",
                fluidRow(
                  box(width = 12,height = 100,status = "warning", title = "DIGITE O NOME DO DEPUTADO FEDERAL:",
                      background = "light-blue",
                      selectInput("parl",label = NULL, choices =unique(as.character(camara$congressperson_name)),selected=sample(unique(as.character(camara$congressperson_name)),1))),
                  box(width = 12,height = 100,status = "warning", title = "SELECIONE O TIPO DE GASTO",
                      background = "light-blue",
                      selectInput("tipo",label = NULL, choices =unique(as.character(camara$subquota_description),selected=sample(unique(as.character(camara$subquota_description)),1))),
                      box(status = "warning",title="Distribuição de gastos ao se remover determinada empresa",width = 12,
                          plotlyOutput('densidades')),
                      box( width = 12, status = "warning",
                           DTOutput('tabela'),
                           tags$head(tags$style("#tabela table {color:black;font-weight:bold; }", media="screen", type="text/css")))
                      
                  )
                  
                )
        )
      )
    )
    #https://rstudio.github.io/shinydashboard/structure.html#mixed-row-and-column-layout
  ),
  
  server= function(input, output, session) {
    
    dados <- reactive({ 
      
      gastos <- camara %>%
        filter(congressperson_name==input$parl,subquota_description==input$tipo) %>% 
        select(supplier,total_net_value,cnpj_cpf) 
      
      
      fornecedores <- gastos %>%
        select(cnpj_cpf) %>%
        unique() %>% 
        pull()
      
      dadosx <- gastos %>% 
        mutate(sem="DENSIDADE COMPLETA") %>% 
        select(-cnpj_cpf,-supplier)
      
      for(i in fornecedores){
        
        dadosx=bind_rows(dadosx, gastos %>% 
                           filter(cnpj_cpf!=i) %>% 
                           mutate(sem=sample(gastos %>% filter(cnpj_cpf==i) %>% select(supplier) %>% unique() %>% pull,1))) %>% 
          select(-cnpj_cpf,-supplier)
      }
      dadosx
    })
    
    
    
    
    
    
    
    output$densidades=renderPlotly({
      ggplotly(ggplot(data=dados(),aes(total_net_value))+
                 geom_density(aes(colour=sem))+
                 theme_bw()
               # +
               #   theme(legend.position = "none")
      )
    })
    
    datasetInput <- reactive({
      
      camara %>%
        filter(congressperson_name==input$parl,subquota_description==input$tipo) %>% 
        select(Valor_Reembolsado=total_net_value,Tipo_de_gasto=subquota_description,Fornecedor=supplier,cnpj_cpf,Data=issue_date) %>% 
        datatable()
      
    })
    
    output$tabela <- renderDT(
      datasetInput(), # data
      class = "display nowrap compact", # style
      filter = "top"
    )
    
  }
)