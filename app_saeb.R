# Carregando pacotes
library(shiny)
library(tidyverse)

# Importando os dados
# base_saeb = read_excel("Base SAEB.xlsx")
# 
# # dependencia administrativa
# base_saeb$depADM5EF <- factor(base_saeb$depADM5EF,levels=c(0,1,2),
#                               labels=c("Municipal","Estadual","Federal"))
# 
# # escolaridade da mãe
# base_saeb$esc_mae5EF <- factor(base_saeb$esc_mae5EF,levels=c(0,1,2,3),
#                                labels=c("Até EF incompleto","EF completo","EM completo",
#                                         "Ensino superior completo ou mais"))
# # escolaridade do pai
# base_saeb$esc_pai5EF <- factor(base_saeb$esc_pai5EF,levels=c(0,1,2,3),
#                                labels=c("Até EF incompleto","EF completo","EM completo",
#                                         "Ensino superior completo ou mais"))
# 
# # raça
# base_saeb$raca5EF <- factor(base_saeb$raca5EF,levels=c(0,1),
#                             labels=c("Branco","Não branco"))
# 
# # turno
# base_saeb$turno5EF <- factor(base_saeb$turno5EF,levels=c(0,1,2),
#                              labels=c("Manhã","Tarde","Noite"))
# 
# # local
# base_saeb$local5EF <- factor(base_saeb$local5EF,levels=c(0,1,2,3),
#                              labels=c("RJ","CE","ES","SP"))
# 
# # sexo
# base_saeb$sexo <- factor(base_saeb$sexo,levels=c(0,1),
#                          labels=c("Feminino","Masculino"))
# 
# # série
# base_saeb$serie <- factor(base_saeb$serie,levels=c(1,2,3),
#                           labels=c("5º ano EF","9º ano EF","3ª série EM"))
# 
# base_saeb = base_saeb |> 
#     filter(serie== "5º ano EF") |> 
#     rename(depADM=depADM5EF, esc_mae= esc_mae5EF, esc_pai= esc_pai5EF, raca=raca5EF, turno=turno5EF, local=local5EF)

load("base_saeb.Rdata")

# Definindo o objeto UI no App para a criação do histograma
ui <- fluidPage(
  
  # Titúlo do App
  titlePanel("Proficiência em Português"),
  
  # Incluindo uma barra lateral que permite o usuário definir o número de intervalos 
  sidebarLayout(
    sidebarPanel(
      sliderInput("intervalos",
                  "Número de intervalos:",
                  min = 1,
                  max = 30,
                  value = 15)
    ),
    
    # Mostra o gráfico
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Definindo a lógica do server necessária para construir o histograma
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # gera o número de interevalos com base em input$intervalos de ui.R
    x    <- base_saeb$prof_port
    intervalos <- seq(min(x), 
                      max(x), 
                      length.out = input$intervalos + 1)
    
    # faz o histograma com o número de intervalos especificados
    ggplot(data = base_saeb,
           mapping = aes(x = prof_port)) +
      geom_histogram(breaks = intervalos,
                     color = "white",
                     fill = "red") +
      theme_classic() +
      labs(x = "Proficiência em Português",
           y = "Frequência")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

