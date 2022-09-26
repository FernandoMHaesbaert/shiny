library(shiny)
library(shinydashboard)
library(tidyverse)

# Definindo a interface do aplicativo
ui <- fluidPage(
    # Application title
    titlePanel("Distribuição Binomial"),
    # Barra de seleção do número de experimentos 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "n",
                        label = "n:",
                        min = 0,
                        max = 20,
                        value = 10),
            sliderInput(inputId = "p",
                      label = "p:",
                      min = 0,
                      max = 1,
                      value = 0.5)
        ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput(outputId = "grafico")
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$grafico <- renderPlot({
      x <- seq(0, input$n, 1)
      p_x <- round(dbinom(x, input$n, input$p), 3) # distribuição dos resultados
      a <- data.frame(x, p_x) # cria um dataframe para usar ggplot
      ggplot(a, aes(x = x, y = p_x)) + 
        geom_bar(stat = "identity", fill = "steelblue")+
        ggtitle(paste("Distribuição Binomial, 
                      n = ", input$n, ", p = ", input$p))+
        geom_text(aes(label = p_x), vjust = 1.6, 
                  color = "white", size = 3.5)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
