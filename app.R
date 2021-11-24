library(shiny)
library(DT)
library(ggplot2)
data("iris")

ui <- fluidPage(
  titlePanel("Izdari izvelni"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Jaizvelas Suga un veids, ka to attelot"),
      
      selectInput("Suga",
                  label = "Izvelies vienu Sugu",
                  choices = c("setosa",
                              "versicolor",
                              "virginica"),
                  selected = "setosa"
      ),
      
      radioButtons("Veids",
                   label = "Izvelies attela veida",
                   choices = list("Histogramma",
                               "Izkliedes",
                               "Boxplots"),
                   selected = "Izkliedes"
      )
    ),
    
    mainPanel(
      h2("Tabula ar datiem"),
      dataTableOutput("Izveleta_tabula"),
      br(),
      h2("Izveletais attels"),
      plotOutput("Izveletais_attels")
    )
  )
  
)

server <- function(input, output) {
  
  dati <- reactive({
    iris[iris$Species == input$Suga,]
    })
  
  output$Izveleta_tabula <- renderDataTable(dati(), options = list(pageLengtht = 5))
  
  output$Izveletais_attels <- renderPlot({
    if(input$Veids == "Izkliedes"){
    ggplot(dati(), aes(Sepal.Length, Sepal.Width)) + geom_point() +
      theme_classic()
  } else if(input$Veids == "Histogramma") {
    ggplot(dati(), aes(Sepal.Length)) + geom_histogram(bins = 10) +
      theme_classic()
  } else {
    ggplot(dati(), aes("", Sepal.Length)) + geom_boxplot(fill = "skyblue") +
      theme_classic()
  }
  })
}

shinyApp(ui = ui, server = server)
