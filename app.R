# Cargar el paquete Shiny
library(shiny)

# Definir la interfaz de usuario
ui <- fluidPage(                                                                
  # crea toda la pagina con altura ajustable (con barra lateral)
  # inicializa todo el HTML, CSS y JS
  
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  # control de input que crea  una lista de opciones para interactuar 
  # ARGS:   # identificador , texto descriptivo, de donde saca las opciones que se visualizaran
  
  
  verbatimTextOutput("resumen"),
  # control de output
  
  tableOutput("tabla")
  # control de output
)

# Definir la interactividad de la app
server <- function(input, output, session) {
  # session -> para anadir funcionalidades interactivas de java

  # output$resumen <- renderPrint({
  #   dataset <- get(input$dataset, "package:datasets") # se repite esta linea, esto se quita con prog reactiva
  #   summary(dataset)
  # })
  # 
  # output$tabla <- renderTable({
  #   dataset <- get(input$dataset, "package:datasets") # se repite esta linea, esto se quita con prog reactiva
  #   dataset
  # })
  
  # Creamos una expresión reactiva
  # ni bien detecte un cambio ejecutara este codigo
  dataset <- reactive({
    get(input$dataset, "package:datasets")          # () como su fuera una funcion - reactividad
  })
  
  output$resumen <- renderPrint({
    # Usamos la expresión reactiva como si fuese una función
    summary(dataset())                              # () como su fuera una funcion - reactividad
  })
  
  output$tabla <- renderTable({
    dataset()
  })
  
}


# Construir y ejecutar la app creada
shinyApp(ui, server)
