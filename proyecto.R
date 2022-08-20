# APP

# Cargar el paquete Shiny
library(shiny)
library(shinydashboard)
library(leaflet)

source('base_estaciones/helper.R')

ui <-dashboardPage(
  
  # 1. Cabecera el dashboard
  dashboardHeader(
    
    title = 'MetUNALM',
    titleWidth = 150,   #tamanho de la casilla del titulo
  
    # 1.x Menu de notificaciones
    dropdownMenu(type = 'notifications',
	  
      notificationItem(
        
        text = 'Alertas del G16',
        icon = icon('exclamation-triangle'),
        status = 'warning',
        
      )
      
    )
    
  ),
  
  # 2. Menu lateral / Sidebar content
  dashboardSidebar(
    
    width = 150,
    
    # 2.x similar to tabPanels
    sidebarMenu(
      
      # headers y sus imagenes
      menuItem('Nowcast', tabName = 'nowcast', icon = icon('dashboard')),
      # menuItem('Analisis', tabName = 'analisis', icon = icon('th')),
      # menuItem('Sinoptica', tabName = 'sinoptica', icon = icon('cloud')),
      menuItem('Estaciones', tabName='estaciones', icon=icon("table"))
      # menuItem('Reporte', tabName = 'doreporte', icon = icon('file')),
      # menuItem('Pronostico', tabName = 'pronos', icon = icon('list-alt')),
      # menuItem('Links', tabName = 'links' , icon = icon('fab fa-chrome')),
      # menuItem('Directivas', tabName = 'directivas', icon = icon('fas fa-book')),
      # menuItem('Glosario', tabName = 'glosario', icon = icon('book open'))
      
    )
    
  ),
  
  # 3. Cuerpo del dashboard
  dashboardBody(
    
    # tabs 
    tabItems(
      
      ####################################################
      # Sección Nowcast del menú lateral
      tabItem(tabName = 'nowcast', #),
              
              # Boxes need to be put in a row (or column)
              fluidRow(
                
                  #Imagen GOES 16 - 1ra columna
                  column(width = 8,
                         
                         box(width = NULL,
                             uiOutput('imgUI')
                             # imageOutput('image01')
                         ),
                  ),
                  #Parámetros de selección de imagen - 2da columna
                  column(width = 4,
                         
                       box(width = NULL,
                            radioButtons('loop', label = 'Animación',
                                         choices = list('Si' = 'B', 'No' = 'c'),
                                         # Si --> Bch / No--> Cch
                                         selected = 'c'
                            )

                        ),
                         
                        box(width = NULL,
                            radioButtons('banda', label = 'Selección de bandas',
                                        choices = list('Banda 02' = '02', 'Banda 07' = '07', 'Banda 08' = '08',
                                                       'Banda 09' = '09', 'Banda 13' = 13, 'Banda 14' = 14,
                                                       'Banda 15' = 15, 'Autoestimador'=18),
                                        selected = '13'
                            )
                        ),
                        
                        box(width = NULL,
                            sliderInput('numima', label = 'Número de imagen',
                                        min = 1, max = 10, value = 10 )
                        ),
                        
                        # valueBoxOutput("relojillo", width=4),
                        # valueBoxOutput("reloj_utc",width=4)
                  
                   ) #no coma 
               ), # no coma
      ),
      
      # ####################################################
      # #Sección de analisis de imágenes
      # tabItem(tabName = 'analisis',
      #         
      #         fluidRow(
      #           
      #           #Gráfica Leaflet
      #           box(width = 8,
      #               
      #               leafletOutput('g16', height = 700)
      #               
      #           ),
      #           
      #           #Parámetros de selección de imágenes
      #           box(title = 'Selección de imagen', width = 2, heigth = 800,
      #               
      #               radioButtons('banda2', label = 'Bandas', 
      #                            choices = list('Banda 02' = '02', 'Banda 07' = '07', 
      #                                           'Banda 08' = '08', 'Banda 09' = '09', 'Banda 13' = '13', 
      #                                           'Banda 14' = '14', 'Banda 15' = '15'),
      #                            selected = '13'
      #               )
      #               
      #           ),
      #           
      #           box(width=4, 
      #               
      #               sliderInput("shingeky",label='Opacidad de la Imagen',
      #                           min = 0.0, max = 1.0, value = 1.0
      #               )
      #               
      #           )
      #           
      #         )
      #         
      # ),
      
      
      # ####################################################
      # # Sección de sinoptica, solo selecciona gráficas
      # tabItem(tabName = 'sinoptica',
      #         
      #         box(width = 6,
      #             
      #             selectInput('sinop_1',
      #                         
      #                         label = h4('Seleccionar una gráfica'),
      #                         choices = sinls,
      #                         selected = head(sinls,1)
      #                         
      #             ),
      #             
      #         #     imageOutput('sinopUI1', height = alto_box())
      #         #     
      #         # ),
      #         # 
      #         # box(width = 6,
      #         #     
      #         #     selectInput('sinop_2',
      #         #                 
      #         #                 label = h4('Seleccionar otra gráfica'),
      #         #                 choices = sinls,
      #         #                 selected = tail(sinls,1)
      #         #                 
      #         #     ),
      #         #     
      #         #     imageOutput('sinopUI2', height = alto_box())
      #             
      #         )
      #         
      # ),
      
      
      
      # ####################################################
      # #Sección de analisis de imágenes
      # tabItem(tabName = 'analisis',
      #         
      #         fluidRow(
      #           
      #           #Gráfica Leaflet
      #           box(width = 8,
      #               
      #               leafletOutput('g16', height = 700)
      #               
      #           ),
      #           
      #           #Parámetros de selección de imágenes
      #           box(title = 'Selección de imagen', width = 2, heigth = 800,
      #               
      #               radioButtons('banda2', label = 'Bandas', 
      #                            choices = list('Banda 02' = '02', 'Banda 07' = '07', 
      #                                           'Banda 08' = '08', 'Banda 09' = '09', 'Banda 13' = '13', 
      #                                           'Banda 14' = '14', 'Banda 15' = '15'),
      #                            selected = '13'
      #               )
      #               
      #           ),
      #           
      #           box(width=4, 
      #               
      #               sliderInput("shingeky",label='Opacidad de la Imagen',
      #                           min = 0.0, max = 1.0, value = 1.0
      #               )
      #               
      #           )
      #           
      #         )
      #         
      
      
      ####################################################
      #Sección de estaciones
      tabItem(tabName = 'estaciones',
              # Boxes need to be put in a row (or column)
              fluidRow(

                column(width = 6,
                       
                       box(width = NULL,
                           selectInput('stn', label='Estaciones', choices=stn_ls,
                                       selected='EL PINTOR'),
                           plotOutput("met"),
                           # uiOutput('imgUI')
                           # imageOutput('image01')
                       ),
                ),

                column(width = 6,

                       box(width = NULL,
                           leafletOutput('ubi_stn'),

                       ),
                )
                
              )
              
              
              
              
      )#,
      
      

  ),
  
),
)



server <- function(input, output, session){
  
  options(stringsAsFactors = FALSE)
  
  #####################################################################
  #NOWCAST
  
  # lista <- reactive({
  #   # lista <-
  #   list.files('/media/manu/Manu/Meteomanu/R-Ladies_Shiny/OpenCloud-master/www/G16/', pattern = paste0(input$loop,input$banda))
  #   # link <- ifelse(input$loop == 'c', lista[input$numima], lista)
  #   # outfile <- ifelse(input$loop == 'c', lista[input$numima], lista)
  # })
  
  #UI con link dinámico para abrir la imagen del G16
  output$imgUI <- renderUI({ #renderImage({ #renderUI({
    
    lista <- list.files('/media/manu/Manu/Meteomanu/R-Ladies_Shiny/OpenCloud-master/www/G16/', pattern = paste0(input$loop,input$banda))
    link <- ifelse(input$loop == 'c', lista[input$numima], lista)
    
    tags$a(
      imageOutput('img', height = '825px'),
      #src = paste0('/media/manu/Manu/Meteomanu/R-Ladies_Shiny/OpenCloud-master/www/G16/', link),
      href = paste0('/media/manu/Manu/Meteomanu/R-Ladies_Shiny/OpenCloud-master/www/G16/', link),
      #dante usaba http://127.0.0.1:6335/G16/name_figure
      target = '_blank'
    )
    
  })
  
  #UI de la imagen G16
  output$img <- renderImage({
    
    lista <- list.files('/media/manu/Manu/Meteomanu/R-Ladies_Shiny/OpenCloud-master/www/G16/', pattern = paste0(input$loop,input$banda), full.name = T)
    outfile <- ifelse(input$loop == 'c', lista[input$numima], lista)
    
    list(src = outfile,
         #contentType = 'image/png',
         width = 880,
         heigth = 825,
         alt = 'Generando imagen'
    )
    
  }, deleteFile = FALSE)
  
  output$relojillo <- renderValueBox({
    
    valueBox(invalidateLater(60000, session), 
             icon = icon("time", lib = "glyphicon"),
             color = "light-blue",
             paste("Hora local: ",format(Sys.time(),format='%H:%M'),"Lima,Peru")
    )
    
  }) 
  
  output$reloj_utc <- renderValueBox({
    valueBox( invalidateLater(60000, session), 
              icon = icon("time", lib = "glyphicon"),
              color = "lime",
              paste("Hora UTC: " ,format(as.POSIXlt(Sys.time(),format="%H:%M:%S",tz="UTC"),"%H:%M"),"LSRGM,UNALM"))
  })                
  
  
  # #####################################################################
  # #ANALISIS
  # 
  # #UI del Leaflet
  # output$g16<-renderLeaflet({
  #   
  #   # lista3 <- list.files('Data/', full.name = T)
  #   # ruta <- paste0('D:/G16_PE/Band', input$banda2)
  #   # lista3 <- list.files(ruta, pattern = 'M3', full.name = T)
  #   lista3 <- list.files('Data/', pattern = input$banda2, full.name = T)
  #   rtr <- raster(tail(lista3, 1))
  #   
  #   rtr <-fact(rtr,as.integer(input$banda2)) 
  #   
  #   if(as.integer(input$banda2)==2){
  #     rtr[rtr<30] <- NA	
  #   }
  #   
  #   else if(as.integer(input$banda2)==7){
  #     rtr [rtr>0] <- NA
  #   }
  #   
  #   else{
  #     rtr[rtr>-40] <- NA
  #   }
  #   
  #   if(as.integer(input$banda2)==2){
  #     titan=" (%)"
  #   } 
  #   else{
  #     titan='   (ºC)'
  #   }
  #   
  #   
  #   pal <- colorNumeric(c('grey99', 'black'), values(rtr), na.color='transparent')
  #   
  #   leaflet()%>% 
  #     addTiles()%>% 
  #     setView(lng = -76, lat = -12, zoom = 7)%>%
  #     addRasterImage(rtr, colors = pal, input$shingeky)%>%
  #     addLegend(pal = pal, values = values(rtr), title = paste0('Banda',input$banda2,titan))
  #   
  # })
  
  
  #####################################################################
  # SINOPTICA
  
  # # Dimensiones de las imagenes
  # ancho <- 1200
  # alto <- 926
  # 
  # # Cargando las imágenes
  # output$sinopUI1 <- renderImage({
  #   
  #   # Tamaño del box que tendrá la imagen
  #   width  <- session$clientData$output_sinopUI1_width
  #   height <- round(alto*width/ancho, 0)
  #   
  #   # Ruta de imagen
  #   sinim1 <- paste0('/media/manu/Manu/Meteomanu/R-Ladies_Shiny/OpenCloud-master/www/GFS/', input$sinop_1)
  #   
  #   # Parámetros de la imagen
  #   list(
  #     src = sinim1,
  #     width = width,
  #     height = height,
  #     alt = 'No hay imagen'
  #   )
  #   
  # }, deleteFile = FALSE)
  # 
  # output$sinopUI2 <- renderImage({
  #   
  #   # Tamaño del box que tendrá la imagen
  #   width  <- session$clientData$output_sinopUI2_width
  #   height <- round(alto*width/ancho, 0)
  #   
  #   # Ruta de imagen
  #   sinim2 <- paste0('/media/manu/Manu/Meteomanu/R-Ladies_Shiny/OpenCloud-master/www/GFS/', input$sinop_2)
  #   
  #   # Parámetros de la imagen
  #   list(
  #     src = sinim2,
  #     width = width,
  #     height = height,
  #     alt = 'No hay imagen'
  #   )
  #   
  # }, deleteFile = FALSE)
  
  
  #####################################################################
  #estaciones

  output$met <- renderPlot({
    
    # library(readxl)
    # stn_metadata <- read_excel("base_estaciones/ACTUALIZANDO_pp_estaciones_Peru.xlsx", sheet = "Metadata_solo_Maranon")
    # stn_pp <- read_excel("base_estaciones/ACTUALIZANDO_pp_estaciones_Peru.xlsx", sheet = "pp_estaciones_Peru", skip=1, col_names = TRUE, col_types= c("date",rep("numeric",87)))
    #stn_metadata <- stn_metadata
    #stn_pp <- stn_pp
    
    #stn_ls <- stn_metadata$Estaciones
    vec_N <- stn_metadata %>% pull("N")
    idx <- as.Date(stn_pp$"...1")
    data.matrix <- stn_pp[,-1][vec_N]
    data.xts <- xts(data.matrix, order.by = idx )
    
    stn_id <- which.max(stn_metadata$Estaciones==input$stn)
    datos_stn <- data.xts[,stn_id]
    #datos_stn <- filter(datos, LUGAR==input$stn)
    #~!!as.name(input$select_stn)
    
    #        datos_stn$datos_stn_var
    #p <- 
    plot(datos_stn, ylim=c(0,100))
    #p <- plot_ly(x= datos_stn$FECHA, y= datos_stn[,input$select_stn] , type= 'scatter', mode='lines', name= 'T max')
    #p <- datos_stn %>%
    #    plot_ly(x= ~FECHA, y= ~MAX , type= 'scatter', mode='lines', name= 'T max') #%>%
    # add_trace(x=~FECHA, y= ~MAX, type= 'scatter', mode='lines',
    #           line = list(color='orangered', width= 2, dash= 'dash'), name= 'T max')
    
  })
  
  
  output$ubi_stn <- renderLeaflet({
    
    stn_id <- which.max(stn_metadata$Estaciones==input$stn)
    
    leaflet() %>%
      addTiles() %>%
      
      # addRasterImage(IR04, colors = pal,
      #                opacity=0.2) %>%
      # addLegend(pal= pal, values= values(IR04),
      #           title='IR4') %>%
      
      ## marker en un punto
      # addMarkers(
      #     lng= c(-71), lat= c(-12),
      #     icon = NULL,
      #         # list(
      #         #        iconUrl = "https://www.freeiconspng.com/uploads/black-circle-icon-23.png",  # circulo gris
      #         #        iconUrl = "https://www.freeiconspng.com/uploads/black-square-frame-23.png", # cuadrado gris
      #         #        iconUrl = "https://www.freeiconspng.com/uploads/red-triangle-png-20.png",     # triangulo rojo
      #         #        iconUrl = 'http://icons.iconarchive.com/icons/artua/star-wars/128/Master-Joda-icon.png',
      #         #        iconSize = c(75,75)
    #         #    ),
    #     popup = c("RStudio @ CiudadX")
    # ) %>%
    
    
    # addMarkers(data = stn_metadata[stn_id, ],
    #            lng = ~ Longitud, lat = ~ Latitud 
    #            # icon = list(
    #            #                iconUrl = 'http://icons.iconarchive.com/icons/artua/star-wars/128/Master-Joda-icon.png',
    #            #                iconSize = c(75, 75)
    #            #            )
    # ) %>%
      
      addCircles(data = stn_metadata, lat = ~ Latitud, lng = ~ Longitud) %>%
      addCircles(data = stn_metadata[stn_id, ], lat = ~ Latitud, lng = ~ Longitud, color='red')
        
      #addPopups(-93.65, 42.0285, "Here is the <b>Department of Statistics</b>, ISU") %>%
      
      #addCircleMarkers(rand_lng(50), rand_lat(50), color = "#ff0000")
  })
  
  
}

# Construir y ejecutar la app creada
shinyApp(ui, server)

# ruta_x <- "C:/Users/Usuario/Desktop/OpenCloud-master/www/G16"
# ruta_x <- "/media/manu/Manu/Meteomanu/R-Ladies_Shiny"
# setwd(ruta_x)





# As a researcher, I want to create collaborative projects with my students that aim to generate research products that benefit my university department and different local communities.