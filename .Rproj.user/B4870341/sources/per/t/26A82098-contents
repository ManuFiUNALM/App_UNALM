#Server

# datos <- read.csv('data/GSOD_mensual.csv')
# datos$FECHA <- as.Date(datos$FECHA, format="%Y-%m-%d")
# 
# stn_senamhi <- read.csv('data/estaciones_senamhi.csv')
# selrows <- c(1:dim(stn_senamhi)[1])
# 
# IR04 <- raster::raster("data/IR04_1704292345.tif")
# 
# rand_lng <- function(n = 10) rnorm(n, -93.65, .01)
# rand_lat <- function(n = 10) rnorm(n, 42.0285, .01)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$distPlot <- renderPlot({
    
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    })
    
    
    output$image01 <- renderImage({
                                list(src = "/media/manu/BD_manu/Shiny/New/luffy_dark.jpg",
                                     alt = "This is alternate text",
                                     height=100,
                                     width=200
                                    )
                                }, deleteFile = FALSE)
    
    
    output$met <- renderPlotly({
        datos_stn <- filter(datos, LUGAR==input$stn)
        #~!!as.name(input$select_stn)

#        datos_stn$datos_stn_var
        p <- plot_ly(x= datos_stn$FECHA, y= datos_stn[,input$select_stn] , type= 'scatter', mode='lines', name= 'T max')
        #p <- datos_stn %>%
        #    plot_ly(x= ~FECHA, y= ~MAX , type= 'scatter', mode='lines', name= 'T max') #%>%
            # add_trace(x=~FECHA, y= ~MAX, type= 'scatter', mode='lines',
            #           line = list(color='orangered', width= 2, dash= 'dash'), name= 'T max')
            
    })
    
    output$rows2 = renderPrint({
        cat('VARS_2 DISPLAY:\n\n')
        cat(input$select_stn)
    })
    
    
    output$table <- DT::renderDataTable(stn_senamhi, server=TRUE)
    
    output$rows = renderPrint({
        cat('Rows on the current page:\n\n')
        cat(input$table_rows_current, sep = ', ')
        cat('\n\nAll rows:\n\n')
        cat(input$table_rows_all, sep = ', ')
        # cat('\n')
        # cat(class(input$table_rows_all))
        # cat("\n\n just rows all - 12 :\n\n")
        # cat(input$table_rows_all - 12)
        cat('\n\nSelected rows:\n\n')
        cat(input$table_rows_selected, sep = ', ')
        #selrows <- c(input$table_rows_all)
    })
    
    
    pal <- colorNumeric(c('#0C2C84','#41B6C4','#FFFFCC'),
                        values(IR04), na.color = 'transparent')
    
    output$ubi_stn <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%

            addRasterImage(IR04, colors = pal,
                           opacity=0.2) %>%
            addLegend(pal= pal, values= values(IR04),
                      title='IR4') %>%
            
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
        
            addMarkers(data = stn_senamhi[input$selectrows, ],
                           lng = ~ lon, lat = ~ lat 
                       # icon = list(
                           #                iconUrl = 'http://icons.iconarchive.com/icons/artua/star-wars/128/Master-Joda-icon.png',
                           #                iconSize = c(75, 75)
                           #            )
                       ) %>%
        
            addCircles(data = stn_senamhi, lat = ~ lat, lng = ~ lon) %>%
        
            addPopups(-93.65, 42.0285, "Here is the <b>Department of Statistics</b>, ISU") %>%
            
            addCircleMarkers(rand_lng(50), rand_lat(50), color = "#ff0000")
    })
    
    
    output$stn_plot <- renderPlotly({
        
        if ((stn_senamhi[input$selectrows,'cate']=="HLM" || stn_senamhi[input$selectrows,'cate']=="HLG")){
            # 8, 2
            stn_selected <- paste0("/media/manu/BD_manu/Shiny/test/data/estaciones_senamhi/",stn_senamhi[input$selectrows,'cod'],"_Data.csv")
            #stn_selected <- paste0("/media/manu/BD_manu/Shiny/test/data/estaciones_senamhi/",stn_senamhi[2,'cod'],"_Data.csv")
            datos_stn <- read.csv(stn_selected, skip=1, na.strings = c("S/D"))
            
            #colnames(datos_stn)
            datos_stn$X0 <- NULL # X0 if skip=1, X if skip=0
            colnames(datos_stn)[colnames(datos_stn)=="AÑO...MES...DÍA"] <- "FECHA"
            #datos_stn <- select(datos_stn, c(X06, X10, X14, X18))
            #stn_data[, 2:5] <- sapply(stn_data[, 2:5], as.numeric)
            
            # stn_senamhi[input$selectrows, ],
            # datos_stn <- mutate(na.omit(datos_stn), nivel= (X06+X10+X14+X18)/4)
            # datos_stn$nivel <- ave(datos_stn$X06, datos_stn$X10,datos_stn$X14, datos_stn$X18)
            datos_stn <- transform(datos_stn, nivel = rowMeans(datos_stn[,-1], na.rm = TRUE))
            
            p <- plot_ly(x= datos_stn$FECHA, y= datos_stn[,input$selectvar], type= 'scatter', mode='lines', name= input$selectvar)
            # p <- datos_stn %>% plot_ly(x= ~FECHA, y= ~nivel, type= 'scatter', mode='lines', name= 'nivel') #%>%
            # add_trace(x=~FECHA, y= ~MAX, type= 'scatter', mode='lines',
            #           line = list(color='orangered', width= 2, dash= 'dash'), name= 'T max')
        }
        
        if ((stn_senamhi[input$selectrows,'cate']=="EHA" || stn_senamhi[input$selectrows,'cate']=="EHMA")){
            #822, 666
            stn_selected <- paste0("/media/manu/BD_manu/Shiny/test/data/estaciones_senamhi/",stn_senamhi[input$selectrows,'cod'],"_Data.csv")
            datos_stn <- read.csv(stn_selected, skip=0, na.strings = c("S/D"))
            colnames(datos_stn)
            datos_stn$X <- NULL
            colnames(datos_stn)[colnames(datos_stn)=="AÑO...MES...DÍA"] <- "FECHA"
            colnames(datos_stn)[colnames(datos_stn)=="NIVEL.DEL.RIO..m."] <- "nivel"
            colnames(datos_stn)[colnames(datos_stn)=="PRECIPITACIÓN..mm.hora."] <- "PP"
            
            # as.Date(x, format, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"),
            #         optional = FALSE, ...)
            
            # as.POSIXct(df$Date, "%m/%d/%Y %H:%M:%S")    
            # strptime(df$Date, "%m/%d/%Y %H:%M:%S")
            
            p <- plot_ly(x= datos_stn$FECHA, y= datos_stn[,input$selectvar], type= 'scatter', mode='lines', name= input$selectvar)
        }
        
        if ((stn_senamhi[input$selectrows,'cate']=="EAMA" || stn_senamhi[input$selectrows,'cate']=="EMA" || stn_senamhi[input$selectrows,'cate']=="EAA")){
            #665, 670, 717
            stn_selected <- paste0("/media/manu/BD_manu/Shiny/test/data/estaciones_senamhi/",stn_senamhi[input$selectrows,'cod'],"_Data.csv")
            datos_stn <- read.csv(stn_selected, skip=0, na.strings = c("S/D"))
            colnames(datos_stn)
            datos_stn$X <- NULL
            colnames(datos_stn)[colnames(datos_stn)=="AÑO...MES...DÍA"] <- "FECHA"
            colnames(datos_stn)[colnames(datos_stn)=="TEMPERATURA...C."] <- "TEMP"
            colnames(datos_stn)[colnames(datos_stn)=="PRECIPITACIÓN..mm.hora."] <- "PP"
            colnames(datos_stn)[colnames(datos_stn)=="HUMEDAD...."] <- "HR"
            colnames(datos_stn)[colnames(datos_stn)=="DIRECCION.DEL.VIENTO...." ] <- "VDIR"
            colnames(datos_stn)[colnames(datos_stn)=="VELOCIDAD.DEL.VIENTO..m.s."] <- "VVEL"
            
            # as.Date(x, format, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"),
            #         optional = FALSE, ...)
            
            # as.POSIXct(df$Date, "%m/%d/%Y %H:%M:%S")    
            # strptime(df$Date, "%m/%d/%Y %H:%M:%S")
            
            p <- plot_ly(x= datos_stn$FECHA, y= datos_stn[,input$selectvar], type= 'scatter', mode='lines', name= input$selectvar)
        }
        
        
        if ((stn_senamhi[input$selectrows,'cate']=="PE")){
            # 18
            stn_selected <- paste0("/media/manu/BD_manu/Shiny/test/data/estaciones_senamhi/",stn_senamhi[input$selectrows,'cod'],"_Data.csv")
            #stn_selected <- paste0("/media/manu/BD_manu/Shiny/test/data/estaciones_senamhi/", stn_senamhi[588,'cod'], "_Data.csv")
            datos_stn <- read.csv(stn_selected, skip=1, na.strings = c("S/D"))
            #colnames(datos_stn)
            datos_stn$X0 <- NULL
            colnames(datos_stn)[colnames(datos_stn)=="AÑO...MES...DÍA"] <- "FECHA"
            # as.Date(x, format, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"),
            #         optional = FALSE, ...)
            colnames(datos_stn)[colnames(datos_stn)=="HUMEDAD.RELATIVA...."] <- "HR"
            colnames(datos_stn)[colnames(datos_stn)=="TOTAL"] <- "PP"
            #stn_data <- select(stn_data, c(X06, X10, X14, X18))
            #stn_data[, 2:5] <- sapply(stn_data[, 2:5], as.numeric
            
            p <- plot_ly(x= datos_stn$FECHA, y= datos_stn[,input$selectvar], type= 'scatter', mode='lines', name= input$selectvar)
            # add_trace(x=~FECHA, y= ~MAX, type= 'scatter', mode='lines',
            #           line = list(color='orangered', width= 2, dash= 'dash'), name= 'T max')
        }
        
        if ((stn_senamhi[input$selectrows,'cate']=="CO" || stn_senamhi[input$selectrows,'cate']=="CP" || stn_senamhi[input$selectrows,'cate']=="MAP")){
            #CO 588
            #MAP 180
            #CP 249
            stn_selected <- paste0("/media/manu/BD_manu/Shiny/test/data/estaciones_senamhi/",stn_senamhi[input$selectrows,'cod'],"_Data.csv")
            #stn_selected <- paste0("/media/manu/BD_manu/Shiny/test/data/estaciones_senamhi/", stn_senamhi[588,'cod'], "_Data.csv")
            datos_stn <- read.csv(stn_selected, skip=1, na.strings = c("S/D"))
            #colnames(datos_stn)
            datos_stn$X0 <- NULL
            colnames(datos_stn)[colnames(datos_stn)=="AÑO...MES...DÍA"] <- "FECHA"
            # as.Date(x, format, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"),
            #         optional = FALSE, ...)
            colnames(datos_stn)[colnames(datos_stn)=="HUMEDAD.RELATIVA...."] <- "HR"
            colnames(datos_stn)[colnames(datos_stn)=="TOTAL"] <- "PP"
            #stn_data <- select(stn_data, c(X06, X10, X14, X18))
            #stn_data[, 2:5] <- sapply(stn_data[, 2:5], as.numeric

            p <- plot_ly(x= datos_stn$FECHA, y= datos_stn[,input$selectvar], type= 'scatter', mode='lines', name= input$selectvar)
            # add_trace(x=~FECHA, y= ~MAX, type= 'scatter', mode='lines',
            #           line = list(color='orangered', width= 2, dash= 'dash'), name= 'T max')
        }
        
        if ((stn_senamhi[input$selectrows,'cate']=="PLU")){
            # 4
            #stn_selected <- paste0("/media/manu/BD_manu/Shiny/test/data/estaciones_senamhi/",stn_senamhi[input$selectrows,'cod'],"_Data.csv")
            stn_selected <- paste0("/media/manu/BD_manu/Shiny/test/data/estaciones_senamhi/", stn_senamhi[4,'cod'], "_Data.csv")
            datos_stn <- read.csv(stn_selected, skip=0, na.strings = c("S/D"))
            colnames(datos_stn)
            datos_stn$X <- NULL
            colnames(datos_stn)[colnames(datos_stn)=="AÑO...MES...DÍA"] <- "FECHA"
            # as.Date(x, format, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"),
            #         optional = FALSE, ...)
            colnames(datos_stn)[colnames(datos_stn)=="TEMPERATURA...C."] <- "MAX"
            colnames(datos_stn)[colnames(datos_stn)=="TEMPERATURA...C..1"] <- "MIN"
            colnames(datos_stn)[colnames(datos_stn)=="HUMEDAD.RELATIVA...."] <- "HR"
            colnames(datos_stn)[colnames(datos_stn)=="PRECIPITACIÓN..mm.día." ] <- "PP"
            #stn_data <- select(stn_data, c(X06, X10, X14, X18))
            #stn_data[, 2:5] <- sapply(stn_data[, 2:5], as.numeric
            
            p <- plot_ly(x= datos_stn$FECHA, y= datos_stn[,input$selectvar], type= 'scatter', mode='lines', name= input$selectvar)
            # add_trace(x=~FECHA, y= ~MAX, type= 'scatter', mode='lines',
            #           line = list(color='orangered', width= 2, dash= 'dash'), name= 'T max')
        }
        
        
    })
        
})

# agregar marcadores por grupos
# https://stackoverflow.com/questions/54978367/custom-markers-with-different-shapes-and-colors-in-leaflet-r

# LINKS para interactuar entre inputs y outputs
# https://stat545.com/shiny-tutorial.html
# https://stackoverflow.com/questions/42565384/in-shiny-is-it-possible-to-select-rows-from-dt-by-clicking-as-reactive-input
# # https://rstudio.github.io/DT/shiny.html
# https://yihui.shinyapps.io/DT-info/
# https://rstudio.github.io/leaflet/map_widget.html
