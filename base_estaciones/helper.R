#datos <- read.csv('data/GSOD_mensual.csv')
#datos$FECHA <- as.Date(datos$FECHA, format="%Y-%m-%d")

#stn_senamhi <- read.csv('data/estaciones_senamhi.csv')
#selrows <- c(1:dim(stn_senamhi)[1])

#selvars <- list('nivel', 'MAX', 'MIN', 'HR', 'PP')

#IR04 <- raster::raster("data/IR04_1704292345.tif")

#rand_lng <- function(n = 10) rnorm(n, -93.65, .01)
#rand_lat <- function(n = 10) rnorm(n, 42.0285, .01)

#stn_ls <- list('Tumbes','Iquitos','Talara','Piura','Yurimaguas','Chiclayo','Cajamarca',
#               'Juanjui','Trujillo','Pucallpa','Tingo Maria'='Tingo_Maria','Carhuaz',
#               'Huanuco','Puerto Maldonado'='Pto_Maldonado','Ayacucho','Cusco','Andahuaylas',
#               'Pisco','Nazca','Juliaca','Arequipa','Ilo','Tacna')
#stn_vars <- list('MAX', 'MIN', 'MED', 'PRCP', 'HR')



stn_ls <- list("EL PINTOR", "CUTERVO", "HUANUCO", "TINGO MARIA", "SAN RAMON", "JEPELACIO", "SORITOR", "NUEVO LIMA", "ALAO", "SAPOSOA", "LAMAS", "TABALOSOS", "PICOTA", "NARANJILLO", "CHAZUTA", "PUCALLPA - HUIMBAYOC", "EL PORVENIR", "SAUCE", "MOYOBAMBA", "JAMALCA") 

# Lista de imágenes sinópticas
sinls <- c(
  'Agua precipitable' = 'Agua_precipitable.png',
  'Presi\u00f3n reducida nivel del mar' = 'presionsuperficial.png',
  'L\u00edneas de corriente 200 hPa' = 'streamlines.png',
  'Divergencia 850 hPa' = 'div_850.png',
  'Divergencia 200 hPa' = 'div_200.png',
  'Velocidad vertical 800 hPa' = 'Vel_vert_800.png',
  'Velocidad vertical 500 hPa' = 'Vel_vert_500.png',
  'Velocidad vertical 300 hPa' = 'Vel_vert_300.png',
  '\u00cdndice CAPE' = 'cape.png',
  '\u00cdndice GDI' = 'GDI.png',
  '\u00cdndice K' = 'IndiceK.png'
)

library(readxl)
stn_metadata <- read_excel("base_estaciones/ACTUALIZANDO_pp_estaciones_Peru.xlsx", sheet = "Metadata_solo_Maranon")
stn_pp <- read_excel("base_estaciones/ACTUALIZANDO_pp_estaciones_Peru.xlsx", sheet = "pp_estaciones_Peru", skip=1, col_names = TRUE, col_types= c("date",rep("numeric",87)))

