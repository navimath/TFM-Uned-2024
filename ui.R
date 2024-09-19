source(librerias.R)
source(extraccion.R)
source(transformacion.R)
source(carga.R)

source(extraccion.R)

shinyUI(fluidPage(
  useShinyjs(),
  titlePanel("Seguimiento de Pedidos y Furgonetas"),
  
  tabsetPanel(
    # Primera pestaña: Peticiones Emergentes y Mapa
    tabPanel("Peticiones Emergentes y Mapa",
             mainPanel(
               DTOutput("tabla_peticiones"),
               br(),
               leafletOutput("mapa_furgonetas", height = 500)
             )
    ),
    
    # Segunda pestaña: Pedidos Aceptados con filtro de fecha
    tabPanel("Pedidos Aceptados",
             sidebarPanel(
               dateInput("fecha_filtro", "Selecciona una fecha:", value = Sys.Date())
             ),
             mainPanel(
               DTOutput("tabla_pedidos")
             )
    )
  )
))
