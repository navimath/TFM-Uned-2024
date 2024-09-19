source(librerias.R)
source(extraccion.R)
source(transformacion.R)
source(carga.R)

api_key_traccar <- "api_traccar"
apli_key_gmaps <- "api_gmaps"

shinyServer(function(input, output, session) {
  
  con <- conectar_base_datos()
  
  # Reactive value
  peticiones_emergentes <- reactiveVal(data.frame())  
  pedidos_aceptados <- reactiveVal(data.frame())
  furgonetas <- reactiveVal(data.frame())
  
  # Mostrar la tabla de peticiones emergentes
  output$tabla_peticiones <- renderDT({
    df <- peticiones_emergentes()[Estado == 'No Aceptado']
    datatable(df[, c(longitud,latitud):=NULL], selection = 'single', escape = FALSE, 
              options = list(scrollY = 200, paging = FALSE))
  })
  
  # Actualizar pedidos aceptados al cambiar la fecha
  observeEvent(input$fecha_filtro, {
    pedidos_aceptados(pedidos_aceptados()[, c(longitud,latitud):=NULL])
  })
  
  # Mostrar la tabla de pedidos aceptados
  output$tabla_pedidos <- renderDT({
    datatable(pedidos_aceptados()[, c(ID,Longitud,Latitud):=NULL], selection = 'single', escape = FALSE, 
              options = list(scrollY = 200, paging = FALSE))
  })
  
  # Aceptar una petición emergente
  observeEvent(input$tabla_peticiones_rows_selected, {
    fila <- input$tabla_peticiones_rows_selected
    if (length(fila) > 0) {
      df <- peticiones_emergentes()
      pedidos_aceptados((rbind(pedidos_aceptados(), df[fila, ])))
      df[fila,"Estado"] <- "Aceptado"
      peticiones_emergentes(df)
    }
  })
  
  # Mapa interactivo
  output$mapa_furgonetas <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = peticiones_emergentes(), lat = ~latitud, lng = ~longitud, 
                 popup = ~paste("Cliente:", Nombre_cliente, "<br>",
                                "Correo:", email, "<br>",
                                "Telefono", telefono, "<br>",
                                "Dirección de Entrega:", DireccionEntrega, "<br>",
                                "Fragil:", Fragil, "<br>",
                                "Comentarios:", Comentarios),
                 icon = icons(iconUrl = "icono_pedido.png", iconWidth = 25, iconHeight = 41)) %>%
      addMarkers(data = furgonetas, lat = ~Latitud, lng = ~Longitud, 
                 popup = ~paste("Furgoneta:", nombre, "<br>",
                                "Matricula:", matricula, "<br>",
                                "Telefono:", conductor),
                 icon = icons(iconUrl = "icono_furgoneta.png", iconWidth = 25, iconHeight = 41))
  })
  
  actualizar_mapa <- function() {
    leafletProxy("mapa_furgonetas") %>%
      clearMarkers() %>%
      addMarkers(data = peticiones_emergentes(), lat = ~latitud, lng = ~longitud, 
                  popup = ~paste("Cliente:", Nombre_cliente, "<br>",
                                 "Correo:", email, "<br>",
                                 "Telefono", telefono, "<br>",
                                 "Dirección de Entrega:", DireccionEntrega, "<br>",
                                 "Fragil:", Fragil, "<br>",
                                 "Comentarios:", Comentarios),
                 icon = icons(iconUrl = "icono_pedido.png", iconWidth = 25, iconHeight = 41)) %>%
                   addMarkers(data = furgonetas, lat = ~Latitud, lng = ~Longitud, 
                              popup = ~paste("Furgoneta:", nombre, "<br>",
                                             "Matricula:", matricula, "<br>",
                                             "Telefono:", conductor),,
                              icon = icons(iconUrl = "icono_furgoneta.png", iconWidth = 25, iconHeight = 41))
  }
    
  
  # Cargar pedidos aceptados al iniciar y cada minuto
  observeEvent(reactiveTimer(60000)(),{
    pedidos_aceptados(obtener_peticiones_aceptadas(con, input$fecha_filtro))
    registrar_acceso(con, session$user, "Obtener pet. aceptadas", "PeticionesAceptadas")
    peticiones_emergentes(transformar_direcciones_a_coordenadas(con, api_key_gmaps))
    registrar_acceso(con, session$user, "Obtener pet. emergentes", "PeticionesEmergentes")
    furgonetas(asignar_ubicacion_vehiculos(con,api_key_traccar))
  },ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # Escritura en la base de datos cada hora
  observeEvent(reactiveTimer(3600000)(),{
    lapply(peticiones_acpetadas(),~insertar_pedido_aceptado(con,.x))
    registrar_acceso(con, session$user, "Escribir pet. aceptadas", "PeticionesAceptadas")
    lapply(peticiones_emergentes(),~inster_pedido_emergente(con,.x))
    registrar_acceso(con, session$user, "Escribir pet. emergentes", "PeticionesEmergentes")
    lapply(peticiones_emergentes(),~aceptar_pedidos_emergentes_bbdd(con,.x))
  })
  
  session$onSessionEnded(function() {
    eliminar_registros_antiguos(con)
    registrar_acceso(con, session$user, "Eliminar pet. aceptadas de hace dos años", "PeticionesAceptadas")
    registrar_acceso(con, session$user, "Eliminar pet. emergentes de hace dos años", "PeticionesEmergentes")
    dbDisconnect(con)
  })
})
