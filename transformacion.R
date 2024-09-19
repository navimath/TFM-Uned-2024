source(librerias.R)
source(extraccion.R)

############# DIRECCION A COORDENADAS CON GOOGLE MAPS ##################

# Funcion para combinar el data frame de peticiones emergentes de Google Cloud con el data frame
# obtenido de leer los nuevos correos.

combinar_peticiones_emergentes <- function(con){
  
  peticiones_cloud <- obtener_peticiones_emergentes(con)
  peticiones_gmail <- obtener_peticiones_desde_gmail()
  
  peticiones_emergentes <- rbind(peticiones_cloud,peticiones_gmail)
  
  return(peticiones_emergentes)
}

# Funcion para convertir una direccion en coordenadas usando la API de Google Geocoding
direccion_a_coordenadas <- function(direccion, api_key) {
  
  # Crear la URL necesaria para la API de Google Geocoding
  url <- paste0(
    "https://maps.googleapis.com/maps/api/geocode/json?address=",
    URLencode(direccion), 
    "&key=", api_key
  )
  
  # Realizar la solicitud GET
  respuesta <- GET(url)
  
  # Verificar si la solicitud fue exitosa
  if (status_code(respuesta) != 200) {
    warning(paste("Error en la solicitud para la direccion:", direccion, "- Estado HTTP", status_code(respuesta)))
    return(c(latitud = NA, longitud = NA))
  }
  
  # Parsear el contenido JSON
  datos <- fromJSON(content(respuesta, as = "text"))
  
  # Verificar si la API devolvio resultados
  if (datos$status != "OK") {
    warning(paste("No se encontraron coordenadas para la direccion:", direccion))
    return(c(latitud = NA, longitud = NA))
  }
  
  # Extraer latitud y longitud del primer resultado
  coordenadas <- datos$results[[1]]$geometry$location
  latitud <- coordenadas$lat
  longitud <- coordenadas$lng
  
  return(c(latitud = latitud, longitud = longitud))
}

# Funcion para transformar direcciones de un data frame a coordenadas
transformar_direcciones_a_coordenadas <- function(con,api_key) {
  peticiones <- combinar_peticiones_emergentes(con)
  # Aplicar la funcion direccion_a_coordenadas a la columna 'DireccionRecogida'
  coordenadas <- apply(peticiones, 1, function(fila) {
    direccion_a_coordenadas(fila["DireccionRecogida"], api_key)
  })
  
  # Convertir el resultado en un data frame
  coordenadas_df <- do.call(rbind, coordenadas)
  
  # Combinar las coordenadas con el data frame original
  peticiones_con_coordenadas <- cbind(peticiones, coordenadas_df)
  
  return(peticiones_con_coordenadas)
}

############### DATA FRAME DE FURGONETAS COMPLETO ############################

# Funcion para obtener el data frame completo de las furgonetas
asignar_ubicacion_vehiculos <- function(con, api_key_traccar) {
  
  datos_furgonetas <- obtener_datos_furgonetas(con)
  coordenadas_furgonetas <- obtener_coordenadas_furgonetas_traccar(api_key_traccar) 
  vehiculos_con_ubicacion <- merge(datos_furgonetas, coordenadas_furgonetas, by = "ID")
  
  return(furgonetas_con_ubicacion)
}




