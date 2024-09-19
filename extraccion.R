source(librerias.R)
################# CARGA PETICIONES DESDE EMAIL #####################################

# Autenticar con Gmail usando las credenciales de OAuth2
use_secret_file("credentials.json")
gm_auth()

# Funcion para extraer los pedidos desde los los ultimos correos electronicos no leidos.
obtener_peticiones_desde_gmail <- function() {
  mensajes <- gm_messages("is:unread")
  pedidos <- list()
  
  for (mensaje in mensajes$messages) {
    # Obtener el contenido de cada correo
    email <- gm_message(mensaje$id)
    cuerpo <- gm_body(email, format = "full")
    
    # Usando expresiones regaluraes, extraemos los campos estructurados desde el cuerpo del correo
    NombreCliente <- sub(".*- Nombre :\\s*(.*)\\s*-.*", "\\1", cuerpo)
    correo <- sub(".*- correo :\\s*(.*)\\s*-.*", "\\1", cuerpo)
    telefono <- sub(".*- telefono :\\s*(.*)\\s*-.*", "\\1", cuerpo)
    direccion_recogida <- sub(".*- direccion recogida :\\s*(.*)\\s*-.*", "\\1", cuerpo)
    direccion_entrega <- sub(".*- direccion entrega :\\s*(.*)", "\\1", cuerpo)
    fragil <- sub(".*- fragil :\\s*(.*)\\s*-.*", "\\1", cuerpo)
    comentarios <- sub(".*- comentarios :\\s*(.*)\\s*-.*", "\\1", cuerpo)
    
    # Crear una lista de cada pedido y agregarla a la lista general
    pedidos[[length(pedidos) + 1]] <- list(
      ID = NA,
      NombreCliente = NombreCliente,
      correo = correo,
      telefono = telefono,
      direccion_recogida = direccion_recogida,
      direccion_entrega = direccion_entrega,
      fragil = fragil,
      comentarios = comentarios,
      estado = "No Aceptado",
      fecha_hora_peticion = Sys.time()
    )
  }
  
  # Convertir la lista de pedidos en un data frame
  pedidos_df <- bind_rows(pedidos)
  return(pedidos_df)
}


############# CARGA COORDENADAS FURGONETAS #############################



obtener_coordenadas_furgonetas <- function(base_url, token) {
  # Construir la URL de la API para obtener las posiciones.
  # base_url: La URL de tu servidor Traccar (por ejemplo, "http://localhost:8082")
  url <- paste0(base_url, "/api/positions")
  
  # Configurar el encabezado de autorizacion con el token
  headers <- add_headers(Authorization = paste("Bearer", token))
  
  # Realizar la solicitud GET a la API
  respuesta <- GET(url, headers)
  
  # Supondremos que la llamada es siempre exisota, de lo contario bastaria con implentar el siguiente codigo:
# 
#     if (status_code(respuesta) != 200) {
#      stop("Error al conectar con la API: estado HTTP ",
#           status_code(respuesta))
#    }
  
  # Parsear la respuesta JSON
  datos <- fromJSON(content(respuesta, "text"))
  
  # Extraer el ID del vehiculo y las coordenadas (latitud y longitud)
  info_vehiculos <- lapply(datos, function(vehiculo) {
    list(id = vehiculo$deviceId, 
         lat = vehiculo$latitude, 
         lon = vehiculo$longitude)
  })
  
  # Convertir la lista en un data frame
  coordfurgonetas_df <- do.call(rbind, lapply(info_vehiculos, as.data.frame))
  
  return(coordfurgonetas_df)
}

################## CONEXION A GOOGLE CLOUD SQL ##################################

# Cargar variables de entorno desde el archivo .env
load_dot_env()

# Funcion para establecer la conexion a la base de datos en Google Cloud SQL
conectar_base_datos <- function() {
  con <- dbConnect(
    RMySQL::MySQL(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    port = as.integer(Sys.getenv("DB_PORT")),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
  return(con)
}

# Funcion para obtener los datos de la tabla PeticionesEmergentes no aceptadas
obtener_peticiones_emergentes <- function(con) {
  query <- "SELECT * FROM PeticionesEmergentes WHERE ESTADO = 'No Aceptado'"
  data <- dbGetQuery(con, query)
  dbDisconnect(con)
  return(data)
}

# Funcion para obtener los datos de la tabla PeticionesAceptadas a partir de cierta fecha.
obtener_peticiones_aceptadas <- function(con,fecha) {
  # Usamos sprintf para formatear con formato C
  query <- sprintf("SELECT * FROM PeticionesAceptadas WHERE DATE(FechaHoraAceptado) >= '%s'", fecha)
  data <- dbGetQuery(con, query)
  dbDisconnect(con)
  return(data)
}

# Funcion para obtener los datos de la tabla Vehiculos
obtener_datos_furgonetas <- function(con) {
  query <- "SELECT * FROM Tabla_Vehiculos"
  data <- dbGetQuery(con, query)
  dbDisconnect(con)
  return(data)
}
