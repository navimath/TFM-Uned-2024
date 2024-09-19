source(librerias.R)
source(extraccion.R)
source(transformacion.R)

# Función para insertar un pedido aceptado
insertar_pedido_aceptado <- function(con, pedido) {
  query <- sprintf("INSERT INTO PeticionesAceptadas (Nombre_cliente, Email, Telefono, DireccionRecogida, DireccionEntrega, Comentarios, Fragil) 
                    VALUES ('%s', '%s','%s', '%s','%s', '%s','%s')", 
                   pedido$Nombre_cliente, pedido$email, pedido$telefono, pedido$DireccionRecogida, pedido$Comentarios, pedido$Fragil)
  dbExecute(con, query)
}

insertar_pedido_emergente <- function(con, pedido) {
  query <- sprintf("UPDATE INTO PeticionesEmergentes (ID,Nombre_cliente, Email, Telefono, DireccionRecogida, DireccionEntrega, Comentarios, Fragil) 
                    VALUES ('%s','%s', '%s','%s', '%s','%s', '%s','%s')", 
                   pedido$ID, pedido$Nombre_cliente, pedido$email, pedido$telefono, pedido$DireccionRecogida, pedido$Comentarios, pedido$Fragil)
  dbExecute(con, query)
}

# Función para actualizar una petición emergente a aceptada
aceptar_pedidos_emergentes_bbdd <- function(con, pedido) {
  query <- sprintf("UPDATE INTO PeticionesEmergentes (ID,Estado) 
                    VALUES ('%s', 'Aceptado')", 
                   pedido$ID)
  dbExecute(con, query)
}

# LEY GDPR

registrar_acceso <- function(con, usuario, accion, tabla_afectada) {
  query <- sprintf("
    INSERT INTO acceso_auditoria (usuario, accion, tabla_afectada)
    VALUES ('%s', '%s', '%s')",
                   usuario, accion, tabla_afectada)
  
  dbExecute(con, query)
}

eliminar_registros_antiguos <- function(con) {
  # Consulta para pedidos aceptados
  query_pedidos_aceptados <- "
    DELETE FROM PeticionesAceptadas
    WHERE DATE(FechaHoraAceptado) < DATE_SUB(CURDATE(), INTERVAL 2 YEAR);
  "
  
  # Consulta para pedidos emergentes
  query_pedidos_emergentes <- "
    DELETE FROM PeticionesEmergentes
    WHERE DATE(FechaHoraAceptado) < DATE_SUB(CURDATE(), INTERVAL 2 YEAR);
  "
  
  # Ejecutar las consultas
  dbExecute(con, query_pedidos_aceptados)
  dbExecute(con, query_pedidos_emergentes)
}

