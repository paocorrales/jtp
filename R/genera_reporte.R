#' Genera reporte del estado de un paquete
#'
#' Guardará un archivo `.md` con el restultado de [chequea_paquete()]
#'
#' @param checks lista con el resultado de [chequea_paquete()]
#' @param nombre_archivo por defecto usa nombrepaquete_feedback.md
#' @param ruta_directorio ruta a la carpeta donde se guardará el reporte. Por defecto
#' se guarda en el directorio de trabajo actual.
#'
#' @returns el path al archivo creado
#' @export
#'
#' @examples
#' \dontrun{
#' genera_reporte(checks)
#' }
genera_reporte <- function(checks, nombre_archivo = NULL, ruta_directorio = ".") {

  if (is.null(nombre_archivo)) {
    nombre_archivo <- paste0(checks$description$nombre_paquete, "_feedback.md")
  }

  archivo <- file.path(ruta_directorio, nombre_archivo)

  utils::capture.output(print(checks)) |>
    writeLines(archivo)

  return(archivo)

}
